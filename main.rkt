#lang racket/base
;; main.rkt — command-line front end.
;;
;;   racket main.rkt scan ~/Dropbox            walk folders, update the catalog
;;   racket main.rkt summary                   totals by content type
;;   racket main.rkt list --content speech     one line per file
;;   racket main.rkt show FILE                 timeline of one file
;;   racket main.rkt dupes                     byte-identical files
;;   racket main.rkt export catalog.csv        everything, for a spreadsheet
;;
;; The catalog is a SQLite file, by default ~/.audio-catalog/catalog.sqlite
;; (deliberately outside Dropbox: syncing a live SQLite file is asking for
;; trouble).  Override with --db FILE on any command.

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string
         file/sha1
         db
         "decode.rkt"
         "classify.rkt"
         "db.rkt")

(provide analyze-file online-only? format-duration format-size)

(define default-db
  (build-path (find-system-path 'home-dir) ".audio-catalog" "catalog.sqlite"))

(define db-path (make-parameter default-db))

(define (open-db)
  (define p (db-path))
  (make-directory* (or (path-only (path->complete-path p)) (current-directory)))
  (open-catalog p))

;; ---------------------------------------------------------------- helpers

(define (format-duration secs)
  (cond
    [(not secs) "   -   "]
    [else
     (define s (inexact->exact (round secs)))
     (define (two n) (~r n #:min-width 2 #:pad-string "0"))
     (format "~a:~a:~a" (quotient s 3600) (two (quotient (remainder s 3600) 60))
             (two (remainder s 60)))]))

(define (format-size bytes)
  (cond
    [(>= bytes (expt 1024 3)) (format "~a GB" (~r (/ bytes (expt 1024.0 3)) #:precision '(= 2)))]
    [(>= bytes (expt 1024 2)) (format "~a MB" (~r (/ bytes (expt 1024.0 2)) #:precision '(= 1)))]
    [else (format "~a kB" (~r (/ bytes 1024.0) #:precision 0))]))

;; A Dropbox "online-only" file has a size but occupies no blocks on disk.
;; Reading it would make Dropbox download it, so scan leaves these alone
;; unless told otherwise.
(define (online-only? path)
  (define st (file-or-directory-stat path))
  (and (> (hash-ref st 'size) 0)
       (zero? (hash-ref st 'block-count 1))))

(define (file-sha1 path)
  (call-with-input-file path (lambda (in) (bytes->hex-string (sha1-bytes in)))))

;; Depth-first list of audio files below root, skipping hidden folders
;; (.dropbox.cache and friends) and folders we may not read.
(define (audio-files-under root)
  (define (visible-dir? p)
    (define-values (base name must-be-dir?) (split-path p))
    (not (and (path? name) (regexp-match? #rx"^[.]" (path->utf8 name)))))
  (for/list ([p (in-directory root visible-dir?)]
             #:when (and (audio-file? p)
                         (not (regexp-match? #rx"^[.]" (path->utf8 (file-name-from-path p))))
                         (file-exists? p)))
    p))

;; ---------------------------------------------------------------- scan

;; Probe, hash and analyse one file.  -> file-record.  Never raises for
;; problems with the file itself; they end up in the record's error field.
(define (analyze-file path
                      #:hash? [hash? #t]
                      #:analyze? [analyze? #t]
                      #:hydrate? [hydrate? #f])
  (define str (path->utf8 path))
  (define size (file-size path))
  (define mtime (file-or-directory-modify-seconds path))
  (define (record status #:error [err #f] #:sha1 [sha #f] #:info [info #f]
                  #:content [content #f] #:seconds [seconds #f] #:segments [segments '()])
    (file-record str size mtime status err sha
                 (and info (media-info-format info)) (and info (media-info-codec info))
                 (and info (media-info-sample-rate info)) (and info (media-info-channels info))
                 (and info (media-info-bit-rate info)) (and info (media-info-duration info))
                 (and info (media-info-title info)) (and info (media-info-artist info))
                 (and info (media-info-album info)) (and info (media-info-date info))
                 content seconds segments))
  (cond
    [(and (not hydrate?) (online-only? path)) (record "online-only")]
    [else
     (with-handlers ([exn:fail? (lambda (e) (record "error" #:error (exn-message e)))])
       (define info (probe path))
       (define sha (and hash? (file-sha1 path)))
       (cond
         [analyze?
          (define blocks (call-with-pcm-port path read-block-features))
          (define segments
            (labels->segments (smooth-labels (map classify-block blocks))
                              (media-info-duration info)))
          (define seconds (label-seconds segments))
          (record "ok" #:sha1 sha #:info info #:segments segments #:seconds seconds
                  #:content (primary-label seconds))]
         [else (record "ok" #:sha1 sha #:info info)]))]))

(define (scan roots #:force? force? #:hash? hash? #:analyze? analyze?
              #:hydrate? hydrate? #:prune? prune?)
  (unless (tools-available?)
    (raise-user-error 'scan "ffmpeg and ffprobe are required (brew install ffmpeg)"))
  (define conn (open-db))
  (for ([root roots])
    (define root-path (simplify-path (path->complete-path (expand-user-path root))))
    (unless (directory-exists? root-path)
      (raise-user-error 'scan "not a directory: ~a" root))
    (eprintf "Looking for audio files under ~a ...\n" (path->utf8 root-path))
    (define files (audio-files-under root-path))
    (define total (length files))
    (eprintf "~a audio files found.\n" total)
    (define counts
      (for/fold ([counts (hash)]) ([path files] [i (in-naturals 1)])
        (define str (path->utf8 path))
        (define known (known-file-state conn str))
        (define unchanged?
          (and known
               (not force?)
               (= (vector-ref known 0) (file-size path))
               (= (vector-ref known 1) (file-or-directory-modify-seconds path))
               (match (vector-ref known 2)
                 ;; retry placeholders once we are allowed to (or they got downloaded)
                 ["online-only" (and (not hydrate?) (online-only? path))]
                 ["error" #t]
                 [_ (or (not analyze?) (equal? (vector-ref known 3) analyzer-version))])))
        (cond
          [unchanged? (hash-update counts "unchanged" add1 0)]
          [else
           (define r (analyze-file path #:hash? hash? #:analyze? analyze? #:hydrate? hydrate?))
           (store-file! conn r)
           (eprintf "[~a/~a] ~a ~a  ~a\n" i total
                    (~a (or (file-record-content r) (file-record-status r)) #:min-width 11)
                    (format-duration (file-record-duration r))
                    (path->utf8 (find-relative-path root-path path)))
           (hash-update counts (file-record-status r) add1 0)])))
    (define seen (for/hash ([p files]) (values (path->utf8 p) #t)))
    (define removed (if prune? (forget-missing! conn (path->utf8 root-path) seen) 0))
    (eprintf "Done: ~a\n"
             (string-join
              (append (for/list ([(k v) (in-hash counts)]) (format "~a ~a" v k))
                      (if prune? (list (format "~a removed from catalog" removed)) '()))
              ", ")))
  (disconnect conn))

;; ---------------------------------------------------------------- reports

(define (summary)
  (define conn (open-db))
  (match-define (vector files bytes seconds online errors) (catalog-totals conn))
  (printf "~a files, ~a, ~a of audio\n" files (format-size bytes) (format-duration seconds))
  (unless (zero? online)
    (printf "~a online-only files not read (scan with --hydrate to include them)\n" online))
  (unless (zero? errors)
    (printf "~a files could not be read (list --content error)\n" errors))
  (printf "\n~a ~a ~a ~a\n" (~a "content" #:min-width 16) (~a "files" #:min-width 7 #:align 'right)
          (~a "size" #:min-width 11 #:align 'right) (~a "duration" #:min-width 11 #:align 'right))
  (for ([row (label-totals conn)])
    (match-define (vector content n size dur) row)
    (printf "~a ~a ~a ~a\n" (~a content #:min-width 16) (~a n #:min-width 7 #:align 'right)
            (~a (format-size size) #:min-width 11 #:align 'right)
            (~a (format-duration dur) #:min-width 11 #:align 'right)))
  (disconnect conn))

(define (percent part whole)
  (if (and part whole (> whole 0)) (~a (~r (* 100.0 (/ part whole)) #:precision 0 #:min-width 3) "%") "   -"))

(define (list-files content under)
  (define conn (open-db))
  (define under* (and under (path->utf8 (simplify-path (path->complete-path (expand-user-path under))))))
  (define rows
    (if (member content '("error" "online-only"))
        (filter (lambda (r) (equal? (vector-ref r 8) content)) (query-files conn #:under under*))
        (query-files conn #:content content #:under under*)))
  (printf "content   duration      size  speech music noise  path\n")
  (for ([r rows])
    (match-define (vector path dur size label speech music noise silence status) r)
    (define sounding (and speech (+ speech music noise)))
    (printf "~a ~a ~a   ~a  ~a  ~a  ~a\n"
            (~a (or label status) #:min-width 8)
            (~a (format-duration dur) #:min-width 9 #:align 'right)
            (~a (format-size size) #:min-width 9 #:align 'right)
            (percent speech sounding) (percent music sounding) (percent noise sounding)
            path))
  (disconnect conn))

(define (show file)
  (define conn (open-db))
  (define path (path->utf8 (simplify-path (path->complete-path (expand-user-path file)))))
  (define segments (file-segments conn path))
  (cond
    [(not segments) (printf "Not in the catalog: ~a\n" path)]
    [(null? segments) (printf "No content analysis stored for ~a\n" path)]
    [else
     (for ([s segments])
       (printf "~a - ~a  ~a\n" (format-duration (segment-start s))
               (format-duration (segment-end s)) (segment-label s)))])
  (disconnect conn))

(define (dupes)
  (define conn (open-db))
  (define groups (duplicate-groups conn))
  (if (null? groups)
      (printf "No byte-identical files in the catalog.\n")
      (for ([g groups])
        (for ([p g]) (printf "~a\n" p))
        (newline)))
  (disconnect conn))

(define (csv-field v)
  (define s (cond [(not v) ""] [(string? v) v] [else (~a v)]))
  (if (regexp-match? #rx"[\",\n\r]" s)
      (string-append "\"" (string-replace s "\"" "\"\"") "\"")
      s))

(define (export out-file)
  (define conn (open-db))
  (call-with-output-file out-file #:exists 'truncate
    (lambda (out)
      (displayln (string-join export-columns ",") out)
      (for ([row (all-rows conn)])
        (displayln (string-join (map csv-field (vector->list row)) ",") out))))
  (printf "Wrote ~a\n" out-file)
  (disconnect conn))

;; ---------------------------------------------------------------- CLI

(define usage
  "usage: racket main.rkt COMMAND [options]

  scan DIR ...    catalog the audio files under each DIR
      --force       re-examine files even if size and date are unchanged
      --hydrate     also read Dropbox online-only files (downloads them!)
      --no-analyze  metadata only, skip speech/music/noise detection
      --no-hash     skip SHA-1 (no duplicate detection)
      --prune       drop catalog entries under DIR whose file is gone
  summary         totals by content type
  list            one line per file
      --content X   only speech | music | noise | silence | mixed | error | online-only
      --under DIR   only files below DIR
  show FILE       timeline of speech/music/noise within one file
  dupes           groups of byte-identical files
  export FILE.csv write the whole catalog as CSV

  Every command accepts --db FILE (default ~/.audio-catalog/catalog.sqlite).")

(module+ main
  (define argv (current-command-line-arguments))
  (define command (if (zero? (vector-length argv)) "help" (vector-ref argv 0)))
  (define rest (if (zero? (vector-length argv)) (vector) (vector-drop* argv 1)))
  (define (parse #:flags [flags '()] #:handler handler #:args arg-names)
    (parse-command-line
     (string-append "main.rkt " command) rest
     `((once-each
        [("--db") ,(lambda (flag file) (db-path (string->path file))) ("catalog file" "FILE")]
        ,@flags))
     handler arg-names))
  (match command
    ["scan"
     (define force? #f) (define hydrate? #f) (define analyze? #t)
     (define hash? #t) (define prune? #f)
     (parse #:flags `([("--force") ,(lambda (f) (set! force? #t)) ("re-examine unchanged files")]
                      [("--hydrate") ,(lambda (f) (set! hydrate? #t)) ("read online-only files")]
                      [("--no-analyze") ,(lambda (f) (set! analyze? #f)) ("metadata only")]
                      [("--no-hash") ,(lambda (f) (set! hash? #f)) ("skip SHA-1")]
                      [("--prune") ,(lambda (f) (set! prune? #t)) ("drop entries of deleted files")])
            #:handler (lambda (accum dir . dirs)
                        (scan (cons dir dirs) #:force? force? #:hash? hash? #:analyze? analyze?
                              #:hydrate? hydrate? #:prune? prune?))
            #:args '("DIR" "DIR"))]
    ["summary" (parse #:handler (lambda (accum) (summary)) #:args '())]
    ["list"
     (define content #f) (define under #f)
     (parse #:flags `([("--content") ,(lambda (f x) (set! content x)) ("filter by content" "X")]
                      [("--under") ,(lambda (f d) (set! under d)) ("filter by folder" "DIR")])
            #:handler (lambda (accum) (list-files content under)) #:args '())]
    ["show" (parse #:handler (lambda (accum file) (show file)) #:args '("FILE"))]
    ["dupes" (parse #:handler (lambda (accum) (dupes)) #:args '())]
    ["export" (parse #:handler (lambda (accum file) (export file)) #:args '("FILE.csv"))]
    [_ (displayln usage)]))

(define (vector-drop* v n)
  (for/vector ([x (in-vector v n)]) x))
