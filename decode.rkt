#lang racket/base
;; decode.rkt — talk to ffprobe/ffmpeg: container metadata and PCM decoding.

(require racket/port
         racket/path
         racket/string
         racket/flonum
         json
         "features.rkt")

(provide (struct-out media-info)
         tools-available?
         probe
         call-with-pcm-port
         read-block-features
         audio-extensions
         path->utf8
         audio-file?)

(define audio-extensions
  '("mp3" "m4a" "m4b" "aac" "wav" "aif" "aiff" "aifc" "caf" "flac" "ogg" "oga"
    "opus" "wma" "amr" "3gp" "mka" "ape" "wv"))

(define (audio-file? path)
  (define ext (path-get-extension path))
  (and ext
       (member (string-downcase (substring (bytes->string/utf-8 ext #\?) 1))
               audio-extensions)
       #t))

(define (find-tool name)
  (or (find-executable-path name)
      (for/or ([dir '("/opt/homebrew/bin" "/usr/local/bin")])
        (define p (build-path dir name))
        (and (file-exists? p) p))))

(define (tools-available?)
  (and (find-tool "ffmpeg") (find-tool "ffprobe") #t))

(struct media-info (format codec sample-rate channels bit-rate duration
                    title artist album date)
  #:transparent)

;; path -> media-info, or raises exn:fail if ffprobe cannot read the file.
(define (probe path)
  (define-values (proc out in err)
    (subprocess #f #f #f (find-tool "ffprobe")
                "-v" "error" "-print_format" "json"
                "-show_format" "-show_streams" "-select_streams" "a:0"
                (path-arg path)))
  (close-output-port in)
  (define err-text (box ""))
  (define err-thread (thread (lambda () (set-box! err-text (port->string err)))))
  (define js (with-handlers ([exn:fail? (lambda (e) eof)]) (read-json out)))
  (subprocess-wait proc)
  (thread-wait err-thread)
  (close-input-port out)
  (close-input-port err)
  (unless (and (hash? js) (zero? (subprocess-status proc)))
    (error 'probe "ffprobe failed: ~a" (string-trim (unbox err-text))))
  (define fmt (hash-ref js 'format (hash)))
  (define streams (hash-ref js 'streams '()))
  (when (null? streams)
    (error 'probe "no audio stream"))
  (define st (car streams))
  ;; tag keys vary in case between containers
  (define tags
    (for/hash ([(k v) (in-hash (hash-ref fmt 'tags (hash)))])
      (values (string-downcase (symbol->string k)) v)))
  (define (num v) (and (string? v) (string->number v)))
  (media-info (hash-ref fmt 'format_name #f)
              (hash-ref st 'codec_name #f)
              (num (hash-ref st 'sample_rate #f))
              (hash-ref st 'channels #f)
              (or (num (hash-ref st 'bit_rate #f)) (num (hash-ref fmt 'bit_rate #f)))
              (let ([d (or (num (hash-ref fmt 'duration #f))
                           (num (hash-ref st 'duration #f)))])
                (and d (exact->inexact d)))
              (hash-ref tags "title" #f)
              (hash-ref tags "artist" #f)
              (hash-ref tags "album" #f)
              (hash-ref tags "date" #f)))

;; Paths are stored and printed as UTF-8 regardless of the current locale
;; (path->string depends on LANG, which breaks umlauts under cron/launchd).
(define (path->utf8 p)
  (if (path? p) (bytes->string/utf-8 (path->bytes p) #\uFFFD) p))

;; Subprocess arguments: hand paths over as raw bytes, never re-encoded.
(define (path-arg p)
  (if (path? p) p (bytes->path (string->bytes/utf-8 p))))

;; Runs ffmpeg decoding `path` to 16 kHz mono signed 16-bit little-endian PCM
;; and calls (proc pcm-input-port).  Returns proc's result.
(define (call-with-pcm-port path proc)
  (define-values (sub out in err)
    (subprocess #f #f #f (find-tool "ffmpeg")
                "-v" "error" "-nostdin" "-i" (path-arg path)
                "-map" "0:a:0" "-ac" "1" "-ar" (number->string sample-rate)
                "-f" "s16le" "-"))
  (close-output-port in)
  (define err-thread (thread (lambda () (copy-port err (open-output-nowhere)))))
  (dynamic-wind
   void
   (lambda () (proc out))
   (lambda ()
     (close-input-port out)
     (subprocess-kill sub #t)
     (subprocess-wait sub)
     (thread-wait err-thread)
     (close-input-port err))))

;; Reads the whole PCM stream, returning the list of block-feat (one per
;; ~2 s; a trailing partial block is kept if it is at least a quarter full).
(define (read-block-features in)
  (define analyze (make-frame-analyzer))
  (define window (make-flvector frame-size 0.0))
  (define buf (make-bytes (* 2 hop-size)))
  (define min-tail (quotient block-frames 4))
  (let loop ([frames '()] [n-frames 0] [blocks '()])
    (define got (read-bytes! buf in))
    (cond
      [(or (eof-object? got) (< got (* 2 hop-size)))
       (reverse (if (>= n-frames min-tail)
                    (cons (summarize-block (reverse frames)) blocks)
                    blocks))]
      [else
       ;; slide the window by one hop and append the new samples
       (for ([i (in-range hop-size)])
         (flvector-set! window i (flvector-ref window (+ i hop-size))))
       (for ([i (in-range hop-size)])
         (flvector-set! window (+ hop-size i)
                        (fl/ (->fl (integer-bytes->integer buf #t #f (* 2 i) (+ 2 (* 2 i))))
                             32768.0)))
       (define frames* (cons (analyze window) frames))
       (if (= (add1 n-frames) block-frames)
           (loop '() 0 (cons (summarize-block (reverse frames*)) blocks))
           (loop frames* (add1 n-frames) blocks))])))
