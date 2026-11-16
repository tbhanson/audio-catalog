#lang racket/base
;; db.rkt — the SQLite catalog.
;;
;; Two tables:
;;   files     one row per audio file (identity, container metadata, content
;;             summary)
;;   segments  time ranges within a file with a content label; this is the
;;             table later stages (e.g. speaker identification) can build on
;;             — they only need to look at the 'speech' rows.

(require db
         racket/list
         "classify.rkt")

(provide analyzer-version
         open-catalog
         (struct-out file-record)
         known-file-state
         store-file!
         forget-missing!
         catalog-totals
         label-totals
         query-files
         file-segments
         duplicate-groups
         all-rows
         export-columns)

;; Bump when features.rkt/classify.rkt change in a way that should cause
;; already-catalogued files to be re-analysed on the next scan.
(define analyzer-version 1)

(define schema
  '("CREATE TABLE IF NOT EXISTS files (
       id            INTEGER PRIMARY KEY,
       path          TEXT NOT NULL UNIQUE,
       size_bytes    INTEGER NOT NULL,
       mtime         INTEGER NOT NULL,
       status        TEXT NOT NULL,      -- ok | online-only | error
       error         TEXT,
       sha1          TEXT,
       format        TEXT,
       codec         TEXT,
       sample_rate   INTEGER,
       channels      INTEGER,
       bit_rate      INTEGER,
       duration_s    REAL,
       title         TEXT,
       artist        TEXT,
       album         TEXT,
       tag_date      TEXT,
       content       TEXT,               -- speech | music | noise | silence | mixed
       speech_s      REAL,
       music_s       REAL,
       noise_s       REAL,
       silence_s     REAL,
       analyzer      INTEGER,            -- analyzer-version, NULL if not analysed
       scanned_at    INTEGER NOT NULL)"
    "CREATE TABLE IF NOT EXISTS segments (
       file_id  INTEGER NOT NULL REFERENCES files(id) ON DELETE CASCADE,
       start_s  REAL NOT NULL,
       end_s    REAL NOT NULL,
       label    TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS segments_file ON segments(file_id)"
    "CREATE INDEX IF NOT EXISTS files_sha1 ON files(sha1)"))

;; path or 'memory -> connection
(define (open-catalog where)
  (define conn (sqlite3-connect #:database where #:mode 'create))
  (query-exec conn "PRAGMA foreign_keys = ON")
  (for ([stmt schema]) (query-exec conn stmt))
  conn)

;; Everything we know about one file.  `seconds` is a hash label -> seconds
;; or #f when the file was not analysed; `segments` a list of segment.
(struct file-record (path size mtime status error sha1
                     format codec sample-rate channels bit-rate duration
                     title artist album tag-date
                     content seconds segments)
  #:transparent)

(define (nullable v) (if v v sql-null))
(define (unnull v) (if (sql-null? v) #f v))

;; -> #f if unknown, else (vector size mtime status analyzer-or-#f)
(define (known-file-state conn path)
  (define row (query-maybe-row conn
                               "SELECT size_bytes, mtime, status, analyzer FROM files WHERE path = ?"
                               path))
  (and row (vector-map* unnull row)))

(define (vector-map* f v) (for/vector ([x v]) (f x)))

(define (store-file! conn r)
  (define secs (file-record-seconds r))
  (define (sec label) (if secs (hash-ref secs label 0.0) sql-null))
  (call-with-transaction
   conn
   (lambda ()
     (query-exec conn "DELETE FROM files WHERE path = ?" (file-record-path r))
     (query-exec
      conn
      "INSERT INTO files (path, size_bytes, mtime, status, error, sha1, format, codec,
                          sample_rate, channels, bit_rate, duration_s, title, artist,
                          album, tag_date, content, speech_s, music_s, noise_s,
                          silence_s, analyzer, scanned_at)
       VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      (file-record-path r) (file-record-size r) (file-record-mtime r)
      (file-record-status r) (nullable (file-record-error r))
      (nullable (file-record-sha1 r)) (nullable (file-record-format r))
      (nullable (file-record-codec r)) (nullable (file-record-sample-rate r))
      (nullable (file-record-channels r)) (nullable (file-record-bit-rate r))
      (nullable (file-record-duration r)) (nullable (file-record-title r))
      (nullable (file-record-artist r)) (nullable (file-record-album r))
      (nullable (file-record-tag-date r))
      (nullable (and (file-record-content r) (symbol->string (file-record-content r))))
      (sec 'speech) (sec 'music) (sec 'noise) (sec 'silence)
      (if secs analyzer-version sql-null)
      (current-seconds))
     (define id (query-value conn "SELECT id FROM files WHERE path = ?" (file-record-path r)))
     (for ([s (file-record-segments r)])
       (query-exec conn "INSERT INTO segments (file_id, start_s, end_s, label) VALUES (?,?,?,?)"
                   id (exact->inexact (segment-start s)) (exact->inexact (segment-end s))
                   (symbol->string (segment-label s)))))))

;; Drop rows under `root` whose path is not in `seen` (a set-like hash).
;; Returns the number of rows removed.
(define (forget-missing! conn root seen)
  (define prefix (let ([s root]) (if (regexp-match? #rx"/$" s) s (string-append s "/"))))
  (define gone
    (for/list ([p (query-list conn "SELECT path FROM files")]
               #:when (and (>= (string-length p) (string-length prefix))
                           (string=? prefix (substring p 0 (string-length prefix)))
                           (not (hash-ref seen p #f))))
      p))
  (call-with-transaction
   conn
   (lambda ()
     (for ([p gone]) (query-exec conn "DELETE FROM files WHERE path = ?" p))))
  (length gone))

;; -> (vector files bytes seconds online-only errors)
(define (catalog-totals conn)
  (vector-map*
   unnull
   (query-row conn
              "SELECT COUNT(*), COALESCE(SUM(size_bytes),0), COALESCE(SUM(duration_s),0.0),
                      COALESCE(SUM(status = 'online-only'),0), COALESCE(SUM(status = 'error'),0)
               FROM files")))

;; -> list of (vector content files bytes seconds), biggest duration first
(define (label-totals conn)
  (query-rows conn
              "SELECT COALESCE(content, '(not analysed)'), COUNT(*), SUM(size_bytes),
                      COALESCE(SUM(duration_s), 0.0)
               FROM files GROUP BY 1 ORDER BY 4 DESC"))

;; macOS hands out file names in either Unicode normal form depending on
;; where they came from, so user-supplied paths are compared NFC-to-NFC.
(define (same-path? a b)
  (string=? (string-normalize-nfc a) (string-normalize-nfc b)))

(define (path-under? path dir)
  (define p (string-normalize-nfc path))
  (define d (string-normalize-nfc dir))
  (and (>= (string-length p) (string-length d))
       (string=? d (substring p 0 (string-length d)))))

;; -> list of (vector path duration size content speech music noise silence status)
(define (query-files conn #:content [content #f] #:under [under #f])
  (define rows
    (query-rows conn
                "SELECT path, duration_s, size_bytes, content, speech_s, music_s, noise_s,
                        silence_s, status
                 FROM files ORDER BY path"))
  (for/list ([r rows]
             #:when (or (not content) (equal? (vector-ref r 3) content))
             #:when (or (not under) (path-under? (vector-ref r 0) under)))
    (vector-map* unnull r)))

;; -> list of segment, or #f if the path is not in the catalog
(define (file-segments conn path)
  (define id
    (or (query-maybe-value conn "SELECT id FROM files WHERE path = ?" path)
        (for/first ([(id p) (in-query conn "SELECT id, path FROM files")]
                    #:when (same-path? p path))
          id)))
  (and id
       (for/list ([(start end label)
                   (in-query conn
                             "SELECT start_s, end_s, label FROM segments
                              WHERE file_id = ? ORDER BY start_s" id)])
         (segment start end (string->symbol label)))))

;; -> list of (list path ...) for files with byte-identical content
(define (duplicate-groups conn)
  (define rows
    (query-rows conn
                "SELECT sha1, path FROM files
                 WHERE sha1 IN (SELECT sha1 FROM files WHERE sha1 IS NOT NULL
                                GROUP BY sha1 HAVING COUNT(*) > 1)
                 ORDER BY sha1, path"))
  (map (lambda (group) (map (lambda (r) (vector-ref r 1)) group))
       (group-by (lambda (r) (vector-ref r 0)) rows)))

(define export-columns
  '("path" "size_bytes" "mtime" "status" "error" "sha1" "format" "codec" "sample_rate"
    "channels" "bit_rate" "duration_s" "title" "artist" "album" "tag_date" "content"
    "speech_s" "music_s" "noise_s" "silence_s"))

(define (all-rows conn)
  (for/list ([r (query-rows conn
                            (string-append "SELECT "
                                           (apply string-append (add-between export-columns ", "))
                                           " FROM files ORDER BY path"))])
    (vector-map* unnull r)))
