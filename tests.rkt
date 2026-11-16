#lang racket/base
;; tests.rkt — run with:  raco test tests.rkt
;; None of these need ffmpeg: signals are synthesised and fed to the
;; analyser as in-memory PCM, and the database lives in memory.

(require rackunit
         racket/flonum
         racket/math
         racket/list
         "features.rkt"
         "decode.rkt"
         "classify.rkt"
         "db.rkt"
         "main.rkt")

;; (sample-index -> amplitude in -1..1), seconds -> input port of s16le PCM
(define (pcm-port gen seconds)
  (define n (* sample-rate seconds))
  (define bs (make-bytes (* 2 n)))
  (for ([i (in-range n)])
    (define v (max -32768 (min 32767 (exact-round (* 32767.0 (gen i))))))
    (integer->integer-bytes v 2 #t #f bs (* 2 i)))
  (open-input-bytes bs))

(define (block-labels gen seconds)
  (map classify-block (read-block-features (pcm-port gen seconds))))

(define (tone freq) (lambda (i) (sin (/ (* 2.0 pi freq i) sample-rate))))

;; a held major chord, each note with a few harmonics
(define (chord i)
  (* 0.08
     (for*/sum ([f '(220.0 277.18 329.63)] [h '(1 2 3 4)])
       (/ ((tone (* f h)) i) h))))

(test-case "fft finds a sine in the right bin"
  (define n 64)
  (define re (for/flvector #:length n ([i n]) (exact->inexact (cos (/ (* 2.0 pi 5 i) n)))))
  (define im (make-flvector n 0.0))
  (fft! re im)
  (define mags (for/list ([k n]) (sqrt (+ (sqr (flvector-ref re k)) (sqr (flvector-ref im k))))))
  (check-equal? (argmax (lambda (k) (list-ref mags k)) (range (quotient n 2))) 5)
  (check-= (list-ref mags 5) 32.0 1e-9)
  (check-= (list-ref mags 6) 0.0 1e-9))

(test-case "block length matches the constants"
  (check-= block-seconds 2.048 1e-9)
  (check-equal? (length (read-block-features (pcm-port (tone 440.0) 10))) 5))

(test-case "digital silence is silence"
  (check-equal? (remove-duplicates (block-labels (lambda (i) 0.0) 6)) '(silence)))

(test-case "white noise is noise"
  (random-seed 1)
  (check-equal? (remove-duplicates (block-labels (lambda (i) (* 0.2 (- (random) 0.5))) 6))
                '(noise)))

(test-case "a held chord is music"
  (check-equal? (remove-duplicates (block-labels chord 6)) '(music)))

(test-case "smoothing removes isolated blocks only"
  (check-equal? (smooth-labels '(speech music speech speech noise noise))
                '(speech speech speech speech noise noise))
  (check-equal? (smooth-labels '(music speech)) '(music speech))
  (check-equal? (smooth-labels '()) '()))

(test-case "segments and totals"
  (define segs (labels->segments '(speech speech music silence) 8.0))
  (check-equal? (map segment-label segs) '(speech music silence))
  (check-= (segment-end (first segs)) 4.096 1e-9)
  (check-= (segment-end (last segs)) 8.0 1e-9 "capped at the file duration")
  (define secs (label-seconds segs))
  (check-= (hash-ref secs 'speech) 4.096 1e-9)
  (check-= (hash-ref secs 'noise) 0.0 1e-9))

(test-case "primary label"
  (check-equal? (primary-label (hash 'speech 70.0 'music 20.0 'noise 10.0 'silence 500.0)) 'speech)
  (check-equal? (primary-label (hash 'speech 50.0 'music 50.0 'noise 0.0 'silence 0.0)) 'mixed)
  (check-equal? (primary-label (hash 'speech 0.0 'music 0.0 'noise 0.5 'silence 60.0)) 'silence))

(test-case "formatting"
  (check-equal? (format-duration 3725.4) "1:02:05")
  (check-equal? (format-size 1536) "2 kB")
  (check-equal? (format-size (* 5 1024 1024)) "5.0 MB"))

(test-case "audio file extensions"
  (check-true (audio-file? (string->path "/x/Aufnahme.M4A")))
  (check-false (audio-file? (string->path "/x/notes.txt")))
  (check-false (audio-file? (string->path "/x/noextension"))))

(define (rec path #:sha1 [sha #f] #:content [content 'speech])
  (define segs (labels->segments '(speech speech noise)))
  (file-record path 1000 42 "ok" #f sha "wav" "pcm_s16le" 16000 1 256000 6.144
               "Title" #f #f #f content (label-seconds segs) segs))

(test-case "catalog round trip"
  (define conn (open-catalog 'memory))
  (store-file! conn (rec "/a/one.wav" #:sha1 "abc"))
  (store-file! conn (rec "/a/sub/two.wav" #:sha1 "abc" #:content 'music))
  (store-file! conn (rec "/b/three.wav" #:sha1 "def"))
  (check-equal? (known-file-state conn "/a/one.wav") (vector 1000 42 "ok" analyzer-version))
  (check-false (known-file-state conn "/nope.wav"))
  (check-equal? (vector-ref (catalog-totals conn) 0) 3)
  (check-equal? (map segment-label (file-segments conn "/a/one.wav")) '(speech noise))
  (check-equal? (duplicate-groups conn) '(("/a/one.wav" "/a/sub/two.wav")))
  (check-equal? (length (query-files conn #:content "music")) 1)
  (check-equal? (length (query-files conn #:under "/a/")) 2)
  ;; storing again replaces the row and its segments
  (store-file! conn (rec "/a/one.wav" #:sha1 "xyz"))
  (check-equal? (length (file-segments conn "/a/one.wav")) 2)
  (check-equal? (duplicate-groups conn) '())
  ;; pruning only touches the given root
  (check-equal? (forget-missing! conn "/a" (hash "/a/one.wav" #t)) 1)
  (check-equal? (vector-ref (catalog-totals conn) 0) 2))

(test-case "paths match across Unicode normal forms"
  (define conn (open-catalog 'memory))
  (define nfc "/m/Müsik/a.wav")
  (define nfd "/m/Müsik/a.wav")
  (store-file! conn (rec nfc))
  (check-equal? (length (file-segments conn nfd)) 2)
  (check-equal? (length (query-files conn #:under "/m/Müsik")) 1))
