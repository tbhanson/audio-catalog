#lang racket/base
;; classify.rkt — turn block features into labels, segments and a summary.
;;
;; Labels: 'silence 'speech 'music 'noise.  The rules are deliberately
;; simple thresholds; they were set by looking at feature distributions for
;; read speech, assorted music and household/office noise (see README).
;; Everything here is pure, so it is easy to retune and unit-test.

(require racket/list
         "features.rkt")

(provide labels
         classify-block
         smooth-labels
         labels->segments
         (struct-out segment)
         label-seconds
         primary-label)

(define labels '(speech music noise silence))

;; block-feat -> label
(define (classify-block b)
  (define flux (block-feat-flux b))
  (define flux-sd (block-feat-flux-sd b))
  (define flat (block-feat-flat b))
  (define tonal (block-feat-tonal b))
  (define persist (block-feat-persist b))
  (cond
    ;; almost nothing above the silence floor
    [(>= (block-feat-silent b) 0.8) 'silence]
    ;; Music: narrow spectral peaks that stay put from frame to frame (held
    ;; notes), over a non-noisy background, with a slowly changing spectrum.
    ;; The flatness test keeps out whining motors such as vacuum cleaners.
    [(and (> persist 0.38)
          (< flat 0.06)
          (or (< flux 0.53)
              (and (> tonal 0.40) (> persist 0.50))))
     'music]
    ;; Speech: voiced sounds are tonal, but the spectrum keeps jumping
    ;; between vowels, consonants and pauses.
    [(and (>= flux 0.53)
          (or (and (> tonal 0.17) (> flux-sd 0.16))
              (> tonal 0.34)))
     'speech]
    [else 'noise]))

;; Remove single-block outliers: a block whose two neighbours agree with
;; each other but not with it takes the neighbours' label.
(define (smooth-labels ls)
  (define v (list->vector ls))
  (define n (vector-length v))
  (for/list ([i (in-range n)])
    (define cur (vector-ref v i))
    (cond
      [(or (= i 0) (= i (sub1 n))) cur]
      [else
       (define before (vector-ref v (sub1 i)))
       (define after (vector-ref v (add1 i)))
       (if (and (eq? before after) (not (eq? cur before))) before cur)])))

(struct segment (start end label) #:transparent)

;; Merge runs of equal labels into segments (times in seconds).  `duration`,
;; when known, caps the end of the last segment.
(define (labels->segments ls [duration #f])
  (define (cap t) (if duration (min t duration) t))
  (let loop ([ls ls] [i 0] [acc '()])
    (cond
      [(null? ls) (reverse acc)]
      [else
       (define label (car ls))
       (define run (length (takef ls (lambda (l) (eq? l label)))))
       (loop (drop ls run)
             (+ i run)
             (cons (segment (cap (* i block-seconds))
                            (cap (* (+ i run) block-seconds))
                            label)
                   acc))])))

;; segments -> immutable hash label -> seconds (every label present)
(define (label-seconds segments)
  (for/fold ([h (for/hash ([l labels]) (values l 0.0))])
            ([s segments])
    (hash-update h (segment-label s)
                 (lambda (t) (+ t (- (segment-end s) (segment-start s)))))))

;; One word for the whole file: a label that covers at least 60 % of the
;; non-silent time, else 'mixed; 'silence if there is (almost) nothing.
(define (primary-label seconds)
  (define sounding
    (for/sum ([l '(speech music noise)]) (hash-ref seconds l 0.0)))
  (cond
    [(< sounding 1.0) 'silence]
    [else
     (or (for/first ([l '(speech music noise)]
                     #:when (>= (hash-ref seconds l 0.0) (* 0.6 sounding)))
           l)
         'mixed)]))
