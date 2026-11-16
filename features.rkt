#lang racket/base
;; features.rkt — signal features for coarse audio content classification.
;;
;; Input is mono PCM at 16 kHz.  We cut it into 64 ms frames (hop 32 ms),
;; compute a handful of classic speech/music-discrimination features per
;; frame, then summarise ~2 s blocks of frames.  No learned model: the
;; classifier in classify.rkt is a few hand-set thresholds on these numbers.

(require racket/flonum
         racket/fixnum
         racket/math
         racket/list)

(provide sample-rate frame-size hop-size block-frames block-seconds
         (struct-out frame-feat)
         (struct-out block-feat)
         make-frame-analyzer
         summarize-block
         fft!)

(define (fllog10 x) (fl/ (fllog x) (fllog 10.0)))

(define sample-rate 16000)
(define frame-size 1024)
(define hop-size 512)
(define block-frames 64)
(define block-seconds (/ (* block-frames hop-size) (exact->inexact sample-rate)))

;; Spectral features are computed over ~94 Hz .. 6 kHz.
(define bin-lo 6)
(define bin-hi 384)
(define silence-db -55.0)

;; rms-db  : frame level in dBFS
;; zcr     : zero crossings per sample (0..1)
;; flat    : spectral flatness, 0 = pure tones, 1 = white noise
;; tonal   : share of band power sitting in narrow spectral peaks
;; persist : share of those peaks that were already peaks one frame earlier
;; flux    : distance between this and the previous normalised spectrum (0..~1.4)
(struct frame-feat (rms-db zcr flat tonal persist flux) #:transparent)

;; ---------------------------------------------------------------- FFT

;; In-place iterative radix-2 FFT on separate real/imag flvectors.
(define (fft! re im)
  (define n (flvector-length re))
  ;; bit-reversal permutation
  (let loop ([i 1] [j 0])
    (when (< i n)
      (define j* (let bit-loop ([bit (quotient n 2)] [j j])
                   (if (zero? (bitwise-and j bit))
                       (bitwise-ior j bit)
                       (bit-loop (quotient bit 2) (bitwise-xor j bit)))))
      (when (< i j*)
        (define tr (flvector-ref re i))
        (define ti (flvector-ref im i))
        (flvector-set! re i (flvector-ref re j*))
        (flvector-set! im i (flvector-ref im j*))
        (flvector-set! re j* tr)
        (flvector-set! im j* ti))
      (loop (add1 i) j*)))
  (let stage ([len 2])
    (when (<= len n)
      (define ang (/ (* -2.0 pi) len))
      (define wr (cos ang))
      (define wi (sin ang))
      (define half (quotient len 2))
      (for ([start (in-range 0 n len)])
        (for/fold ([cr 1.0] [ci 0.0]) ([k (in-range half)])
          (define a (fx+ start k))
          (define b (fx+ a half))
          (define xr (fl- (fl* (flvector-ref re b) cr) (fl* (flvector-ref im b) ci)))
          (define xi (fl+ (fl* (flvector-ref re b) ci) (fl* (flvector-ref im b) cr)))
          (flvector-set! re b (fl- (flvector-ref re a) xr))
          (flvector-set! im b (fl- (flvector-ref im a) xi))
          (flvector-set! re a (fl+ (flvector-ref re a) xr))
          (flvector-set! im a (fl+ (flvector-ref im a) xi))
          (values (fl- (fl* cr wr) (fl* ci wi))
                  (fl+ (fl* cr wi) (fl* ci wr)))))
      (stage (* len 2)))))

;; ---------------------------------------------------------------- frames

(define hann
  (for/flvector #:length frame-size ([i (in-range frame-size)])
    (* 0.5 (- 1.0 (cos (/ (* 2.0 pi i) (sub1 frame-size)))))))

;; Returns a procedure  (flvector of frame-size samples in -1..1) -> frame-feat.
;; The procedure keeps the previous frame's spectrum internally (needed for
;; flux and peak persistence), so use one analyzer per audio stream.
(define (make-frame-analyzer)
  (define half (quotient frame-size 2))
  (define re (make-flvector frame-size))
  (define im (make-flvector frame-size))
  (define pow (make-flvector half))
  (define prefix (make-flvector (add1 half)))
  (define norm (make-flvector half))
  (define prev-norm (make-flvector half))
  (define peaks (make-bytes half 0))
  (define prev-peaks (make-bytes half 0))
  (define first? #t)
  (lambda (samples)
    ;; time domain: level and zero crossings
    (define-values (energy crossings)
      (for/fold ([e 0.0] [z 0]) ([i (in-range frame-size)])
        (define x (flvector-ref samples i))
        (values (fl+ e (fl* x x))
                (if (and (fx> i 0)
                         (not (eq? (fl>= x 0.0)
                                   (fl>= (flvector-ref samples (fx- i 1)) 0.0))))
                    (fx+ z 1)
                    z))))
    (define rms (flsqrt (fl/ energy (->fl frame-size))))
    (define rms-db (if (fl> rms 1e-10) (fl* 20.0 (fllog10 rms)) -200.0))
    ;; spectrum
    (for ([i (in-range frame-size)])
      (flvector-set! re i (fl* (flvector-ref samples i) (flvector-ref hann i)))
      (flvector-set! im i 0.0))
    (fft! re im)
    (flvector-set! prefix 0 0.0)
    (for ([k (in-range half)])
      (define p (fl+ (fl* (flvector-ref re k) (flvector-ref re k))
                     (fl* (flvector-ref im k) (flvector-ref im k))))
      (flvector-set! pow k p)
      (flvector-set! prefix (fx+ k 1) (fl+ (flvector-ref prefix k) p)))
    (define (band-sum lo hi) ; power in bins lo..hi-1
      (fl- (flvector-ref prefix hi) (flvector-ref prefix lo)))
    (define total (fl+ (band-sum bin-lo bin-hi) 1e-20))
    (define nbins (->fl (- bin-hi bin-lo)))
    ;; flatness: geometric mean / arithmetic mean of band power
    (define log-sum
      (for/fold ([s 0.0]) ([k (in-range bin-lo bin-hi)])
        (fl+ s (fllog (fl+ (flvector-ref pow k) 1e-20)))))
    (define flat (fl/ (flexp (fl/ log-sum nbins)) (fl/ total nbins)))
    ;; tonal peaks: local maxima standing well above their neighbourhood
    (bytes-fill! peaks 0)
    (define-values (peak-power n-peaks n-persist)
      (for/fold ([pp 0.0] [np 0] [ns 0]) ([k (in-range bin-lo bin-hi)])
        (define p (flvector-ref pow k))
        (define lo (fxmax 1 (fx- k 10)))
        (define hi (fxmin half (fx+ k 11)))
        (define around ; mean power nearby, excluding the peak itself
          (fl/ (fl- (band-sum lo hi) (band-sum (fx- k 1) (fx+ k 2)))
               (->fl (fx- (fx- hi lo) 3))))
        (cond
          [(and (fl> p (flvector-ref pow (fx- k 1)))
                (fl>= p (flvector-ref pow (fx+ k 1)))
                (fl> p (fl* 8.0 around)))
           (bytes-set! peaks k 1)
           (values (fl+ pp (band-sum (fx- k 1) (fx+ k 2)))
                   (fx+ np 1)
                   (if (or (fx= 1 (bytes-ref prev-peaks k))
                           (fx= 1 (bytes-ref prev-peaks (fx- k 1)))
                           (fx= 1 (bytes-ref prev-peaks (fx+ k 1))))
                       (fx+ ns 1)
                       ns))]
          [else (values pp np ns)])))
    (define tonal (flmin 1.0 (fl/ peak-power total)))
    (define persist (if (fx= n-peaks 0) 0.0 (fl/ (->fl n-persist) (->fl n-peaks))))
    ;; flux between unit-normalised magnitude spectra
    (define mag-norm (flsqrt total))
    (define flux-sq
      (for/fold ([s 0.0]) ([k (in-range bin-lo bin-hi)])
        (define m (fl/ (flsqrt (flvector-ref pow k)) mag-norm))
        (define d (fl- m (flvector-ref prev-norm k)))
        (flvector-set! norm k m)
        (fl+ s (fl* d d))))
    (define flux (if first? 0.0 (flsqrt flux-sq)))
    (set! first? #f)
    (for ([k (in-range bin-lo bin-hi)])
      (flvector-set! prev-norm k (flvector-ref norm k)))
    (bytes-copy! prev-peaks 0 peaks)
    (frame-feat rms-db
                (fl/ (->fl crossings) (->fl frame-size))
                flat tonal persist flux)))

;; ---------------------------------------------------------------- blocks

;; Summary of ~2 s of frames.  Spectral means are taken over non-silent
;; frames only, so pauses do not dilute them.
;; silent   : share of frames below the silence floor
;; level-db : mean level of the non-silent frames
;; dips     : share of non-silent frames more than 6 dB under the block's
;;            mean power — speech has many (gaps between syllables)
;; mod      : strength of 2–8 Hz loudness modulation (syllable rhythm)
(struct block-feat (silent level-db dips mod zcr zcr-sd flat tonal persist flux flux-sd)
  #:transparent)

(define (mean xs) (if (null? xs) 0.0 (/ (apply + xs) (->fl (length xs)))))
(define (std-dev xs)
  (define m (mean xs))
  (if (null? xs) 0.0 (flsqrt (mean (map (lambda (x) (* (- x m) (- x m))) xs)))))

(define (db->power db) (flexpt 10.0 (fl/ db 10.0)))

(define (summarize-block frames)
  (define n (length frames))
  (define loud (filter (lambda (f) (> (frame-feat-rms-db f) silence-db)) frames))
  (define powers (map (lambda (f) (db->power (frame-feat-rms-db f))) loud))
  (define mean-power (mean powers))
  (define (avg sel) (mean (map sel loud)))
  ;; loudness-envelope modulation: DFT of the frame amplitude sequence
  (define env (map (lambda (f) (flsqrt (db->power (frame-feat-rms-db f)))) frames))
  (define frame-rate (/ sample-rate (->fl hop-size)))
  (define (dft-power k)
    (define-values (re im)
      (for/fold ([re 0.0] [im 0.0]) ([x (in-list env)] [i (in-naturals)])
        (define a (/ (* 2.0 pi k i) n))
        (values (+ re (* x (cos a))) (+ im (* x (sin a))))))
    (+ (* re re) (* im im)))
  (define k-lo (max 1 (exact-ceiling (/ (* 2.0 n) frame-rate))))
  (define k-hi (exact-floor (/ (* 8.0 n) frame-rate)))
  (define dc (dft-power 0))
  (define mod
    (if (or (< n 16) (<= dc 0.0))
        0.0
        (/ (for/sum ([k (in-range k-lo (add1 k-hi))]) (dft-power k)) dc)))
  (block-feat (if (zero? n) 1.0 (- 1.0 (/ (length loud) (->fl n))))
              (if (null? loud) -200.0 (* 10.0 (fllog10 mean-power)))
              (if (null? loud)
                  0.0
                  (/ (count (lambda (p) (< p (* 0.25 mean-power))) powers)
                     (->fl (length loud))))
              mod
              (avg frame-feat-zcr)
              (std-dev (map frame-feat-zcr loud))
              (avg frame-feat-flat)
              (avg frame-feat-tonal)
              (avg frame-feat-persist)
              (avg frame-feat-flux)
              (std-dev (map frame-feat-flux loud))))
