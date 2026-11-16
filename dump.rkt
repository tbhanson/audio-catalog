#lang racket/base
;; dump.rkt — print the raw block features of audio files, one line per ~2 s.
;; Use this to see why the classifier decided what it did, or to retune it:
;;   racket dump.rkt file.mp3 ...
(require racket/format racket/cmdline "features.rkt" "decode.rkt" "classify.rkt")

(define (f x) (~a (~r x #:precision '(= 3)) #:min-width 8 #:align 'right))

(module+ main
  (command-line
   #:args files
   (printf "~a\n" "  label  silent   level    dips     mod     zcr  zcr-sd    flat   tonal persist    flux flux-sd  file")
   (for ([file files])
     (for ([b (call-with-pcm-port file read-block-features)])
       (printf "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a  ~a\n"
               (~a (classify-block b) #:min-width 7 #:align 'right)
               (f (block-feat-silent b)) (f (block-feat-level-db b))
               (f (block-feat-dips b)) (f (block-feat-mod b))
               (f (block-feat-zcr b)) (f (block-feat-zcr-sd b))
               (f (block-feat-flat b)) (f (block-feat-tonal b))
               (f (block-feat-persist b)) (f (block-feat-flux b))
               (f (block-feat-flux-sd b))
               file)))))
