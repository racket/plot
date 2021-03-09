#lang racket
(require rackunit
         plot pict racket/runtime-path
         "../helpers.rkt")

; tests for PR#89, https://github.com/racket/plot/pull/89
; "fix calculation of text-extends for anchor 'right center left"

(define (do-plot output-fn)
  (parameterize ([plot-x-label #f]
                 [plot-y-label #f])
    (output-fn
     (polar (lambda (t) 1))
      #:x-min 4025584149602844706/10000000000000000000
      #:x-max 4025584149602844712/10000000000000000000
      #:y-min 6805367900508167/10000000000000000
      #:y-max 6805367900508171/10000000000000000)))

(define-runtime-path pr89 "./test-data/pr89.dat")

(define pr86-test-suite
  (test-suite
   "PR#89: text-extents"
   (test-case "PR89: text-extents"
     (check-draw-steps do-plot pr89))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr86-test-suite))
