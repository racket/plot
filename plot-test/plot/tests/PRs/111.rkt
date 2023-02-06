#lang racket

(require rackunit
         plot racket/runtime-path
         "../helpers.rkt")

;; Tests for https://github.com/racket/plot/pull/100, ignoring axis transforms
;; for lines renderer.

(define (do-plot output-fn)
  (define x (list 28 0.13 0.006765))
  (define y (list 1.27 0.63 0.325))
  (parameterize ([plot-x-transform log-transform]
                 [plot-y-transform log-transform])
    (output-fn
     (list
      (tick-grid)
      (lines (map vector x y)
             #:label "with axis transforms"
             #:color 1)
      (lines (map vector x y)
             #:ignore-axis-transforms? #t
             #:label "no axis transforms"
             #:color 2))
      #:title "Log-Log Plot")))

(define-runtime-path pr111-data "./test-data/pr111.dat")

(define pr111-test-suite
  (test-suite
   "PR#111: add no-axis-transforms? to lines renderer"
   (test-case "Log-Log Plot With no-axis-transforms?"
     (check-draw-steps do-plot pr111-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr111-test-suite))
