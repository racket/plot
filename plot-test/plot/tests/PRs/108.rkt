#lang racket

(require rackunit
         plot racket/runtime-path
         "../helpers.rkt")

;; Tests for https://github.com/racket/plot/issues/108, error bars are not
;; culled out based on their center location, and partial error bars should be
;; visible when the plot area does not include the entire error bar

(define-runtime-path pr108-data "./test-data/pr108.dat")

;; This plot  is set up  to have the  area such that  the center and  edges of
;; error  bars are  not in  the plot,  it should  still draw  a line  from the
;; partial error bar at 4.
(define (do-plot-pr108 output-fn)
  (output-fn
   (list (function sqr 1 7)
         (error-bars (list (vector 2 4 12)
                           (vector 4 16 20)
                           (vector 6 36 10))))
   #:x-min 3.5 #:x-max 4.5
   #:y-min 20 #:y-max 30))

(define pr108-test-suite
  (test-suite
   "PR#108: Zoom in for error-bars (plot package) not working properly."
   (test-case "pr108"
     (check-draw-steps do-plot-pr108 pr108-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr108-test-suite))
