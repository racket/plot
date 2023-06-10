#lang racket

(require rackunit
         plot racket/runtime-path
         "../helpers.rkt")

;; See https://github.com/racket/plot/issues/117 and associated discution.

(define-runtime-path pr117-data "./test-data/pr117.dat")

(define (do-plot-pr117 output-fn)
  (parameterize ([plot-y-transform log-transform]
                 [plot-y-ticks (ticks-add
                                (log-ticks #:number 10)
                                (list 500 ; should be visible on the plot
                                      (+ (expt 10 3) 500) ; should be visible on the plot
                                      (+ (expt 10 4) 500) ; should be visible, next to the 10^4 label
                                      (+ (expt 10 5) 500) ; should not be visible, too close to 10^5
                                      (+ (expt 10 6) 500) ; should not be visible, too colse to 10^6
                                      (+ (expt 10 7) 500) ; should not be visible, too close to 10^7
                                      ))])
    (output-fn (function exp) #:x-min 0. #:x-max 20)))

(define pr117-test-suite
  (test-suite
   "PR#117: Tick labels at the wrong place on log plot"
   (test-case "pr117"
     (check-draw-steps do-plot-pr117 pr117-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr117-test-suite))
