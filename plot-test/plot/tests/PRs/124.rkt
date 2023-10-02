#lang racket

(require rackunit
         plot racket/runtime-path
         "../helpers.rkt")

;; see https://github.com/racket/plot/pull/124

(define-runtime-path pr124-data "./test-data/pr124.dat")

(define (do-plot-pr124 output-fn)
  (parameterize
      ([plot-line-width 15]
       [line-width 15]
       [plot-inset '(0 10 10 0)]
       [plot-pen-color-map 'set1]
       [plot-legend-padding '(0 15 0 0)]
       [plot-line-cap 'round]
       [line-cap 'round])
    (output-fn
     (list
      (function sin -5 5 #:color 0 #:label "sin(x)")
      (function cos -5 5 #:color 1 #:label "cos(x)")))))

(define pr124-test-suite
  (test-suite
   "PR#124: Add plot-inset and plot-legend-padding parameters"
   (test-case "pr124"
     (check-draw-steps do-plot-pr124 pr124-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr124-test-suite))
