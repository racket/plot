#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/5

(define (do-plot output-fn)
  (parameterize ([plot-font-size 8]
                 [plot-font-family 'default]
                 [plot-legend-font-size 14]
                 [plot-legend-font-family 'modern])
    (output-fn (function sin -5 5 #:label "sin(x)"))))

(define-runtime-path pr5-data "./test-data/pr5.dat")

(define pr5-test-suite
  (test-suite
   "PR#5: Separate legend font from plot font for more control."
   (test-case "pr5" (check-draw-steps do-plot pr5-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr5-test-suite))
