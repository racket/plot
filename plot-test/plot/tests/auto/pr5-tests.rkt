#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/5

;; This is the function which generates the plot.  The function sets up the
;; plot parameters, prepares a renderer tree and passes it to OUTPUT-FN.  See
;; below how this is used.
(define (do-plot output-fn)
  (parameterize ([plot-font-size 8]
                 [plot-font-family 'default]
                 [plot-legend-font-size 14]
                 [plot-legend-font-family 'modern])
    (output-fn (function sin -5 5 #:label "sin(x)"))))

;; To generate a plot image (e.g. to compare it to the saves samples, run:
;;
;;   (do-plot (lambda (rt) (plot-file rt "./data/pr5.png")))
;;
(define-runtime-path pr5-data "./data/pr5-data.rktd")

(define pr5-test-suite
  (test-suite
   "PR#5: Separate legend font from plot font for more control."
   (test-case "pr5" (check-draw-steps do-plot pr5-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr5-test-suite))
