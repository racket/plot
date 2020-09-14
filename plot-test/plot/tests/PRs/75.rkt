#lang racket
(require rackunit
         plot pict
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/75

(define (do-plot-margin-calc output-fn)
  (output-fn (function values 0 1)
             #:title (standard-fish 700 350)))

(define-runtime-path pr75-data "./test-data/pr75.dat")

(define pr75-test-suite
  (test-suite
   "PR#75: margin error"
   (test-case "pr75-margin"
     (check-draw-steps do-plot-margin-calc pr75-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr75-test-suite))
