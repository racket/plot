#lang racket
(require rackunit
         plot pict
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/76

(define (do-plot-vector-field output-fn)
  (output-fn (function values 0 1)
             #:title (text "a realy realy realy realy realy realy realy realy ridiculously high title" null 12 1.55)))

(define-runtime-path pr76-data "./test-data/pr76.dat")

(define pr76-test-suite
  (test-suite
   "PR#76: margin error"
   (test-case "pr76-margin"
     (check-draw-steps do-plot-vector-field pr76-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr76-test-suite))
