#lang racket
(require rackunit
         plot pict
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/68

(define (do-plot-title output-fn)
  (output-fn
   (surface3d (λ (x y) 1) 0 1 0 1)
   #:title (text "a high title" null 12 1.55)))

(define-runtime-path pr76-data "./test-data/pr76.dat")

(define pr76-test-suite
  (test-suite
   "PR#76: pict title for plot3d"
   (test-case "pr76-plot3d-title"
     (check-draw-steps-3d do-plot-title pr76-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr76-test-suite))
