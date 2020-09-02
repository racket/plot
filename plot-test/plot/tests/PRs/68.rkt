#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/68

(define (do-plot-vector-field output-fn)
  (output-fn
   (vector-field (λ (x y) (list x 0))
                 -10 10 -10 10
                 #:scale 'auto)))

(define-runtime-path pr68-vectorfield-data "./test-data/pr68.dat")

(define pr68-test-suite
  (test-suite
   "PR#68: vector-field divide by zero error"
   (test-case "pr68-vectorfield"
     (check-draw-steps do-plot-vector-field pr68-vectorfield-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr68-test-suite))
