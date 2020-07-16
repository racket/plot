#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/59

(define (do-plot-not-inverted output-fn)
  (output-fn (list (function sqr 1 7)
                   (error-bars (list (vector 2 4 12)
                                     (vector 4 16 20)
                                     (vector 6 36 10))
                               #:invert? #f))))

;; NOTE: both the invert? parameter and the coordinates need to be inverted.
(define (do-plot-inverted output-fn)
  (output-fn (list (function sqr 1 7)
                   (error-bars (list (vector 4 2 12)
                                     (vector 16 4 20)
                                     (vector 36 6 10))
                               #:invert? #t))))

(define-runtime-path pr59-not-inverted-data "./data/pr59-not-inverted-data.rktd")
(define-runtime-path pr59-inverted-data "./data/pr59-inverted-data.rktd")

(define pr59-test-suite
  (test-suite
   "PR#59: Add #:invert? option to error-bars"
   (test-case "pr59 not inverted"
     (check-draw-steps do-plot-not-inverted pr59-not-inverted-data))
   (test-case "pr59 inverted"
     (check-draw-steps do-plot-inverted pr59-inverted-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr59-test-suite))
