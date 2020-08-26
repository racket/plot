#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/42 - arrows and arrows3d

(define (do-plot-arrows output-fn)
  (output-fn (arrows '((0 0)(1 1)))))

(define (do-plot-arrows+param output-fn)
  (define skip '(+nan.0 +nan.0))
  (parameterize ([arrow-head-size-scale '(= 10)])
    (output-fn
     (arrows
      `((0 0)(2 1)(3 3),skip(0 0)(3 3))
      #:color 6 #:label "a=b+c"))))

(define (do-plot-arrows3d output-fn)
  (output-fn (arrows3d '((0 0 0)(1 1 1)(1 1 0)))))

(define-runtime-path pr42-arrows-data "./test-data/pr42-1.dat")
(define-runtime-path pr42-arrows+param-data "./test-data/pr42-2.dat")
(define-runtime-path pr42-arrows3d-data "./test-data/pr42-3.dat")

(define pr42-test-suite
  (test-suite
   "PR#42: Arrows and arrows3d"
   (test-case "pr42-arrows"
     (check-draw-steps do-plot-arrows pr42-arrows-data))
   (test-case "pr42-arrows+parameters"
     (check-draw-steps do-plot-arrows+param pr42-arrows+param-data))
   (test-case "pr42-arrows3d"
     (check-draw-steps-3d do-plot-arrows3d pr42-arrows3d-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr42-test-suite))
