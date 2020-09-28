#lang racket
(require rackunit
         plot pict
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/68

(define rendertree
  (list (contour-intervals (λ (x y) (* x y)) 0 1 #:label "a")
        (function values 0 1 #:label "b" #:color 1)
        (function add1 0 1 #:label "a rather long one"  #:color 2)
        (function sub1 0 1 #:label (standard-fish 20 40)  #:color 4)))
  
(define (do-3row output-fn)
  (parameterize ([plot-legend-layout '(rows 3)])
    (output-fn rendertree #:legend-anchor 'outside-top)))

(define (do-2col output-fn)
  (parameterize ([plot-legend-layout '(columns 2)])
    (output-fn rendertree #:legend-anchor 'outside-left)))

(define-runtime-path pr81-data-1 "./test-data/pr81-1.dat")
(define-runtime-path pr81-data-2 "./test-data/pr81-2.dat")

(define pr81-test-suite
  (test-suite
   "PR#81: horizontal layout for legend"
   (test-case "pr81-3row"
     (check-draw-steps do-3row pr81-data-1))
   (test-case "pr81-2col"
     (check-draw-steps do-2col pr81-data-2))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr81-test-suite))
