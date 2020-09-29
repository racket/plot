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
  
(define (do-3row-equal output-fn)
  (parameterize ([plot-legend-layout '(rows 3 equal-size)])
    (output-fn rendertree #:legend-anchor 'outside-top)))

(define (do-3row-compact output-fn)
  (parameterize ([plot-legend-layout '(rows 3 compact)])
    (output-fn rendertree #:legend-anchor 'outside-top)))

(define (do-2col-equal output-fn)
  (parameterize ([plot-legend-layout '(columns 2 equal-size)])
    (output-fn rendertree #:legend-anchor 'outside-left)))

(define (do-2col-compact output-fn)
  (parameterize ([plot-legend-layout '(columns 2 compact)])
    (output-fn rendertree #:legend-anchor 'outside-left)))

(define-runtime-path pr81-data-1 "./test-data/pr81-1.dat")
(define-runtime-path pr81-data-2 "./test-data/pr81-2.dat")
(define-runtime-path pr81-data-3 "./test-data/pr81-3.dat")
(define-runtime-path pr81-data-4 "./test-data/pr81-4.dat")

(define pr81-test-suite
  (test-suite
   "PR#81: horizontal layout for legend"
   (test-case "pr81-3row-equal"
     (check-draw-steps do-3row-equal pr81-data-1))
   (test-case "pr81-3row-compact"
     (check-draw-steps do-3row-compact pr81-data-2))
   (test-case "pr81-2col-equal"
     (check-draw-steps do-2col-equal pr81-data-3))
   (test-case "pr81-2col-compact"
     (check-draw-steps do-2col-compact pr81-data-4))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr81-test-suite))
