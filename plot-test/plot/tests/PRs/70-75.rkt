#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         pict
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/70 , 71, 72, 73, 74

(define rendertree2d (list (function values 0 1 #:label "fct" #:color 'blue)
                         (contour-intervals (λ (x y) (* x y)) 1 2 #:label "cti")))

(define rendertree3d (list (surface3d (λ (x y) 1) 0 1 0 1 #:label "srf")
                           (contour-intervals3d (λ (x y) (* x y)) 1 2 1 2 #:label "cti")))

(define-runtime-path pr70/75-legend-data-1 "./test-data/pr70-1.dat")
(define (do-plot-nolegend output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'no-legend
             #:title "no-legend"))

(define-runtime-path pr70/75-legend-data-2 "./test-data/pr70-2.dat")
(define (do-plot-br-legend output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-right-bottom
             #:title "legend, outside, right-bottom"))

(define-runtime-path pr70/75-legend-data-3 "./test-data/pr70-3.dat")
(define (do-plot3d-br-legend output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-top-left
             #:title "legend, outside, top-left"))

(define-runtime-path pr70/75-legend-data-4 "./test-data/pr70-4.dat")
(define (do-plot-legend-names output-fn)
  (output-fn rendertree2d
             #:x-min -1 #:x-max 5
             #:y-min -1 #:y-max 5
             #:legend-anchor 'outside-global-top
             #:title "legend names of contour based on outside bounds, aligned outside-global-top"))

(define pr70-test-suite
  (test-suite
   "PR#70-75: legend outside of plot-area"
   (test-case "pr70-75: no legend"
     (check-draw-steps do-plot-nolegend pr70/75-legend-data-1))

   (test-case "pr70-75: br legend"
     (check-draw-steps do-plot-br-legend pr70/75-legend-data-2))

   (test-case "pr70-75: br legend 3d"
              (check-draw-steps-3d do-plot3d-br-legend pr70/75-legend-data-3))

   (test-case "pr70-75: legend name check"
              (check-draw-steps do-plot-legend-names pr70/75-legend-data-4))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr70-test-suite))
