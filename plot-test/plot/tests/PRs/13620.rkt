#lang racket
(require rackunit
         plot
         plot/utils
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; https://github.com/racket/gnats-bugs/blob/7e4bb9a65cd4783bef9936b576c5e06a5da3fb01/all/13620

(define (do-plot-contour-intervals output-fn)
  (output-fn (contour-intervals * -1 1 -1 1 #:alphas '())))

(define (do-plot-contour-intervals3d output-fn)
  (output-fn (contour-intervals3d * -1 1 -1 1 #:alphas '())))

(define (do-plot-isosurface3d output-fn)
  (output-fn (isosurfaces3d * -1 1 -1 1 -1 1 #:alphas '())))

(define-runtime-path gnats13620-ci-data "./test-data/gnats13620-ci.dat")
(define-runtime-path gnats13620-ci3d-data "./test-data/gnats13620-ci3d.dat")
(define-runtime-path gnats13620-is3d-data "./test-data/gnats13620-is3d.dat")

(define gnats13620-test-suite
  (test-suite
   "GNATS#13620: stacked-histogram loops when given an empty list of labels"
   (test-case "gnats13620-contour-intervals"
     (check-draw-steps do-plot-contour-intervals gnats13620-ci-data))
   (test-case "gnats13620-contour-intervals3d"
     (check-draw-steps-3d do-plot-contour-intervals3d gnats13620-ci3d-data))
  (test-case "gnats13620-isosurface3d"
    (check-draw-steps-3d do-plot-isosurface3d gnats13620-is3d-data))
  (test-case "gnats13620-stacked-histogram"
    ;; Should fail with a "could not determine sensible plot bounds" message
    (check-exn
     exn:fail?
     (lambda () (plot (stacked-histogram (list (vector 'a 1)) #:alphas '())))))
  (test-case "gnats13620-histogram3d"
    ;; Should fail with a "could not determine sensible plot bounds" message
    (check-exn
     exn:fail?
     (lambda () (plot3d (stacked-histogram3d (list (vector 'a 'a 1)) #:alphas '())))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests gnats13620-test-suite))
