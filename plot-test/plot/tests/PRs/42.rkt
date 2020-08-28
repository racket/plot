#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/42 - arrows and arrows3d

(define (do-plot-arrows output-fn)
  (parameterize ([arrow-head-size-or-scale '(= 10)]
                 [arrow-head-angle .1])
    (output-fn
     (list
      (arrows
       `((0 0) (2 1) (3 3) (0 0))
       #:color 6 #:label "a+b+c=0")
      (arrows
       `(((2 0) (0 1)) ((3 0) (-1 1)))
       #:color 2 #:label "d")))))

(define (do-plot-arrows3d output-fn)
  (parameterize ([arrow-head-size-or-scale .5]
                 [arrow-head-angle .3]
                 [arrows-color 4])
    (output-fn
     (list (arrows3d #((0 0 0)(1 1 1)(1 1 0)))
           (arrows3d #(#(#(1 2 3)#(-1 -1 -1))
                       #(#(2 0 2)#(0 2 0))))))))

(define (do-plot-vectorfield output-fn)
  (parameterize ([arrow-head-size-or-scale '(= 10)]
                 [arrow-head-angle .1])
    (output-fn
     (vector-field (λ (x y) (vector (+ x y) (- x y)))
                   -2 2 -2 2))))

(define (do-plot-vectorfield3d output-fn)
  (parameterize ([arrow-head-size-or-scale .3]
                 [arrow-head-angle 1.])
    (output-fn
     (vector-field3d (λ (x y z) (vector x z y))
                     -2 2 -2 2 -2 2))))

(define-runtime-path pr42-arrows-data "./test-data/pr42-1.dat")
(define-runtime-path pr42-arrows3d-data "./test-data/pr42-2.dat")
(define-runtime-path pr42-vectorfield-data "./test-data/pr42-3.dat")
(define-runtime-path pr42-vectorfield3d-data "./test-data/pr42-4.dat")

(define pr42-test-suite
  (test-suite
   "PR#42: Arrows and arrows3d"
   (test-case "pr42-arrows"
              (check-draw-steps do-plot-arrows pr42-arrows-data))
   (test-case "pr42-arrows3d"
              (check-draw-steps-3d do-plot-arrows3d pr42-arrows3d-data))

   (test-case "pr42-vectorfield"
              (check-draw-steps do-plot-vectorfield pr42-vectorfield-data))
   (test-case "pr42-vectorfield3d"
              (check-draw-steps-3d do-plot-vectorfield3d pr42-vectorfield3d-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr42-test-suite))
