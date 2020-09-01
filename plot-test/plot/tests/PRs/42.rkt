#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/42 - arrows and arrows3d

;; test arrows - heads
(define (do-plot-arrows1 output-fn)
  (output-fn
   (list
    (arrows
     `((0 0) (2 1) (3 3) (0 0))
     #:arrow-head-size-or-scale '(= 50)
     #:arrow-head-angle .2
     #:color 6 #:label "same size arrowheads")
    (arrows
     `(((2 0) (0 1))
       ((3 0) (-1 1)))
     #:color 2 #:label "scaled arrowheads"))))

;; test arrows going outside of the plot area
(define (do-plot-arrows2 output-fn)
  (parameterize ([arrow-head-size-or-scale '(= 50)])
    (output-fn
     (list
      (arrows '((0 5) (3 5)) #:color 1 #:width 3 #:label "start OUT, end OUT")
      (arrows '((0 4) (2 4)) #:color 2 #:width 3 #:label "start OUT, end IN")
      (arrows '((1.5 3) (3 3)) #:color 3 #:width 3 #:label "start IN, end OUT")
      (arrows '((1.5 2) (2.6 2)) #:color 4 #:width 3 #:label "start IN, end JUST OUT (ears not visible)")
      (arrows '((0 1) (2.6 1)) #:color 5 #:width 3 #:label "start OUT, end JUST OUT (ears not visible)"))
     #:x-min 1 #:x-max 2.5
     #:y-min 0 #:y-max 6
     #:legend-anchor 'bottom-left)))

;; test arrows3d - heads
(define (do-plot-arrows3d1 output-fn)
  (parameterize ([arrow-head-size-or-scale .5]
                 [arrow-head-angle .3]
                 [arrows-color 4])
    (output-fn
     (list (arrows3d #((0 0 0) (1 1 1) (1 1 0) (1 2 2))
                     #:alpha .5 #:label "scaled chained arrows")
           (arrows3d #(#(#(1 2 3) #(-1/2 -1/2 -1/2))
                       #(#(2 0 2) #(0 2 0)))
                     #:arrow-head-size-or-scale '(= 50)
                     #:arrow-head-angle .1
                     #:color 'blue
                     #:label "same size arrowheads")))))

;; test arrows going outside of the plot area
(define (do-plot-arrows3d2 output-fn)
  (parameterize ([arrow-head-size-or-scale '(= 50)])
    (output-fn
     (list
      (arrows3d '((0 5 0) (3 5 0)) #:color 1 #:width 3 #:label "start OUT, end OUT")
      (arrows3d '((0 4 0) (2 4 0)) #:color 2 #:width 3 #:label "start OUT, end IN")
      (arrows3d '((1.5 3 0) (3 3 0)) #:color 3 #:width 3 #:label "start IN, end OUT")
      (arrows3d '((1.5 2 0) (2.6 2 0)) #:color 4 #:width 3 #:label "start IN, end JUST OUT (ears not visible)")
      (arrows3d '((0 1 0) (2.6 1 0)) #:color 5 #:width 3 #:label "start OUT, end JUST OUT (ears not visible)")
      )
     #:x-min 1 #:x-max 2.5
     #:y-min 0 #:y-max 6
     #:z-min -1 #:z-max 1
     #:legend-anchor 'bottom-left)))

;; effect of parameters on vector fields
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

(define-runtime-path pr42-arrows1-data "./test-data/pr42-1-1.dat")
(define-runtime-path pr42-arrows2-data "./test-data/pr42-1-2.dat")
(define-runtime-path pr42-arrows3d1-data "./test-data/pr42-2-1.dat")
(define-runtime-path pr42-arrows3d2-data "./test-data/pr42-2-2.dat")
(define-runtime-path pr42-vectorfield-data "./test-data/pr42-3.dat")
(define-runtime-path pr42-vectorfield3d-data "./test-data/pr42-4.dat")

(define pr42-test-suite
  (test-suite
   "PR#42: Arrows and arrows3d"
   (test-case "pr42-arrows"
              (check-draw-steps do-plot-arrows1 pr42-arrows1-data))
   (test-case "pr42-arrows"
              (check-draw-steps do-plot-arrows2 pr42-arrows2-data))
   (test-case "pr42-arrows3d"
              (check-draw-steps-3d do-plot-arrows3d1 pr42-arrows3d1-data))
   (test-case "pr42-arrows3d"
              (check-draw-steps-3d do-plot-arrows3d2 pr42-arrows3d2-data))

   (test-case "pr42-vectorfield"
              (check-draw-steps do-plot-vectorfield pr42-vectorfield-data))
   (test-case "pr42-vectorfield3d"
              (check-draw-steps-3d do-plot-vectorfield3d pr42-vectorfield3d-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr42-test-suite))
