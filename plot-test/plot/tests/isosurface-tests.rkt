#lang racket
(require rackunit
         plot
         plot/utils
         racket/draw
         racket/runtime-path
         math/flonum
         "helpers.rkt")


(define (do-plot1 output-fn)
  (output-fn (isosurface3d (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z)))) 1
                           #:color 2 #:line-style 'transparent
                           #:label "Sphere")
             #:x-min -0.8 #:x-max 0.8
             #:y-min -0.8 #:y-max 0.8
             #:z-min -0.8 #:z-max 0.8
             #:altitude 30
             #:legend-anchor 'center))


(define (do-plot2 output-fn)
  (define saddle (λ (x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z))))))
  (output-fn (isosurfaces3d saddle #:d-min -1 #:d-max 1 #:label "d")
             #:x-min -2 #:x-max 2
             #:y-min -2 #:y-max 2
             #:z-min -2 #:z-max 2
             #:legend-anchor 'top-left))

(define (do-plot3 output-fn)
  (define saddle (λ (x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z))))))
  (output-fn (isosurfaces3d saddle #:d-min -1 #:d-max 1
                            #:colors '(1 2 3)
                            #:line-colors '(1 2 3)
                            #:line-styles '(solid)
                            #:alphas '(3/4)
                            #:label "d")
         #:x-min -2 #:x-max 2
         #:y-min -2 #:y-max 2
         #:z-min -2 #:z-max 2
         #:legend-anchor 'top-left))

(define (do-plot4 output-fn)
  (define saddle (λ (x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z))))))
  (output-fn (isosurface3d saddle -1/4 #:samples 21
                           #:color "black" #:style 3
                           #:alpha 1
                           #:label "d = -1/4")
             #:x-min -2 #:x-max 2
             #:y-min -2 #:y-max 2
             #:z-min -2 #:z-max 2))

(define 2pi (* 2 pi))

(define (do-plot5 output-fn)
  (define (f1 θ ρ) (+ 1 (/ θ 2pi) (* 1/8 (sin (* 8 ρ)))))
  (define (f2 θ ρ) (+ (/ θ 2pi) (* 1/8 (sin (* 8 ρ)))))
  (output-fn (list (polar3d f1 #:samples 41 #:color "navajowhite" #:line-style 'transparent #:alpha 2/3)
                   (polar3d f2 #:samples 41 #:color "navajowhite" #:line-style 'transparent #:alpha 2/3)
                   (parametric3d (λ (ρ) (3d-polar->3d-cartesian 0 ρ (f1 2pi ρ)))
                                 (* -1/2 pi) (* 1/2 pi)
                                 #:color "navajowhite" #:width 2)
                   (lines3d '(#(0 0 2) #(0 0 -2)) #:color "navajowhite" #:width 2))
             #:title "A Seashell" #:x-label #f #:y-label #f #:angle 210 #:altitude 30))

(define flepsilon (flnext 0.0))

(define (do-plot6 output-fn)
  (output-fn (isosurface3d (λ (x y z) (+ (sqr x) (sqr y) (sqr z))) (sqr (inexact->exact flepsilon))
                           (- flepsilon) flepsilon (- flepsilon) flepsilon (- flepsilon) flepsilon)))

(define-runtime-path isosurface-tests-1-data "./test-data/is-1.dat")
(define-runtime-path isosurface-tests-2-data "./test-data/is-2.dat")
(define-runtime-path isosurface-tests-3-data "./test-data/is-3.dat")
(define-runtime-path isosurface-tests-4-data "./test-data/is-4.dat")
(define-runtime-path isosurface-tests-5-data "./test-data/is-5.dat")
(define-runtime-path isosurface-tests-6-data "./test-data/is-6.dat")


(define isosurface-tests
  (test-suite
   "isosurface-tests"
   (test-case "test case 1" (check-draw-steps-3d do-plot1 isosurface-tests-1-data))
   (test-case "test case 2" (check-draw-steps-3d do-plot2 isosurface-tests-2-data))
   (test-case "test case 3" (check-draw-steps-3d do-plot3 isosurface-tests-3-data))
   (test-case "test case 4" (check-draw-steps-3d do-plot4 isosurface-tests-4-data))
   (test-case "test case 5" (check-draw-steps-3d do-plot5 isosurface-tests-5-data))
   (test-case "test case 6" (check-draw-steps-3d do-plot6 isosurface-tests-6-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests isosurface-tests))


