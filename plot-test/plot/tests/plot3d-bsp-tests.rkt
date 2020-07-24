#lang racket
(require plot
         (except-in plot/utils sample)
         math
         rackunit
         racket/runtime-path
         "helpers.rkt")

;; Test under crazy transformation, doing so will invalidate all test results,
;; but the plot output is nice.

#;(begin
  (plot-x-transform cbrt-transform)
  (plot-y-transform cbrt-transform)
  (plot-z-transform cbrt-transform))

(define-runtime-path p3d-bsp-1-data "./test-data/p3d-bsp-1.dat")
(define (do-plot-1 output-fn)
  (output-fn (list (contour-intervals3d * -1 1 -1 1 #:samples 2)
                   (isosurface3d (λ (x y z) z)
                                 0 -1 1 -1 1 -1 1
                                 #:samples 2 #:color 3))))

(define-runtime-path p3d-bsp-2-data "./test-data/p3d-bsp-2.dat")
(define (do-plot-2 output-fn)
  (output-fn (list (parametric3d (λ (t)
                                   (list (* 0.5 (cos (* 2 pi (- t))))
                                         (* 0.5 (sin (* 2 pi (- t))))
                                         t))
                                 -1 1
                                 #:samples 53
                                 #:color "green"
                                 #:width 15
                                 #:alpha 0.6)
                   (parametric3d (λ (t)
                                   (list (* 0.75 (cos (* 2 pi t)))
                                         (* 0.75 (sin (* 2 pi t)))
                                         t))
                                 -1 1
                                 #:samples 53
                                 #:color "red"
                                 #:width 15
                                 #:alpha 0.6))))

(define-runtime-path p3d-bsp-3-data "./test-data/p3d-bsp-3.dat")
(define (do-plot-3 output-fn)
  (output-fn (list (isosurface3d (λ (x y z) (+ x y z)) 0 -1 1 -1 1 -1 1
                                 #:samples 2
                                 #:line-width 2)
                   (isosurface3d (λ (x y z) x) 0 #:samples 2
                                 #:color "red"
                                 #:line-width 2)
                   (isosurface3d (λ (x y z) (+ x (- y) z)) 0
                                 #:samples 2
                                 #:line-width 2)
                   (parametric3d (λ (t)
                                   (list (* 0.75 (cos (* 5 pi t)))
                                         (* 0.75 (sin (* 5 pi t)))
                                         t))
                                 -1 1 #:width 2 #:color 2))))

(define-runtime-path p3d-bsp-4-data "./test-data/p3d-bsp-4.dat")
(define (do-plot-4 output-fn)
  (random-seed 42)      ; make `sample` and `uniform-dist` repeatable in tests
  (let* ([xs  (sample (uniform-dist -1 1) 10000)]
         [ys  (sample (uniform-dist -1 1) 10000)]
         [zs  (sample (uniform-dist -1 1) 10000)]
         [xyzs  (map list xs ys zs)])
    (output-fn (list (isosurface3d (λ (x y z) (+ x y z)) 0 -1 1 -1 1 -1 1
                                   #:samples 2
                                   #:line-width 2)
                     (isosurface3d (λ (x y z) x) 0 #:samples 2
                                   #:color "red"
                                   #:line-width 2)
                     (isosurface3d (λ (x y z) (+ x (- y) z)) 0
                                   #:samples 2
                                   #:line-width 2)
                     (points3d xyzs #:sym 'dot)))))

(define-runtime-path p3d-bsp-5-data "./test-data/p3d-bsp-5.dat")
(define (do-plot-5 output-fn)
  (output-fn
   (list (contour-intervals3d
          (λ (x y)
            (* x (+ 0.1 y)))
          -1 1 -1 1
          #:samples 41
          #:alphas '(0.85)
          ;#:alpha 0.75
          ;#:line-width 2
          ;#:line-widths '(2)
          ;#:line-styles '(transparent)
          #:contour-widths '(2)
          ;#:color 1
          ;#:label ""
          )
         
         (surface3d
          (λ (x y)
            (* (- (* (flnormal-pdf 0.0 0.2 (fl x) #f)
                     (flnormal-pdf 0.0 0.2 (fl y) #f))
                  0.7)
               0.4))
          -1 1 -1 1
          #:samples 40
          ;#:alphas '(0.75)
          #:alpha 0.95
          #:color "plum"
          #:line-color 6
          ;#:line-style 'transparent
          ;#:line-width 2
          ))
   #:x-min -1 #:x-max 1
   #:y-min -1 #:y-max 1
   ;#:out-file "test.pdf"
   ))

(define (f2 x y)
  (let ([x  (fl x)] [y  (fl y)])
    (- (sqrt (+ (abs y) (abs x))))))

(define-runtime-path p3d-bsp-6-data "./test-data/p3d-bsp-6.dat")
(define (do-plot-6 output-fn)
  (output-fn (list (surface3d * -1 1 -1 1 #:samples 6 #:alpha 0.75 #:color 1)
                   (surface3d (λ (x y) (+ 0.1 (* x y))) -1 1 -1 1 #:samples 6 #:alpha 0.75 #:color 2)
                   (surface3d (λ (x y) (+ 0.2 (* x y))) -1 1 -1 1 #:samples 6 #:alpha 0.75 #:color 3
                              #:line-width 2)
                   )))

(define-runtime-path p3d-bsp-7-data "./test-data/p3d-bsp-7.dat")
(define (do-plot-7 output-fn)
  (output-fn (list
              (isosurface3d (λ (x y z) (+ (- 1 x) (- 1 y) (- z 1.5))) 0
                            #:alpha 0.85 #:color 2 #:line-color 2
                            #:samples 4)
              (discrete-histogram3d (list (vector 'a 'a 1)
                                          (vector 'a 'b 2)
                                          (vector 'b 'b 3))
                                    #:color 4 #:line-color 4 #:line-width 2
                                    #:alpha 0.65))))

(define-runtime-path p3d-bsp-8-data "./test-data/p3d-bsp-8.dat")
(define (do-plot-8 output-fn)
  (output-fn (list
              ;(isosurface3d (λ (x y z) (+ x y z)) 0 #:samples 2)
              (surface3d * -1 1 -1 1)
              (vector-field3d (λ (x y z) (vector x z y))
                              #:line-width 2))))

(define (f x y) (* (sin x) (cos y)))
(define-runtime-path p3d-bsp-9-data "./test-data/p3d-bsp-9.dat")
(define (do-plot-9 output-fn)
  (output-fn (list (contour-intervals3d f -3 3 -3 3)
                   (point-label3d (list -1 1 (f -1 1))))))

(define-runtime-path p3d-bsp-10-data "./test-data/p3d-bsp-10.dat")
(define (do-plot-10 output-fn)
  (parameterize ([plot-x-transform  (hand-drawn-transform 50)]
                 [plot-y-transform  (hand-drawn-transform 50)]
                 [plot-z-transform  (hand-drawn-transform 50)]
                 )
    (output-fn (contour-intervals3d (λ (x y) (- (sqr x) (sqr y)))
                                    -1 1 -1 1 #:samples 9
                                    #:contour-widths '(2)
                                    #:line-widths '(2)))))

(define (saddle x y) (- (sqr x) (sqr y)))
(define-runtime-path p3d-bsp-11-data "./test-data/p3d-bsp-11.dat")
(define (do-plot-11 output-fn)
  (output-fn (list (surface3d saddle -1 1 -1 1)
                   (isoline3d saddle 1/4 #:width 2 #:style 'long-dash))))


(define plot3d-bsp-tests
  (test-suite
   "plot3d-bsp-tests"
   (test-case "test case 1" (check-draw-steps-3d do-plot-1 p3d-bsp-1-data))
   (test-case "test case 2" (check-draw-steps-3d do-plot-2 p3d-bsp-2-data))
   (test-case "test case 3" (check-draw-steps-3d do-plot-3 p3d-bsp-3-data))
   (test-case "test case 4" (check-draw-steps-3d do-plot-4 p3d-bsp-4-data))
   (test-case "test case 5" (check-draw-steps-3d do-plot-5 p3d-bsp-5-data))
   (test-case "test case 6" (check-draw-steps-3d do-plot-6 p3d-bsp-6-data))
   (test-case "test case 7" (check-draw-steps-3d do-plot-7 p3d-bsp-7-data))
   (test-case "test case 8" (check-draw-steps-3d do-plot-8 p3d-bsp-8-data))
   (test-case "test case 9" (check-draw-steps-3d do-plot-9 p3d-bsp-9-data))
   (test-case "test case 10" (check-draw-steps-3d do-plot-10 p3d-bsp-10-data))
   (test-case "test case 11" (check-draw-steps-3d do-plot-11 p3d-bsp-11-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests plot3d-bsp-tests))
