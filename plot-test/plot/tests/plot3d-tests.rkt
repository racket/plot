#lang racket

(require plot plot/utils math/flonum
         racket/runtime-path
         rackunit
         "helpers.rkt")

;(plot-new-window? #t)

(define-runtime-path p3d-1-data "./test-data/p3d-1.dat")
(define (do-plot-1 output-fn)
  (output-fn empty #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1
              #:title "this plot should be empty"))

(define-runtime-path p3d-2-data "./test-data/p3d-2.dat")
(define (do-plot-2 output-fn)
  (output-fn (points3d empty) #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1
              #:title "this plot should be empty"))

(define-runtime-path p3d-3-data "./test-data/p3d-3.dat")
(define (do-plot-3 output-fn)
  (output-fn (rectangles3d (list (list (ivl 0 1) (ivl 0 1) (ivl 0 1))))
             #:x-min 2 #:x-max 3
             #:title "this plot should be empty"))

(define-runtime-path p3d-4-data "./test-data/p3d-4.dat")
(define (do-plot-4 output-fn)
  (parameterize ([plot-background  "black"]
                 [plot-foreground  "white"]
                 [plot-background-alpha  1/2]
                 [plot-foreground-alpha  1/2])
    (output-fn (surface3d (λ (x y) (* (sin x) (sin y))) -2 2 -2 2 #:label "z = trig(x,y)"))))

(define-runtime-path p3d-5-data "./test-data/p3d-5.dat")
(define (do-plot-5 output-fn)
  (output-fn (points3d '(#(0.1 0.6 0.3)))
             #:x-min 0 #:x-max 1 #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1))

(define-runtime-path p3d-6-data "./test-data/p3d-6.dat")
(define (do-plot-6 output-fn)
  (output-fn (vector-field3d (λ (x y z) (vector x z y)) -2 2 -2 2 -2 2
                             #:line-width 3)))

(define-runtime-path p3d-7-data "./test-data/p3d-7.dat")
(define (do-plot-7 output-fn)
  (define x-ivls (bounds->intervals (linear-seq 2 8 10)))
  (define y-ivls (bounds->intervals (linear-seq -5 5 10)))
  (define x-mids (linear-seq 2 8 9 #:start? #f #:end? #f))
  (define y-mids (linear-seq -5 5 9 #:start? #f #:end? #f))
  (output-fn (rectangles3d (append*
                            (for/list ([y-ivl  (in-list y-ivls)] [y  (in-list y-mids)])
                              (for/list ([x-ivl  (in-list x-ivls)] [x  (in-list x-mids)])
                                (vector x-ivl y-ivl (ivl 0 (exp (* -1/2 (+ (sqr (- x 5)) (sqr y)))))))))
                           #:alpha 3/4
                           #:label "Approximate 2D Normal")))

(define-runtime-path p3d-8-data "./test-data/p3d-8.dat")
(define (do-plot-8 output-fn)
  (output-fn (discrete-histogram3d (list (vector 'a 'a 1)
                                         (vector 'a 'b 2)
                                         (vector 'b 'b 3))
                                   #:label "Missing (b,a)"
                                   #:color 4 #:line-color 4)))

(define-runtime-path p3d-9-data "./test-data/p3d-9.dat")
(define (do-plot-9 output-fn)
  (random-seed 42)                      ; make test reproducible
  (define c1s #(a b c d e))
  (define c2s #(1 2 3 4 5))
  (define cat-vals (build-list 15 (λ (n) (vector (vector-ref c1s (random 5))
                                                 (vector-ref c2s (random 5))
                                                 n))))
  (output-fn (discrete-histogram3d cat-vals)))

(define-runtime-path p3d-10-data "./test-data/p3d-10.dat")
(define (do-plot-10 output-fn)
  (output-fn (stacked-histogram3d '(#(a a (1 1 1)) #(a b (1.5 3)) #(b b ()) #(b a (1/2)))
                                  #:labels '("Red" #f "Blue") #:alphas '(2/3))))

(define-runtime-path p3d-11-data "./test-data/p3d-11.dat")
(define (do-plot-11 output-fn)
  (output-fn (stacked-histogram3d '(#(a a (1 1 1)) #(a b (1.5 3)) #(b b ()) #(b a (1/2)))
                                  #:labels '("Red" #f "Blue") #:alphas '(2/3)
                                  #:add-x-ticks? #f #:add-y-ticks? #f)))

(define-runtime-path p3d-12-data "./test-data/p3d-12.dat")
(define (do-plot-12 output-fn)
  (output-fn (surface3d + 0 10 0 1) #:angle 10 #:z-label "z axis"))

(define-runtime-path p3d-13-data "./test-data/p3d-13.dat")
(define (do-plot-13 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-y-transform  log-transform])
    (output-fn (surface3d + .01 3 .01 1))))

;(plot-x-transform cbrt-transform)
;(plot-y-transform cbrt-transform)
;(plot-z-transform cbrt-transform)

(define-runtime-path p3d-14-data "./test-data/p3d-14.dat")
(define (do-plot-14 output-fn)
  (output-fn (surface3d (λ (x y) (+ (/ 1.0 (fl x))
                                    (/ 1.0 (fl y))))
                        -2 2 -2 2
                        #:color '(255 128 128)
                        #:line-color '(255 128 128)
                        #:line-width 1.5
                        #:label "Inverse")
             #:title "Here it is!"
             #:x-label "WannaHockaLoogi"
             #:y-label "An Impossibly Long Y Axis Label"
             #:angle 330 #:altitude 0))

(define-runtime-path p3d-15-data "./test-data/p3d-15.dat")
(define (do-plot-15 output-fn)
  (output-fn (surface3d (λ (x y)
                          (+ (/ (+ (abs x) 0.01))
                             (/ (+ (abs y) 0.01))))
                        -4 4 -4 4 #:color '(128 128 255)
                        #:label "Z sort test polygons")
             #:angle 330 #:altitude 41
             #:z-label #f #:y-label #f #:x-label #f))

(define-runtime-path p3d-16-data "./test-data/p3d-16.dat")
(define (do-plot-16 output-fn)
  (random-seed 42)                      ; make test reproducible
  (define xs (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (define ys (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (define zs (build-list 200 (λ (n) (/ 1 (- (random) 0.5)))))
  (output-fn (points3d (map vector xs ys zs)
                       #:x-min -20 #:x-max 20
                       #:y-min -20 #:y-max 20
                       #:z-min -20 #:z-max 20
                       #:label "Widget Locations")
             #:angle 15 #:title "Random Points"))

(define-runtime-path p3d-17-data "./test-data/p3d-17.dat")
(define (do-plot-17 output-fn)
  (random-seed 42)                      ; make test reproducible
  (define xs (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (define ys (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (define zs (build-list 10000 (λ (n) (/ 1 (- (random) 0.5)))))
  (output-fn (points3d (map vector xs ys zs)
                       #:x-min -20 #:x-max 20
                       #:y-min -20 #:y-max 20
                       #:z-min -20 #:z-max 20
                       #:color "blue" #:sym 'dot ;#:size 10
                       #:alpha 0.5)
             #:angle 30 #:altitude 30
             #:title "A Bunch of Random Points Concentrated at the Origin"
             #:x-label "x" #:y-label "y" #:z-label "z"))

;; tests line clipping: should look like a sphere with six poles chopped off
(define-runtime-path p3d-18-data "./test-data/p3d-18.dat")
(define (do-plot-18 output-fn)
  (output-fn (parametric3d (λ (t)
                             (vector (* (cos (* 80 t)) (cos t))
                                     (* (sin (* 80 t)) (cos t))
                                     (sin t)))
                           (- pi) pi
                           #:x-min -0.8 #:x-max 0.8
                           #:y-min -0.8 #:y-max 0.8
                           #:z-min -0.8 #:z-max 0.8
                           #:color "blue" #:width 1/2 #:style 'long-dash
                           #:samples 3000 #:alpha 0.5
                           #:label "Sphere")
             #:altitude 22
             #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(define-runtime-path p3d-19-data "./test-data/p3d-19.dat")
(define (do-plot-19 output-fn)
  (output-fn (surface3d (λ (x y) (+ x y)) -0.81 0.81 -0.81 0.81
                        #:line-color '(0 0 255) #:line-width 1 #:line-style 'dot)
             #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(define-runtime-path p3d-20-data "./test-data/p3d-20.dat")
(define (do-plot-20 output-fn)
  (random-seed 42)                      ; make test reproducible
  (define xs (build-list 200 (λ (n) (* 2 (- (random) 0.5)))))
  (define ys (build-list 200 (λ (n) (* 2 (- (random) 0.5)))))
  (define zs (build-list 200 (λ (n) (* 2 (- (random) 0.5)))))
  (output-fn (list (surface3d (λ (x y) (+ x y)) -0.81 0.81 -0.81 0.81
                              #:line-color '(0 0 255) #:line-width 1
                              #:line-style 'dot)
                   (points3d (map vector xs ys zs)))
             #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1 #:z-min -1 #:z-max 1))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (- (sqr x) (sqr y)))

(define (f2 x y)
  (- (sqrt (+ (abs y) (abs x)))))

(define (f3 x y)
  (define d (* 2 pi (+ (abs x) (abs y))))
  (+ (* 1/8 (cos d)) (- (sqr x) (sqr y))))

(define (f4 x y)
  (imag-part (log (make-rectangular (fl x) (fl y)))))

(define (f5 x y)
  (+ (* 1.1 (norm -1.5 -1.5 x y))
     (* 2 (norm 1 1 x y))
     (* 1.3 (norm 2 -2 x y))))

(define (f6 x y)
  (define d (sqrt (+ (sqr x) (sqr y))))
  (if (d . < . 1)
      (sqrt (- 1 (sqr d)))
      0))

(define-runtime-path p3d-21-data "./test-data/p3d-21.dat")
(define (do-plot-21 output-fn)
  (parameterize ([plot3d-diffuse-light? #f]
                 [plot3d-specular-light? #f])
    (output-fn (surface3d f5 -5 5 -5 5 #:style 'transparent))))

(define-runtime-path p3d-22-data "./test-data/p3d-22.dat")
(define (do-plot-22 output-fn)
  (output-fn (contours3d f5 -4 4 -4 4 #:colors '(0) #:label "z")))

(define-runtime-path p3d-23-data "./test-data/p3d-23.dat")
(define (do-plot-23 output-fn)
  (output-fn (contour-intervals3d f5 -4 4 -4 4 #:label "z")))

(define-runtime-path p3d-24-data "./test-data/p3d-24.dat")
(define (do-plot-24 output-fn)
  (output-fn (contour-intervals3d
              (λ (x y)
                (define z (- x y))
                (cond [(< z -1) -1]
                      [(> z 1)   1]
                      [else      z]))
              -2 2 -2 2)))

(define-runtime-path p3d-25-data "./test-data/p3d-25.dat")
(define (do-plot-25 output-fn)
  (output-fn (contour-intervals3d (λ (x y) (+ x y)))
             #:x-min #e100000000000000.0 #:x-max #e100000000000000.1
             #:y-min #e100000000000000.0 #:y-max #e100000000000000.1
             #:width 500))

(define-runtime-path p3d-26-data "./test-data/p3d-26.dat")
(define (do-plot-26 output-fn)
  (output-fn (list (surface3d f5 0 4 -4 4 #:color '(128 255 160) #:alpha 0.5
                              #:label "x pos.")
                   (contour-intervals3d f5 -4 0 -4 4
                                        #:colors '(0 1 5)
                                        #:line-colors '(0 4 2)
                                        #:line-widths '(1.5) #:line-styles '(dot)
                                        #:contour-colors '(0)
                                        #:contour-widths '(0)
                                        #:contour-styles '(transparent)
                                        #:alphas '(0.75)
                                        #:label "x neg."))
             #:z-min 0.25 #:z-max 1.1
             #:legend-anchor 'top))

(define-runtime-path p3d-27-data "./test-data/p3d-27.dat")
(define (do-plot-27 output-fn)
  (parameterize ([plot3d-samples 81])
    (output-fn (contour-intervals3d
                f5 -4 4 -4 4 #:label "z"
                #:line-styles '(transparent)))))

(define-runtime-path p3d-28-data "./test-data/p3d-28.dat")
(define (do-plot-28 output-fn)
  (output-fn (list (contours3d f5 -4 4 -4 4)
                   (contour-intervals3d f5 -2.5 2.5 -2.5 2.5
                                        #:z-min 0.25 #:z-max 1.5 #:label "z"))))

(define-runtime-path p3d-29-data "./test-data/p3d-29.dat")
(define (do-plot-29 output-fn)
  (output-fn (contour-intervals3d f5 -3 3 -3 3
                                  #:colors '((255 128 128) (128 128 255)))))

(define-runtime-path p3d-30-data "./test-data/p3d-30.dat")
(define (do-plot-30 output-fn)
  (output-fn (list (surface3d f4 -4 4 -4 4 #:color '(255 224 0))
                   (contours3d f4 -4 4 -4 4))
             #:angle -30))

(define-runtime-path p3d-31-data "./test-data/p3d-31.dat")
(define (do-plot-31 output-fn)
  (output-fn (contour-intervals3d f1 -4 4 -4 4)))

(define-runtime-path p3d-32-data "./test-data/p3d-32.dat")
(define (do-plot-32 output-fn)
  (parameterize ([plot3d-samples  101])
    (output-fn (contour-intervals3d f2 -2 2 -2 2 #:levels 10
                                    #:line-styles '(transparent)
                                    #:contour-styles '(long-dash)
                                    #:alphas '(1 2/3))
               #:altitude 20)))

(define-runtime-path p3d-33-data "./test-data/p3d-33.dat")
(define (do-plot-33 output-fn)
  (output-fn (contour-intervals3d (λ (x y) (- (sqr x) (sqr y))) -min.0 +min.0 -min.0 +min.0)))

(define-runtime-path p3d-34-data "./test-data/p3d-34.dat")
(define (do-plot-34 output-fn)
  (define (f x y) (* (sin x) (cos y)))
  (output-fn (list (contour-intervals3d f -3 3 -3 3)
                   (point-label3d (list -1 1 (f -1 1))))))

(define-runtime-path p3d-35-data "./test-data/p3d-35.dat")
(define (do-plot-35 output-fn)
  (output-fn (vector-field3d (lambda (x y z) (list 1 1 0)) 0 10 0 10 0 10)))

(define plot3d-tests
  (test-suite
   "plot3d-tests"
   (test-case "test case 1" (check-draw-steps-3d do-plot-1 p3d-1-data))
   (test-case "test case 2" (check-draw-steps-3d do-plot-2 p3d-2-data))
   (test-case "test case 3" (check-draw-steps-3d do-plot-3 p3d-3-data))
   (test-case "test case 4" (check-draw-steps-3d do-plot-4 p3d-4-data))
   (test-case "test case 5" (check-draw-steps-3d do-plot-5 p3d-5-data))
   (test-case "test case 6" (check-draw-steps-3d do-plot-6 p3d-6-data))
   (test-case "test case 7" (check-draw-steps-3d do-plot-7 p3d-7-data))
   (test-case "test case 8" (check-draw-steps-3d do-plot-8 p3d-8-data))
   (test-case "test case 9" (check-draw-steps-3d do-plot-9 p3d-9-data))
   (test-case "test case 10" (check-draw-steps-3d do-plot-10 p3d-10-data))
   (test-case "test case 11" (check-draw-steps-3d do-plot-11 p3d-11-data))
   (test-case "test case 12" (check-draw-steps-3d do-plot-12 p3d-12-data))
   (test-case "test case 13" (check-draw-steps-3d do-plot-13 p3d-13-data))
   (test-case "test case 14" (check-draw-steps-3d do-plot-14 p3d-14-data))
   (test-case "test case 15" (check-draw-steps-3d do-plot-15 p3d-15-data))
   (test-case "test case 16" (check-draw-steps-3d do-plot-16 p3d-16-data))
   (test-case "test case 17" (check-draw-steps-3d do-plot-17 p3d-17-data))
   (test-case "test case 18" (check-draw-steps-3d do-plot-18 p3d-18-data))
   (test-case "test case 19" (check-draw-steps-3d do-plot-19 p3d-19-data))
   (test-case "test case 20" (check-draw-steps-3d do-plot-20 p3d-20-data))
   (test-case "test case 21" (check-draw-steps-3d do-plot-21 p3d-21-data))
   (test-case "test case 22" (check-draw-steps-3d do-plot-22 p3d-22-data))
   (test-case "test case 23" (check-draw-steps-3d do-plot-23 p3d-23-data))
   (test-case "test case 24" (check-draw-steps-3d do-plot-24 p3d-24-data))
   (test-case "test case 25" (check-draw-steps-3d do-plot-25 p3d-25-data))
   (test-case "test case 26" (check-draw-steps-3d do-plot-26 p3d-26-data))
   (test-case "test case 27" (check-draw-steps-3d do-plot-27 p3d-27-data))
   (test-case "test case 28" (check-draw-steps-3d do-plot-28 p3d-28-data))
   (test-case "test case 29" (check-draw-steps-3d do-plot-29 p3d-29-data))
   (test-case "test case 30" (check-draw-steps-3d do-plot-30 p3d-30-data))
   (test-case "test case 31" (check-draw-steps-3d do-plot-31 p3d-31-data))
   (test-case "test case 32" (check-draw-steps-3d do-plot-32 p3d-32-data))
   (test-case "test case 33" (check-draw-steps-3d do-plot-33 p3d-33-data))
   (test-case "test case 34" (check-draw-steps-3d do-plot-34 p3d-34-data))
   (test-case "test case 35" (check-draw-steps-3d do-plot-35 p3d-35-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests plot3d-tests))
