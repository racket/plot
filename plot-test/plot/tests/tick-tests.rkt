#lang racket

(require plot plot/utils
         racket/runtime-path
         rackunit
         "helpers.rkt")

(plot-font-family 'swiss)

(define (get-isosurface-ticks z-min z-max)
  (cond [(z-min . >= . z-max)  empty]
        [else
         (map pre-tick-value
              (filter pre-tick-major?
                      (contour-ticks (plot-d-ticks) z-min z-max 'auto #f)))]))

;; try to verify that we always get 3-5 isosurfaces from the isosurfaces3d renderer
(define-runtime-path tt-1-data "./test-data/tt-1.dat")
(define (do-plot-1 output-fn)
  (output-fn (function (λ (x)
                         (let ([ts  (get-isosurface-ticks 1/10 (+ 1/10 x))])
                           (if (empty? ts) +nan.0 (length ts))))
                       #:samples 10000)
             #:x-min 0 #:x-max 10
             #:x-label "bounds size (min = 1/10)"
             #:y-label "number of ticks"))

;; try to verify that we always get 3-5 isosurfaces from the isosurfaces3d renderer
(define-runtime-path tt-2-data "./test-data/tt-2.dat")
(define (do-plot-2 output-fn)
  (output-fn (contour-intervals3d (λ (x y)
                                    (let ([ts  (get-isosurface-ticks x (+ x y))])
                                      (if (empty? ts) +nan.0 (length ts))))
                                  #:samples 101 #:line-styles '(transparent))
             #:x-min 0 #:x-max 10 #:y-min 0 #:y-max 10
             #:x-label "bounds min" #:y-label "bounds size"
             #:z-label "number of ticks"))

(define-runtime-path tt-3-data "./test-data/tt-3.dat")
(define (do-plot-3 output-fn)
  (output-fn (contour-intervals (λ (x y)
                                  (let ([ts  (get-isosurface-ticks x (+ x y))])
                                    (if (empty? ts) +nan.0 (length ts))))
                                #:samples 101)
             #:x-min 0 #:x-max 10 #:y-min 0 #:y-max 10
             #:x-label "bounds min" #:y-label "bounds size"))

(define-runtime-path tt-4-data "./test-data/tt-4.dat")
(define (do-plot-4 output-fn)
  (output-fn (function (λ (x) (count pre-tick-major? (ticks-generate (linear-ticks) 0 x)))
                       #e0.1 10)))
(define-runtime-path tt-5-data "./test-data/tt-5.dat")
(define (do-plot-5 output-fn)
  (output-fn (function (λ (x) (count pre-tick-major? (ticks-generate (linear-ticks #:number 40) 0 x)))
                       1 100)))

(define-runtime-path tt-6-data "./test-data/tt-6.dat")
(define (do-plot-6 output-fn)
  (parameterize ([plot-x-ticks  (linear-ticks #:base 2 #:divisors '(1 2))]
                 #;[plot-y-ticks  (linear-ticks #:base (* 1 2 3 4 5) #:divisors '(1 2 3 4 5))])
    (output-fn (function cos 0.013 2.1176))))

(define-runtime-path tt-7-data "./test-data/tt-7.dat")
(define (do-plot-7 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-x-ticks  (ticks (log-ticks-layout)
                                       (fraction-ticks-format))]
                 [plot-y-ticks  (fraction-ticks)])
    (output-fn (function (λ (x) (+ 1 (cos x))) 0.0001 12))))

(define-runtime-path tt-8-data "./test-data/tt-8.dat")
(define (do-plot-8 output-fn)
  (parameterize ([plot-x-ticks  (date-ticks #:number 3)]
                 [plot-y-ticks  (currency-ticks)])
    (output-fn (function values -1 1))))

(define-runtime-path tt-9-data "./test-data/tt-9.dat")
(define (do-plot-9 output-fn)
  (parameterize* ([currency-ticks-formats uk-currency-formats]
                  [currency-ticks-scales uk-currency-scales]
                  [plot-x-ticks  (date-ticks)]
                  [plot-y-ticks  (currency-ticks #:kind 'GBP)])
    (output-fn (function values 101232512 2321236192))))

(define-runtime-path tt-10-data "./test-data/tt-10.dat")
(define (do-plot-10 output-fn)
  (parameterize ([plot-x-ticks  (currency-ticks #:kind 'EUR
                                                #:scales eu-currency-scales
                                                #:formats eu-currency-formats)]
                 [plot-y-ticks  (currency-ticks)])
    (output-fn (function (λ (x) (* x 1.377)) 8000000 10000000)
               #:title "EUR-USD Conversion, 2011-10-13"
               #:x-label "Euros"
               #:y-label "Dollars")))

(define-runtime-path tt-11-data "./test-data/tt-11.dat")
(define (do-plot-11 output-fn)
  (parameterize ([plot-x-ticks  no-ticks])
    (output-fn (function sin -1 4))))

(define-runtime-path tt-12-data "./test-data/tt-12.dat")
(define (do-plot-12 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-y-transform  log-transform]
                 [plot-x-ticks      (log-ticks #:base 10)]
                 [plot-y-ticks      (log-ticks #:base 2)])
    (output-fn (function values 0.1 10))))

(define-runtime-path tt-13-data "./test-data/tt-13.dat")
(define (do-plot-13 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-y-transform  (stretch-transform -1 1 4)]
                 [plot-x-ticks      (ticks (linear-ticks-layout)
                                           (log-ticks-format #:base 10))]
                 [plot-y-ticks      (ticks (linear-ticks-layout)
                                           (currency-ticks-format #:kind 'USD))])
    (output-fn (function log 0.1 10))))

(define-runtime-path tt-14-data "./test-data/tt-14.dat")
(define (do-plot-14 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-x-ticks      (log-ticks #:base 10)])
    (output-fn (function values 10000000000000 1000000000000000))))

(define-runtime-path tt-15-data "./test-data/tt-15.dat")
(define (do-plot-15 output-fn)
  (output-fn (polar-axes) #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1))

(define-runtime-path tt-16-data "./test-data/tt-16.dat")
(define (do-plot-16 output-fn)
  (output-fn (polar-axes) #:x-min 0 #:x-max 3 #:y-min 0 #:y-max 3))

(define-runtime-path tt-17-data "./test-data/tt-17.dat")
(define (do-plot-17 output-fn)
  (output-fn (polar-axes) #:x-min 1 #:x-max 4 #:y-min 1 #:y-max 4))

(define-runtime-path tt-18-data "./test-data/tt-18.dat")
(define (do-plot-18 output-fn)
  (output-fn (polar-axes #:number 12) #:x-min 10 #:x-max 12 #:y-min 10 #:y-max 12))

(define-runtime-path tt-31-data "./test-data/tt-31.dat")
(define-runtime-path tt-32-data "./test-data/tt-32.dat")
(define-runtime-path tt-33-data "./test-data/tt-33.dat")
(define-runtime-path tt-34-data "./test-data/tt-34.dat")

(define-values (do-plot-31 do-plot-32 do-plot-33 do-plot-34)
  (parameterize ([plot-z-transform  log-transform]
                 [plot-z-ticks      (log-ticks)]
                 [contour-samples   (plot3d-samples)])
    (values
     (lambda (output-fn)
       (output-fn (contours (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z")))
     (lambda (output-fn)
       (output-fn (contour-intervals (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z")))
     (lambda (output-fn)
       (output-fn (contours3d (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z")))
     (lambda (output-fn)
       (output-fn (contour-intervals3d (λ (x y) (exp (- (+ (sqr x) (sqr y))))) -2 2 -2 2 #:label "z"))))))


(define-runtime-path tt-19-data "./test-data/tt-19.dat")
(define (do-plot-19 output-fn)
  (output-fn (contours (λ (x y) (* 1/2 (+ (sqr x) (sqr y)))) -1 1 -1 1 #:label "z")))
(define-runtime-path tt-20-data "./test-data/tt-20.dat")
(define (do-plot-20 output-fn)
  (output-fn (contours3d (λ (x y) (* 1/2 (+ (sqr x) (sqr y)))) -1 1 -1 1 #:label "z")))

(define-runtime-path tt-21-data "./test-data/tt-21.dat")
(define (do-plot-21 output-fn)
  (parameterize ([plot-y-ticks  (ticks-scale (plot-y-ticks) (linear-scale 2 1))])
    (output-fn (list (function sqr -2 2)
                     (function sin -4 4)))))

(define exp-scale (invertible-function exp log))

(define-runtime-path tt-22-data "./test-data/tt-22.dat")
(define (do-plot-22 output-fn)
  (parameterize ([plot-y-ticks  (ticks-scale (log-ticks) exp-scale)])
    (output-fn (function values -10 10))))

(define-runtime-path tt-23-data "./test-data/tt-23.dat")
(define (do-plot-23 output-fn)
  (parameterize ([plot-y-ticks  (ticks-add (ticks no-ticks-layout (linear-ticks-format))
                                           '(1/3 2/3))])
    (output-fn (function sin -4 4))))

(define-runtime-path tt-24-data "./test-data/tt-24.dat")
(define (do-plot-24 output-fn)
  (output-fn (list (function sin -4 4)
                   (points '(#(-3.75 -1/4)) #:size 10)
                   (x-ticks (list (tick 1.5 #t "3/2") (tick 3 #t "Three")))
                   (y-ticks (list (tick 1/4 #t "1/4") (tick -1/4 #f ""))))))

(define-runtime-path tt-25-data "./test-data/tt-25.dat")
(define (do-plot-25 output-fn)
  (parameterize ([plot-z-ticks  (linear-ticks #:number 5)])
    (output-fn (list (surface3d (λ (x y) (* 2 (+ (sin x) (cos y)))) -4 4 -4 4 #:alpha 1/2)
                       (x-ticks (list (tick 1.5 #t "3/2") (tick 3 #t "Three")))
                       (y-ticks (list (tick 1/3 #t "1/3") (tick -1/3 #f "1/3")))
                       (z-ticks (list (tick pi #f "π") (tick (- pi) #t "-π")))))))

(define-runtime-path tt-35-data "./test-data/tt-35.dat")
(define-runtime-path tt-36-data "./test-data/tt-36.dat")
(define-values (do-plot-35 do-plot-36)
  (parameterize ([plot-title  "Money for time in a sine wave"]
                 [plot-x-far-ticks  (time-ticks)]
                 [plot-y-ticks  (currency-ticks #:kind 'USD)]
                 [plot-y-far-ticks  (ticks-scale (currency-ticks #:kind 'EUR) (linear-scale 1.47))]
                 [plot-x-label  #f]
                 [plot-y-label  #f])
    (values
     (lambda (output-fn)
       (parameterize ([plot-x-axis?  #f]
                      [plot-x-far-axis?  #f])
         (output-fn (list (function sin -4 4)
                          (x-axis -0.25 #:ticks? #f #:labels? #t)
                          (x-axis 0.25 #:ticks? #t #:labels? #t #:far? #t)))))

     (lambda (output-fn)
       (parameterize ([plot-y-axis?  #f]
                      [plot-y-far-axis?  #f])
         (output-fn (list (function sin -4 4)
                          (y-axis -1 #:ticks? #f #:labels? #t)
                          (y-axis 1 #:ticks? #t #:labels? #t #:far? #t))))))))

(define-runtime-path tt-26-data "./test-data/tt-26.dat")
(define (do-plot-26 output-fn)
  (parameterize ([plot-y-ticks  (fraction-ticks)])
    (output-fn (function sin (- pi) pi))))

(define-runtime-path tt-37-data "./test-data/tt-37.dat")
(define (do-plot-37 output-fn)
  (parameterize ([plot-x-far-label  "x far axis"]
                 [plot-x-ticks      (linear-ticks #:number 10)]
                 [plot-y-far-label  "y far axis"]
                 [plot-y-far-ticks  (date-ticks)]
                 [plot-z-label  "z axis"]
                 [plot-z-far-label  "z far axis"]
                 [plot-z-far-ticks  (currency-ticks #:number 5)])
    (output-fn (surface3d (λ (x y) (+ (sin x) (cos y))) -2 2 -2 2 #:alpha 1/2)
               #:angle 60 #:altitude 35)))

(define-runtime-path tt-27-data "./test-data/tt-27.dat")
(define (do-plot-27 output-fn)
  (parameterize ([plot-title  "Saddle"]
                 [plot-x-axis?  #f]
                 [plot-y-axis?  #f]
                 [plot-z-axis?  #f]
                 [plot-x-far-axis?  #f]
                 [plot-y-far-axis?  #f]
                 [plot-z-far-axis?  #f]
                 [plot-x-label  #f]
                 [plot-y-label  #f]
                 [plot-z-label  #f]
                 [plot-x-far-label  #f]
                 [plot-y-far-label  #f]
                 [plot-z-far-label  #f])
    (output-fn (contour-intervals3d (λ (x y) (- (sqr x) (sqr y))) -2 2 -2 2
                                    #:label "z"))))

(define-runtime-path tt-38-data "./test-data/tt-38.dat")
(define-runtime-path tt-39-data "./test-data/tt-39.dat")
(define-values (do-plot-38 do-plot-39)
  (parameterize ([plot-decorations?  #f])
    (values
     (lambda (output-fn)
       (output-fn (function sin -4 4) #:title "Hello"))
     (lambda (output-fn)
       (output-fn (contour-intervals3d (λ (x y) (- (sqr x) (sqr y))) -2 2 -2 2))))))

(define-runtime-path tt-28-data "./test-data/tt-28.dat")
(define (do-plot-28 output-fn)
  (define ((degrees-ticks-format suffix) x-min x-max ts)
    (map (λ (label) (format "~a\ub0~a" label suffix))
         ((linear-ticks-format) x-min x-max ts)))
  
  (define C-ticks (ticks (linear-ticks-layout) (degrees-ticks-format 'C)))
  
  (define F/C-ticks (ticks-scale
                     (ticks (linear-ticks-layout) (degrees-ticks-format 'F))
                     (linear-scale 9/5 32)))
  
  (define data (list #(0 0) #(15 0.6) #(30 9.5) #(45 10.0) #(60 16.6)
                     #(75 41.6) #(90 42.7) #(105 65.5) #(120 78.9)
                     #(135 78.9) #(150 131.1) #(165 151.1) #(180 176.2)))
  
  (define (temp/time-trend x) (/ (sqr x) 180))

  (define above-data (filter (λ (v) (match-let ([(vector x y)  v])
                                      (y . > . (temp/time-trend x))))
                             data))

  (parameterize ([plot-x-ticks      (time-ticks)]
                 [plot-y-ticks      C-ticks]
                 [plot-y-far-ticks  F/C-ticks])
    (output-fn (list (function temp/time-trend 0 180 #:style 'long-dash #:color 3
                               #:label "Trend")
                     (lines data #:color 2 #:width 2)
                     (points data #:color 2 #:line-width 2 #:fill-color 0 #:sym 'fullcircle
                             #:label "Measurement")
                     (map (λ (d) (point-label d #:anchor 'bottom #:point-color 2 #:point-size 7))
                          above-data))
               #:y-min -25 #:x-label "Time" #:y-label "Temp."
               #:title "Temp./Time With Applied Heat (Measurement and Trend)")))

(define-runtime-path tt-29-data "./test-data/tt-29.dat")
(define (do-plot-29 output-fn)
  (parameterize ([plot-x-ticks  (fraction-ticks)]
                 [plot-y-ticks  (currency-ticks)])
    (output-fn (list (function sin -4 4)
                     (function-label sin 1/3)))))

(define-runtime-path tt-30-data "./test-data/tt-30.dat")
(define (do-plot-30 output-fn)
  (parameterize ((plot-x-tick-label-angle 45)
                 (plot-x-tick-label-anchor 'top-right)
                 (plot-y-tick-label-angle 45)
                 (plot-y-tick-label-anchor 'bottom-right)
                 (plot-x-far-tick-label-angle 45)
                 (plot-x-far-tick-label-anchor 'bottom-left)
                 (plot-y-far-tick-label-angle 45)
                 (plot-y-far-tick-label-anchor 'top-left)
                 (plot-x-far-label "x far axis")
                 (plot-y-far-label "y far axis"))
    (output-fn (list (discrete-histogram '(#(asdglkj 5399) #(liegjd 5390) #(pqlcxkgfj 3534)))
                     (x-ticks (list (tick 1 #t "asdgwieasdgwefj")) #:far? #t)
                     (y-ticks (list (tick 2500 #t "asdgwegawegfgwiej")) #:far? #t)))))

(define tick-tests
  (test-suite
   "tick-tests"
   (test-case "test case 1" (check-draw-steps do-plot-1 tt-1-data))
   (test-case "test case 2" (check-draw-steps-3d do-plot-2 tt-2-data))
   (test-case "test case 3" (check-draw-steps do-plot-3 tt-3-data))
   (test-case "test case 4" (check-draw-steps do-plot-4 tt-4-data))
   (test-case "test case 5" (check-draw-steps do-plot-5 tt-5-data))
   (test-case "test case 6" (check-draw-steps do-plot-6 tt-6-data))
   (test-case "test case 7" (check-draw-steps do-plot-7 tt-7-data))
   (test-case "test case 8" (check-draw-steps do-plot-8 tt-8-data))
   (test-case "test case 9" (check-draw-steps do-plot-9 tt-9-data))
   (test-case "test case 10" (check-draw-steps do-plot-10 tt-10-data))
   (test-case "test case 11" (check-draw-steps do-plot-11 tt-11-data))
   (test-case "test case 12" (check-draw-steps do-plot-12 tt-12-data))
   (test-case "test case 13" (check-draw-steps do-plot-13 tt-13-data))
   (test-case "test case 14" (check-draw-steps do-plot-14 tt-14-data))
   (test-case "test case 15" (check-draw-steps do-plot-15 tt-15-data))
   (test-case "test case 16" (check-draw-steps do-plot-16 tt-16-data))
   (test-case "test case 17" (check-draw-steps do-plot-17 tt-17-data))
   (test-case "test case 18" (check-draw-steps do-plot-18 tt-18-data))
   (test-case "test case 19" (check-draw-steps do-plot-19 tt-19-data))
   (test-case "test case 20" (check-draw-steps-3d do-plot-20 tt-20-data))
   (test-case "test case 21" (check-draw-steps do-plot-21 tt-21-data))
   (test-case "test case 22" (check-draw-steps do-plot-22 tt-22-data))
   (test-case "test case 23" (check-draw-steps do-plot-23 tt-23-data))
   (test-case "test case 24" (check-draw-steps do-plot-24 tt-24-data))
   (test-case "test case 25" (check-draw-steps-3d do-plot-25 tt-25-data))
   (test-case "test case 26" (check-draw-steps do-plot-26 tt-26-data))
   (test-case "test case 27" (check-draw-steps-3d do-plot-27 tt-27-data))
   (test-case "test case 28" (check-draw-steps do-plot-28 tt-28-data))
   (test-case "test case 29" (check-draw-steps do-plot-29 tt-29-data))
   (test-case "test case 30" (check-draw-steps do-plot-30 tt-30-data))
   (test-case "test case 31" (check-draw-steps do-plot-31 tt-31-data))
   (test-case "test case 32" (check-draw-steps do-plot-32 tt-32-data))
   (test-case "test case 33" (check-draw-steps-3d do-plot-33 tt-33-data))
   (test-case "test case 34" (check-draw-steps-3d do-plot-34 tt-34-data))
   (test-case "test case 35" (check-draw-steps do-plot-35 tt-35-data))
   (test-case "test case 36" (check-draw-steps do-plot-36 tt-36-data))
   (test-case "test case 37" (check-draw-steps-3d do-plot-37 tt-37-data))
   (test-case "test case 38" (check-draw-steps do-plot-38 tt-38-data))
   (test-case "test case 39" (check-draw-steps-3d do-plot-39 tt-39-data))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests tick-tests))
