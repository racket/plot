#lang racket

(require rackunit
         plot
         plot/utils
         racket/draw
         racket/runtime-path
         math/flonum
         pict
         "helpers.rkt")

(define-runtime-path p2d-1-data "./test-data/p1d-1.dat")
(define (do-plot-1 output-fn)
  (output-fn (function / -249 250)))

(define-runtime-path p2d-2-data "./test-data/p2d-2.dat")
(define (do-plot-2 output-fn)
  (random-seed 42)
  (define xs (build-list 10000 (λ _ (random))))
  (output-fn (density xs 1/2)))

(define-runtime-path p2d-3-data "./test-data/p2d-3.dat")
(define (do-plot-3 output-fn)
  (output-fn empty #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1
             #:title "plot should be empty"))

(define-runtime-path p2d-4-data "./test-data/p2d-4.dat")
(define (do-plot-4 output-fn)
  (output-fn (points empty) #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1
             #:title "plot should be empty"))

(define-runtime-path p2d-5-data "./test-data/p2d-5.dat")
(define (do-plot-5 output-fn)
  (output-fn (rectangles (list (list (ivl 0 1) (ivl 0 1))))
             #:x-min 2 #:x-max 3
             #:title "plot should be empty"))

(define-runtime-path p2d-6-data "./test-data/p2d-6.dat")
(define (do-plot-6 output-fn)
  (output-fn (list (function values -4 4) (axes 1 2))))

(define-runtime-path p2d-7-data "./test-data/p2d-7.dat")
(define (do-plot-7 output-fn)
  (output-fn (function values 0 1000)))

(define-runtime-path p2d-8-data "./test-data/p2d-8.dat")
(define (do-plot-8 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-x-ticks      (log-ticks #:base 4)]
                 [plot-y-transform  log-transform]
                 [plot-y-ticks      (linear-ticks #:number 10)])
    (output-fn (function values 1 243))))

(define-runtime-path p2d-9-data "./test-data/p2d-9.dat")
(define (do-plot-9 output-fn)
  (parameterize ([plot-background  "black"]
                 [plot-foreground  "white"]
                 [plot-background-alpha  1/2]
                 [plot-foreground-alpha  1/2])
    (output-fn (function sin -4 4 #:label "y = sin(x)"))))

(define-runtime-path p2d-10-data "./test-data/p2d-10.dat")
(define (do-plot-10 output-fn)
  (parameterize ([plot-x-transform  (hand-drawn-transform 200)]
                 [plot-y-transform  (hand-drawn-transform 200)])
    (output-fn (function sqr -1 1))))

(define-runtime-path p2d-11-data "./test-data/p2d-11.dat")
(define (do-plot-11 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 #;[plot-y-transform  log-transform])
    (output-fn (list (function (λ (x) x) 0.1 10 #:samples 2 #:label "y = x")
                     (polar-axes))
               #:title "Nonlinear scaling" #:x-label "x axis" #:y-label "y-axis")))

(define (degrees->point θ [r 1])
  (vector (* r (cos (degrees->radians θ)))
          (* r (sin (degrees->radians θ)))))

(define-runtime-path p2d-12-data "./test-data/p2d-12.dat")
(define (do-plot-12 output-fn)
  (output-fn (list (lines (list (degrees->point 0)
                                (degrees->point 120)
                                (degrees->point 180 0)
                                (degrees->point 240)
                                (degrees->point 0)))
                   (polar (λ (θ) 1) #:color 0))))

;(plot-x-transform cbrt-transform)
;(plot-y-transform cbrt-transform)
;(plot-x-transform (hand-drawn-transform 100))
;(plot-y-transform (hand-drawn-transform 100))

(define-runtime-path p2d-13-data "./test-data/p2d-13.dat")
(define (do-plot-13 output-fn)
  (output-fn (vector-field (λ (x y) (vector x y))
                           -5 5 -5 5 #:scale 'auto)))

(define-runtime-path p2d-14-data "./test-data/p2d-14.dat")
(define (do-plot-14 output-fn)
  (output-fn (vector-field (λ (x y) (vector (- y) x))
                            -2 2 -1 4 #:scale 'normalized)))

(define-runtime-path p2d-15-data "./test-data/p2d-15.dat")
(define (do-plot-15 output-fn)
  (output-fn (list (lines (for/list ([i  (in-range 6)])
                            (degrees->point (* 2 72 i))))
                   (list (polar (λ (θ) 1) #:color 0)
                         (polar (λ (θ) 0.38) #:color 0)))))

(define-runtime-path p2d-16-data "./test-data/p2d-16.dat")
(define (do-plot-16 output-fn)
  (output-fn (list (lines (for/list ([i  (in-range 4)])
                            (degrees->point (* 120 i))))
                   (lines (for/list ([i  (in-range 4)])
                            (degrees->point (+ 60 (* 120 i)))))
                   (polar (λ (θ) 1) #:color 0)
                   (polar (λ (θ) 0.5) #:color 0))))

(define-runtime-path p2d-17-data "./test-data/p2d-17.dat")
(define (do-plot-17 output-fn)
  (random-seed 42)
  (define xs (build-list 100 (λ _ (random))))
  (define ys (build-list 100 (λ _ (random))))
  (output-fn (list (points (map vector ys xs) #:sym 'full6star #:color 1)
                   (points (map vector xs ys) #:sym "B" #:color 3))))

(define-runtime-path p2d-18-data "./test-data/p2d-18.dat")
(define (do-plot-18 output-fn)
  (output-fn (vector-field (λ (x y) (vector +nan.0 +nan.0)))
             #:x-min -2 #:x-max 2 #:y-min -2 #:y-max 2
             #:title "plot should be empty"))

#;; high-contrast white-on-black:
(begin
  (plot-foreground "white")
  (plot-background "black")
  (point-color "white")
  (histogram-line-color "white")
  (histogram-fill-color "black")
  (contour-color "white"))

;; an exact rational function and a floating-point function
;; the plot of the exact rational function's graph should be smooth
(define-runtime-path p2d-19-data "./test-data/p2d-19.dat")
(define (do-plot-19 output-fn)
  (parameterize ([plot-x-tick-label-angle 15])
    (output-fn (list (function (λ (x) x) #:label "Exact")
                     (function (λ (x) (fl x)) #:color 2 #:label "Inexact"))
               #:x-min #e100000000000000.0 #:x-max #e100000000000000.1
               #:width 450)))

(define-runtime-path p2d-20-data "./test-data/p2d-20.dat")
(define (do-plot-20 output-fn)
   (output-fn (function cos 0 0.0000001) #:width 500))

(define-runtime-path p2d-21-data "./test-data/p2d-21.dat")
(define (do-plot-21 output-fn)
  (output-fn (list (function sin #:label "Sine"
                             #:color "Blue" #:style 'long-dash #:width 3)
                   (function sqr #:label "Square"))
             #:x-min -3 #:x-max 3
             #:legend-anchor 'top-left))

(define-runtime-path p2d-22-data "./test-data/p2d-22.dat")
(define (do-plot-22 output-fn)
  (output-fn (list (axes)
                   (function sqr -2.1 2.1 #:label "x^2")
                   (error-bars (map (λ (x) (vector x (sqr x) (/ 1/2 (+ (abs x) 1))))
                                    (sequence->list (in-range -2 2.1 1/8)))
                               #:x-min -1))))

(define-runtime-path p2d-23-data "./test-data/p2d-23.dat")
(define (do-plot-23 output-fn)
  (output-fn (list (points '(#(1 1) #(2 2) #(3 3)) #:sym "bob" #:size 10
                           #:x-min 2 #:x-max 3 #:y-min 1 #:y-max 3))
             #:x-min 0 #:x-max 4 #:y-min 0 #:y-max 4))

(define-runtime-path p2d-24-data "./test-data/p2d-24.dat")
(define (do-plot-24 output-fn)
  (output-fn (list (x-axis 1) (y-axis 1)
                   (function sqr #f 2 #:color 1)
                   (inverse sqr #:color 2)
                   (function values #:color 0 #:style 1))
             #:x-min -2 #:y-min -1))

(define-runtime-path p2d-25-data "./test-data/p2d-25.dat")
(define (do-plot-25 output-fn)
  (output-fn (list (polar-axes #:number 4)
                   (polar (λ (θ) (+ 0.5 (cos (* 1/2 θ)))) (* -2 pi) (* 2 pi)))))

; draws both functions with x in [-1,1]
(define-runtime-path p2d-26-data "./test-data/p2d-26.dat")
(define (do-plot-26 output-fn)
  (output-fn (list (function sqr #f 1)
                   (function (λ (x) (* 2 (sqr x))) -1 #f
                             #:color "blue")
                   (axes 1 0 #:y-ticks? #f))))

(define-runtime-path p2d-27-data "./test-data/p2d-27.dat")
(define (do-plot-27 output-fn)
  (output-fn (list (function sqr #f -1)
                   (function sqr 1 #f))
             #:title "plot should be empty"))

; draws both functions with x in [-1,2] (meaning nothing is drawn)
(define-runtime-path p2d-28-data "./test-data/p2d-28.dat")
(define (do-plot-28 output-fn)
  (output-fn (list (function sqr #f -1)
                   (function sqr 2 #f))
             #:title "plot should be empty"))

; draws first function with x in [-2,-1]
(define-runtime-path p2d-29-data "./test-data/p2d-29.dat")
(define (do-plot-29 output-fn)
  (output-fn (list (function sqr #f -1)
                   (function sqr 1 #f))
             #:x-min -2))

; draws second function with x in [1,2]
(define-runtime-path p2d-30-data "./test-data/p2d-30.dat")
(define (do-plot-30 output-fn)
  (output-fn (list (function sqr #f -1)
                   (function sqr 1 #f))
             #:x-max 2))

; draws both functions with x in [-2,2]
(define-runtime-path p2d-31-data "./test-data/p2d-31.dat")
(define (do-plot-31 output-fn)
  (output-fn (list (function sqr #f -1)
                   (function sqr 1 #f))
             #:x-min -2 #:x-max 2))

; draws both in full (particularly, without chopping off the top of the parabola), in [-2,2]
(define-runtime-path p2d-32-data "./test-data/p2d-32.dat")
(define (do-plot-32 output-fn)
  (output-fn (list (function sqr)
                   (function sin -2 2))))

(define-runtime-path p2d-33-data "./test-data/p2d-33.dat")
(define (do-plot-33 output-fn)
  (output-fn (list (discrete-histogram
                    (build-list 10 (λ (n) (vector (string-ref "abcdefghij" n)
                                                  (sqr n))))
                    #:label "ord(x)^2")
                   (function truncate))))

(define-runtime-path p2d-34-data "./test-data/p2d-34.dat")
(define (do-plot-34 output-fn)
  (output-fn (list (x-axis)
                   (discrete-histogram '((a -1) (b 2.6) (c 4) (d 3.1)) #:y-min #f
                                       #:color 5 #:line-color 5 #:line-style 'long-dash
                                       #:label "Corrupt")
                   (discrete-histogram '(#(a 1) #(b 2.6) #(c 4) #(d 3.1))
                                       #:x-min 5
                                       #:color 1 #:line-color 1 #:line-width 3
                                       #:label "Clean"))
             #:title "Widgetyness of Widgets"
             #:x-label "Widget"
             #:y-label "Widgetyness"
             #:legend-anchor 'bottom-right))

(define-runtime-path p2d-35-data "./test-data/p2d-35.dat")
(define (do-plot-35 output-fn)
  (output-fn (stacked-histogram '(#(a (1 1 1)) #(b (1.5 3)) #(c ()) #(d (1/2)))
                                #:labels '("Red" #f "Blue"))))

(define-runtime-path p2d-36-data "./test-data/p2d-36.dat")
(define (do-plot-36 output-fn)
  (parameterize ([discrete-histogram-gap  0]
                 [discrete-histogram-skip  3]
                 [rectangle-line-width 2])
    (output-fn (list (discrete-histogram '(#(a 1) #(b 2.5) #(c 2)) #:label "Blue")
                     (discrete-histogram '(#(a 2) #(b 4) #(c 1)) #:x-min 2/3 #:color 1 #:line-color 1
                                         #:label "Red")
                     (discrete-histogram '(#(a 3) #(b 3) #(c 2.5)) #:x-min 4/3 #:color 2 #:line-color 2
                                         #:label "Green")))))

(define-runtime-path p2d-37-data "./test-data/p2d-37.dat")
(define (do-plot-37 output-fn)
  (parameterize ([discrete-histogram-gap  0]
                 [discrete-histogram-skip  2]
                 [stacked-histogram-line-widths '(3)])
    (output-fn (list (stacked-histogram '(#(a (0.2 1)) #(b (2.5 1.2)) #(c (2 0))))
                     (stacked-histogram '((a (2 1)) (b (1.1 0.9)) (c (1 1.1))) #:x-min 7/8
                                        #:colors '(3 4)
                                        #:line-colors '(3 4))))))

(define-runtime-path p2d-38-data "./test-data/p2d-38.dat")
(define (do-plot-38 output-fn)
  (parameterize ([plot-x-ticks  (currency-ticks)])
    (output-fn (discrete-histogram (list (vector '(a . a) 1) (vector '(a . b) 2)
                                         (vector '(b . b) 3) (vector '(b . a) 4))
                                   #:invert? #t #:add-ticks? #f))))

(define-runtime-path p2d-39-data "./test-data/p2d-39.dat")
(define (do-plot-39 output-fn)
  (parameterize ([plot-x-ticks  (currency-ticks)])
    (output-fn (stacked-histogram (list (vector '(a . a) '(1 2 1)) (vector '(a . b) '(2 1 3))
                                        (vector '(b . b) '()) (vector '(b . a) '(4 4 2)))
                                  #:invert? #t #:add-ticks? #f))))

(define-runtime-path p2d-40-data "./test-data/p2d-40.dat")
(define (do-plot-40 output-fn)
  (output-fn (rectangles
              (map vector
                   (bounds->intervals (map log (linear-seq 10 20 10)))
                   (build-list 9 (λ (n) (ivl (sqr n) (- (sqr n)))))))))

(define-runtime-path p2d-41-data "./test-data/p2d-41.dat")
(define (do-plot-41 output-fn)
  (define (f x) (* (/ 1 (sqrt (* 2 pi)))
                   (exp (* -1/2 (sqr x)))))
  (output-fn (list (area-histogram f (linear-seq -4 4 10))
                   (function f -4 4))))

(define-runtime-path p2d-42-data "./test-data/p2d-42.dat")
(define (do-plot-42 output-fn)
  ;; Intentionally using fewer samples than bins
  (output-fn (list (area-histogram sqr (map (λ (x) (* (sqrt x) (sqrt 8))) (linear-seq 0 8 10)) #:samples 2)
                   (function sqr 0 8 #:samples 2))))

(define-runtime-path p2d-43-data "./test-data/p2d-43.dat")
(define (do-plot-43 output-fn)
  (output-fn (list (area-histogram sqr (map (λ (x) (* (sqrt x) (sqrt 8))) (linear-seq 0 8 10)))
                    (function sqr 0 8))))

(define-runtime-path p2d-44-data "./test-data/p2d-44.dat")
(define (do-plot-44 output-fn)
  (random-seed 42)
  (define xs (build-list 10000 (λ _ (random))))
  (define ys (build-list 10000 (λ _ (random))))
  (output-fn (list
              (points (map vector xs ys)
                      #:x-min -1 #:x-max 1 #:y-min 0.5 #:y-max 1
                      #:sym 'fullcircle #:size 6.5 #:alpha 1/8
                      #:label "Dots"))
             #:y-max 1.5))

(define-runtime-path p2d-45-data "./test-data/p2d-45.dat")
(define (do-plot-45 output-fn)
  (output-fn (vector-field (λ (x y) (vector x y)) -0.5 1.85 -5 0.5
                           #:color "blue" #:line-width 2/3
                           #:scale 'auto #:label "(vector x y)")
             #:x-min -1 #:x-max 5))

(define-runtime-path p2d-46-data "./test-data/p2d-46.dat")
(define (do-plot-46 output-fn)
  (output-fn (list (function (λ (x) (* 220 (cos (* 4 x)))) -2 2)
                   (function (λ (x) (* 200 (sin (* 3 x)))) 0 #f
                             #:y-min -150 #:y-max 150
                             #:color "blue"))
             #:x-min -1/2 #:x-max 3))

(define-runtime-path p2d-47-data "./test-data/p2d-47.dat")
(define (do-plot-47 output-fn)
  (random-seed 42)
  (output-fn (lines (reverse
                     (for/fold ([lst (list (vector 0 0))]) ([i  (in-range 1 400)])
                       (match-define (vector x y) (first lst))
                       (cons (vector i (+ y (* 1/100 (- (random) 1/2)))) lst)))
                    #:alpha 0.5 #:label "Random walk")))

(define-runtime-path p2d-48-data "./test-data/p2d-48.dat")
(define (do-plot-48 output-fn)
  (output-fn (function (λ (x) (/ 1.0 (fl x))) -2 2)))

(define-runtime-path p2d-49-data "./test-data/p2d-49.dat")
(define (do-plot-49 output-fn)
  (output-fn (parametric (λ (t) (vector (sin t) (cos t))) (- pi) pi
                         #:x-min -0.5 #:y-max 0.5)
             #:x-min -1))

(define-runtime-path p2d-50-data "./test-data/p2d-50.dat")
(define (do-plot-50 output-fn)
  (output-fn (list (function sin -1/2 1)
                   (parametric (λ (t) (vector (cos t) (sin t))) -2 1
                               #:color "blue" #:style 'short-dash))))

(define (norm mx my x y)
  (exp (* -1/2 (+ (sqr (- x mx)) (sqr (- y my))))))

(define (f1 x y)
  (+ (norm -1.5 -1.5 x y)
     (* 2 (norm 1 1 x y))
     (* 1.5 (norm 2 -2 x y))))

(define (f2 x y)
  (- (sqr x) (sqr y)))

(define-runtime-path p2d-51-data "./test-data/p2d-51.dat")
(define (do-plot-51 output-fn)
  (output-fn (list (contours f1 0 5 #:label "Cyan/Redness")
                   (contours f2 -5 0 #:colors '("blue") #:label "Blueness"
                             #:widths '(2) #:styles '(dot)))
             #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5))

(define-runtime-path p2d-52-data "./test-data/p2d-52.dat")
(define (do-plot-52 output-fn)
  (output-fn (contours f2 -5 0) #:x-min 1 #:x-max 3 #:y-min 1 #:y-max 3
             #:title "plot should be empty"))
(define-runtime-path p2d-53-data "./test-data/p2d-53.dat")
(define (do-plot-53 output-fn)
  (output-fn (contour-intervals f2 -5 0) #:x-min 1 #:x-max 3 #:y-min 1 #:y-max 3
             #:title "plot should be empty"))

(define-runtime-path p2d-54-data "./test-data/p2d-54.dat")
(define (do-plot-54 output-fn)
  (output-fn (contour-intervals f1 -5 5 -5 5 #:label "z")))

(define-runtime-path p2d-55-data "./test-data/p2d-55.dat")
(define (do-plot-55 output-fn)
  (output-fn (contour-intervals
              (λ (x y)
                (define z (- x y))
                (cond [(< z -1) -1]
                      [(> z 1)   1]
                      [else      z]))
              -2 2 -2 2)))

(define-runtime-path p2d-56-data "./test-data/p2d-56.dat")
(define (do-plot-56 output-fn)
  (output-fn (list (tick-grid)
                   (contour-intervals f1 -5 2 -5 2
                                      #:levels 5
                                      #:contour-styles '(transparent)
                                      #:label "")
                   (contours f1 -2 5 -2 5 #:levels 5 #:label ""))
             #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5
             #:legend-anchor 'center))

(define-runtime-path p2d-57-data "./test-data/p2d-57.dat")
(define (do-plot-57 output-fn)
  (output-fn (list (tick-grid)
                   (contour-intervals f1 -5 2 -5 2
                                      #:levels '(0.25 0.5 0.75 1.0 1.25 1.5 1.75)
                                      #:colors (compose default-contour-colors (curry map ivl-center))
                                      #:styles '(0 1 2 3 4 5 6)
                                      #:contour-styles '(transparent)
                                      #:label "z")
                   (contours f1 -2 5 -2 5 #:levels '(0.25 0.5 0.75 1.0 1.25 1.5 1.75)))
             #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5
             #:legend-anchor 'top-left))

; tests contour 7-sided and 8-sided saddle facets
; contour shading should line up with contour lines, no matter how weird
(define-runtime-path p2d-58-data "./test-data/p2d-58.dat")
(define (do-plot-58 output-fn)
  (parameterize ([contour-samples  10])
    (define (f x y) (sqr (sin (- x y))))
    (output-fn (contour-intervals f)
               #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)))

(define-runtime-path p2d-59-data "./test-data/p2d-59.dat")
(define (do-plot-59 output-fn)
  (define (f2 x) (sin (* x pi)))
  (output-fn (list (x-tick-lines)
                   (function-interval atan (λ (x) (- (atan x)))
                                      #:color 6 #:line1-color 6 #:line2-color 6)
                   (function-interval sqr f2 -1 1 #:alpha 0.5)
                   (function-label f2 -1/4 #:anchor 'top-left))))

(define-runtime-path p2d-60-data "./test-data/p2d-60.dat")
(define (do-plot-60 output-fn)
  (define amps (linear-seq 1/4 1 8))
  (define colors (color-seq* '("darkred" "white" "darkblue") 7))
  (output-fn (flatten
              (list
               (x-tick-lines)
               (for/list ([a1     (in-list amps)]
                          [a2     (in-list (rest amps))]
                          [color  (in-list colors)])
                 (inverse-interval (λ (y) (* a1 (sin y)))
                                   (λ (y) (* a2 (sin y)))
                                   (- pi) pi #:color color #:alpha 1
                                   #:label (format "f(~a,~a)" a1 a2)))
               (y-tick-lines)
               (inverse-label (λ (y) (* 1/4 (sin y))) (* -1/2 pi)
                              "x = 1/4 sin(y)")))
             #:legend-anchor 'top-left))

(define-runtime-path p2d-61-data "./test-data/p2d-61.dat")
(define (do-plot-61 output-fn)
  (define a #(0 0))
  (define b #(1 1/2))
  (define c #(0 1))
  (define d #(1 3/2))
  (output-fn (list
              (tick-grid)
              (lines-interval (list a b) (list c d)
                              #:color 4 #:line1-color 4 #:line2-color 4
                              #:label "Parallelogram")
              (point-label #(1/2 5/4) #:anchor 'bottom-right #:alpha 0.5))
             #:legend-anchor 'bottom-left))

(define-runtime-path p2d-62-data "./test-data/p2d-62.dat")
(define (do-plot-62 output-fn)
  (define (fa t) (vector (* 2 (cos (* 4/5 t))) (* 2 (sin (* 4/5 t)))))
  (define (fb t) (vector (cos t) (sin t)))
  (define (fc t) (vector (* 1/2 (cos (* 4/5 t))) (* 1/2 (sin (* 4/5 t)))))
  (define t1 (- pi))
  (define t2 pi)
  (output-fn (list
              (x-tick-lines)
              (lines (list (fa t1) (fb t1) (vector +nan.0 +nan.0) (fb t1) (fc t1))
                     #:color "black" #:style 'dot)
              (lines (list (fa t2) (fb t2) (vector +nan.0 +nan.0) (fb t2) (fc t2))
                     #:color "black" #:style 'dot)
              (parametric fa t1 t2 #:color 5 #:label "fa")
              (parametric-interval fa fb t1 t2 #:color 5 #:label "(fa,fb)"
                                   #:line1-style 'transparent
                                   #:line2-style 'transparent)
              (parametric fb t1 t2 #:color 1 #:label "fb")
              (parametric-interval fb fc t1 t2 #:color 2 #:label "(fb,fc)"
                                   #:line1-style 'transparent
                                   #:line2-style 'transparent)
              (parametric fc t1 t2 #:color 2 #:label "fc")
              (x-axis #:ticks? #f)
              (parametric-label fa t1 "fa(-π)"
                                #:size 14 #:anchor 'left #:point-size 5)
              (parametric-label fa t2 "fa(π)"
                                #:size 14 #:anchor 'left #:point-size 5))
             #:legend-anchor 'top-right))

(define-runtime-path p2d-63-data "./test-data/p2d-63.dat")
(define (do-plot-63 output-fn)
  (define (f1 θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))
  (define (f2 θ) (+ 1 (* 1/4 (cos (* 10 θ)))))
  (output-fn (list (polar-axes #:number 10)
                   (polar-interval f1 f2
                                   #:color 3 #:label "[f1,f2]"
                                   #:line1-color 1 #:line1-width 2 #:line1-style 'dot
                                   #:line2-color 2 #:line2-width 2)
                   (polar-label f1 0 #:anchor 'top-left)
                   (polar-label f2 (degrees->radians 36) #:anchor 'bottom-right)
                   (point-label #(1/2 1/2)))))

(define-runtime-path p2d-64-data "./test-data/p2d-64.dat")
(define (do-plot-64 output-fn)
  (define (f1 θ) (/ θ pi 2))
  (define (f2 θ) (+ (/ θ pi 2) 1))
  (output-fn (list (tick-grid)
                   (polar-interval f1 f2 0 (* 5 pi)
                                   #:color 4 #:alpha 3/4
                                   #:line1-color 1 #:line2-color 1
                                   #:label "[f1,f2]"))
             #:legend-anchor 'center))

(define ((make-fun y) x)
    (+ y (sqr x)))

(define-runtime-path p2d-65-data "./test-data/p2d-65.dat")
(define-runtime-path p2d-65a-data "./test-data/p2d-65a.dat")
(define (do-plot-65 output-fn)
   (output-fn (build-list
               20 (λ (n) (function (make-fun n) #:color n #:style n #:width 2)))
              #:x-min -2 #:x-max 2))

(define (do-plot-65a output-fn)
  (output-fn (list
              (tick-grid)
              (function-interval (λ (x) 0) (λ (x) 16) #:color "black" #:alpha 1/20)
              (build-list
               12 (λ (n) (function-interval
                          (make-fun n) (make-fun (add1 n)) -2 0
                          #:color (->pen-color n) #:style n #:alpha 1
                          #:line1-style 'transparent #:line2-style 'transparent)))
              (build-list
               12 (λ (n) (function-interval
                          (make-fun n) (make-fun (add1 n)) 0 2
                          #:color n
                          #:line1-style 'transparent #:line2-style 'transparent)))
              (build-list
               13 (λ (n) (function (make-fun n) -2 0
                                   #:color n #:width 2)))
              (build-list
               13 (λ (n) (function (make-fun n) 0 2
                                   #:color n #:width 2 #:style n))))
             #:x-min -2 #:x-max 2))

(define-runtime-path p2d-66-data "./test-data/p2d-66.dat")
(define (do-plot-66 output-fn)
  (define (f x) (/ (sin x) x))
  (parameterize ([plot-x-transform  (stretch-transform -1 1 10)]
                 [plot-x-ticks      (ticks-add (plot-x-ticks) '(-1 1))]
                 [plot-y-ticks      (fraction-ticks)])
    (output-fn (list (y-axis -1 #:ticks? #f) (y-axis 1 #:ticks? #f)
                     (function f -1 1 #:width 2 #:color 4)
                     (function f -14 -1 #:color 4 #:label "y = sin(x)/x")
                     (function f 1 14 #:color 4)
                     (point-label (vector 0 1) "y → 1 as x → 0" #:anchor 'bottom-right))
               #:y-max 1.2)))

(define-runtime-path p2d-67-data "./test-data/p2d-67.dat")
(define (do-plot-67 output-fn)
  (output-fn
   (list (hrule -1)
         (hrule 0 0)
         (hrule 1 0 1 #:color 1 #:width 3 #:style 'long-dash #:alpha 0.6 #:label "H")
         (vrule -1)
         (vrule 0 0)
         (vrule 1 0 1 #:color 2 #:width 3 #:style 'long-dash #:alpha 0.6 #:label "V"))
   #:x-min -2 #:x-max 2
   #:y-min -2 #:y-max 2))

;; (printf "This plot should contain~n- 4 labels, all within the plot bounds~n- points near the top and bottom labels~n- no points near the left and right labels~n")
(define-runtime-path p2d-68-data "./test-data/p2d-68.dat")
(define (do-plot-68 output-fn)
  (output-fn
   ;; Add labels on the edges of the plot area.  Each label will be positioned
   ;; so it is inside the plot area
   (list (point-label (vector 0 10) #:anchor 'auto)
         (point-label (vector 0 -10) #:anchor 'auto)
         ;; Side labels have no corresponding point
         (point-label (vector 10 0) #:anchor 'auto #:point-sym 'none)
         (point-label (vector -10 0) #:anchor 'auto #:point-sym 'none))
   #:x-min -10 #:x-max 10
   #:y-min -10 #:y-max 10))

;; (printf "This plot should contain points and functions labeled with standard fish~n")
(define-runtime-path p2d-69-data "./test-data/p2d-69.dat")
(define (do-plot-69 output-fn)
  (define blue-fish (standard-fish 40 15 #:color "lightblue"))
  (define red-fish (standard-fish 40 15 #:color "salmon"))
  (define (f t) (vector (cos t) (sin t)))
  (output-fn
   (list (point-pict (vector 0 -10) blue-fish #:anchor 'auto #:point-size 15)
         (parametric f 0 (* 2 pi))
         (parametric-pict f (/ pi 2) red-fish #:anchor 'bottom )
         (function sin)
         (function-pict sin -5 red-fish #:anchor 'left)
         (polar-pict (lambda (a) (* 8 (sin a))) (/ pi 4) red-fish)
         (inverse-pict asin 1 red-fish))
   #:x-min -10 #:x-max 10
   #:y-min -10 #:y-max 10))

(define-runtime-path p2d-70-data "./test-data/p2d-70.dat")
(define (do-plot-70 output-fn)
  (output-fn (rectangles (list (vector (ivl 2 3) (ivl -inf.0 +inf.0))
                               (vector (ivl -inf.0 +inf.0) (ivl 2 3))
                               (vector (ivl 5 6) (ivl 0 +inf.0)))
                         #:line-style 'transparent
                         #:alpha 0.5)
             #:x-min -10 #:x-max 10
             #:y-min -10 #:y-max 10
             #:title "rectangles that extend to the edge of the plot"))

(define plot2d-tests
  (test-suite
   "plot2d-tests"
   (test-case "test case 1" (check-draw-steps do-plot-1 p2d-1-data))
   (test-case "test case 2" (check-draw-steps do-plot-2 p2d-2-data))
   (test-case "test case 3" (check-draw-steps do-plot-3 p2d-3-data))
   (test-case "test case 4" (check-draw-steps do-plot-4 p2d-4-data))
   (test-case "test case 5" (check-draw-steps do-plot-5 p2d-5-data))
   (test-case "test case 6" (check-draw-steps do-plot-6 p2d-6-data))
   (test-case "test case 7" (check-draw-steps do-plot-7 p2d-7-data))
   (test-case "test case 8" (check-draw-steps do-plot-8 p2d-8-data))
   (test-case "test case 9" (check-draw-steps do-plot-9 p2d-9-data))
   (test-case "test case 10" (check-draw-steps do-plot-10 p2d-10-data))
   (test-case "test case 11" (check-draw-steps do-plot-11 p2d-11-data))
   (test-case "test case 12" (check-draw-steps do-plot-12 p2d-12-data))
   (test-case "test case 13" (check-draw-steps do-plot-13 p2d-13-data))
   (test-case "test case 14" (check-draw-steps do-plot-14 p2d-14-data))
   (test-case "test case 15" (check-draw-steps do-plot-15 p2d-15-data))
   (test-case "test case 16" (check-draw-steps do-plot-16 p2d-16-data))
   (test-case "test case 17" (check-draw-steps do-plot-17 p2d-17-data))
   (test-case "test case 18" (check-draw-steps do-plot-18 p2d-18-data))
   (test-case "test case 19" (check-draw-steps do-plot-19 p2d-19-data))
   (test-case "test case 20" (check-draw-steps do-plot-20 p2d-20-data))
   (test-case "test case 21" (check-draw-steps do-plot-21 p2d-21-data))
   (test-case "test case 22" (check-draw-steps do-plot-22 p2d-22-data))
   (test-case "test case 23" (check-draw-steps do-plot-23 p2d-23-data))
   (test-case "test case 24" (check-draw-steps do-plot-24 p2d-24-data))
   (test-case "test case 25" (check-draw-steps do-plot-25 p2d-25-data))
   (test-case "test case 26" (check-draw-steps do-plot-26 p2d-26-data))
   (test-case "test case 27" (check-draw-steps do-plot-27 p2d-27-data))
   (test-case "test case 28" (check-draw-steps do-plot-28 p2d-28-data))
   (test-case "test case 29" (check-draw-steps do-plot-29 p2d-29-data))
   (test-case "test case 30" (check-draw-steps do-plot-30 p2d-30-data))
   (test-case "test case 31" (check-draw-steps do-plot-31 p2d-31-data))
   (test-case "test case 32" (check-draw-steps do-plot-32 p2d-32-data))
   (test-case "test case 33" (check-draw-steps do-plot-33 p2d-33-data))
   (test-case "test case 34" (check-draw-steps do-plot-34 p2d-34-data))
   (test-case "test case 35" (check-draw-steps do-plot-35 p2d-35-data))
   (test-case "test case 36" (check-draw-steps do-plot-36 p2d-36-data))
   (test-case "test case 37" (check-draw-steps do-plot-37 p2d-37-data))
   (test-case "test case 38" (check-draw-steps do-plot-38 p2d-38-data))
   (test-case "test case 39" (check-draw-steps do-plot-39 p2d-39-data))
   (test-case "test case 40" (check-draw-steps do-plot-40 p2d-40-data))
   (test-case "test case 41" (check-draw-steps do-plot-41 p2d-41-data))
   (test-case "test case 42" (check-draw-steps do-plot-42 p2d-42-data))
   (test-case "test case 43" (check-draw-steps do-plot-43 p2d-43-data))
   (test-case "test case 44" (check-draw-steps do-plot-44 p2d-44-data))
   (test-case "test case 45" (check-draw-steps do-plot-45 p2d-45-data))
   (test-case "test case 46" (check-draw-steps do-plot-46 p2d-46-data))
   (test-case "test case 47" (check-draw-steps do-plot-47 p2d-47-data))
   (test-case "test case 48" (check-draw-steps do-plot-48 p2d-48-data))
   (test-case "test case 49" (check-draw-steps do-plot-49 p2d-49-data))
   (test-case "test case 50" (check-draw-steps do-plot-50 p2d-50-data))
   (test-case "test case 51" (check-draw-steps do-plot-51 p2d-51-data))
   (test-case "test case 52" (check-draw-steps do-plot-52 p2d-52-data))
   (test-case "test case 53" (check-draw-steps do-plot-53 p2d-53-data))
   (test-case "test case 54" (check-draw-steps do-plot-54 p2d-54-data))
   (test-case "test case 55" (check-draw-steps do-plot-55 p2d-55-data))
   (test-case "test case 56" (check-draw-steps do-plot-56 p2d-56-data))
   (test-case "test case 57" (check-draw-steps do-plot-57 p2d-57-data))
   (test-case "test case 58" (check-draw-steps do-plot-58 p2d-58-data))
   (test-case "test case 59" (check-draw-steps do-plot-59 p2d-59-data))
   (test-case "test case 60" (check-draw-steps do-plot-60 p2d-60-data))
   (test-case "test case 61" (check-draw-steps do-plot-61 p2d-61-data))
   (test-case "test case 62" (check-draw-steps do-plot-62 p2d-62-data))
   (test-case "test case 63" (check-draw-steps do-plot-63 p2d-63-data))
   (test-case "test case 64" (check-draw-steps do-plot-64 p2d-64-data))
   (test-case "test case 65" (check-draw-steps do-plot-65 p2d-65-data))
   (test-case "test case 65a" (check-draw-steps do-plot-65a p2d-65a-data))
   (test-case "test case 66" (check-draw-steps do-plot-66 p2d-66-data))
   (test-case "test case 67" (check-draw-steps do-plot-67 p2d-67-data))
   (test-case "test case 68" (check-draw-steps do-plot-68 p2d-68-data))
   (test-case "test case 69" (check-draw-steps do-plot-69 p2d-69-data))
   (test-case "test case 70" (check-draw-steps do-plot-70 p2d-70-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests plot2d-tests))
