#lang typed/racket/base

;; Renderers for points and other point-like things.

(require typed/racket/class racket/match racket/math racket/list
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(require/typed
 "../common/untyped-utils.rkt"
 [fix-a-field-fun  (All (A)
                        (-> Symbol
                            (U (-> Real Real A)
                               (-> (Vector Real Real) A))
                            (-> Real Real A)))])

(provide (all-defined-out))

;; ===================================================================================================
;; color-field
;; similar to point.rkt/vector-field, but draws a square area with a color

(require (only-in "../common/parameters.rkt" color-field-samples color-field-alpha))

(: color-field-render-fun
   (-> (-> Real Real Plot-Color)
       Positive-Integer
       Nonnegative-Real
       2D-Render-Proc))
(define ((color-field-render-fun f samples alpha) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  
  (cond
    [(and x-min x-max y-min y-max)
     (define xs (linear-seq x-min x-max (+ samples 1) #:start? #t #:end? #t))
     (define ys (linear-seq y-min y-max (+ samples 1) #:start? #t #:end? #t))

     (send area put-alpha alpha)
     (send area put-pen 'black 0 'transparent)
     (for ([x- (in-list xs)]
           [x+ (in-list (cdr xs))])
       (define x (/ (+ x- x+) 2))
       (for ([y- (in-list ys)]
             [y+ (in-list (cdr ys))])
         (define y (/ (+ y- y+) 2))
         (define c (f x y))
         (send area put-brush c 'solid)
         (send area put-rect (vector (ivl x- x+)
                                     (ivl y- y+)))))
     empty]
    [else  empty]))

(:: color-field
    (->* [(U (-> Real Real Plot-Color)
             (-> (Vector Real Real) Plot-Color))]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:samples Positive-Integer
          #:alpha Nonnegative-Real]
         renderer2d))
(define (color-field f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                      #:samples [samples (color-field-samples)]
                      #:alpha [alpha (color-field-alpha)])
  (define fail/pos (make-raise-argument-error 'vector-field3d f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'vector-field3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 2)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 3)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 4)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([f ((inst fix-a-field-fun Plot-Color) 'color-field f)])
       (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                   (color-field-render-fun
                    f samples alpha)))]))

(module+ test
  (require plot)
  ;https://rosettacode.org/wiki/Mandelbrot_set#Racket
  (define (iterations [a : Number][z : Number][i : Nonnegative-Integer]) : Nonnegative-Integer
    (define z′ (+ (* z z) a))
    (if (or (<= 255 i) (< 2 (magnitude z′)))
        i
        (iterations a z′ (+ i 1))))
 
  (define (iter->color [i : Nonnegative-Integer]) : Plot-Color
    (if (= i 255)
        'black
        (list (* 5 (modulo i 15)) (* 32 (modulo i 7)) (* 8 (modulo i 31)))))

  (define (mandelbrot [x : Real][y : Real]) : Plot-Color
    (define z (make-rectangular (* 1.0 x) (* 1.0 y)))
    (iter->color (iterations z 0 0)))

  (plot (color-field mandelbrot -2.25 0.75 -1.25 1.25
                     #:samples 200)))

#;(module+ test
  (require plot)
  (define (in-circle [x : Real][y : Real])
    (define z (make-rectangular x y))
    (if (< (magnitude z) 1) (random 10) 'black))
  (plot (color-field in-circle -2 2 -2 2)))
