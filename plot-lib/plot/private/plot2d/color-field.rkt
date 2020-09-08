#lang typed/racket/base

;; Renderers for points and other point-like things.

(require typed/racket/class racket/match racket/list
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

(: color-field-render-fun
   (-> (-> Real Real Plot-Color)
       Positive-Integer
       Nonnegative-Real
       2D-Render-Proc))
(define ((color-field-render-fun f samples alpha) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  
  (when (and x-min x-max y-min y-max)
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
                                    (ivl y- y+)))))))

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
       (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun #f
                   (color-field-render-fun
                    f samples alpha)))]))

