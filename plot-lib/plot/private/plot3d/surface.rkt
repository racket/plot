#lang typed/racket/base

(require typed/racket/class racket/match racket/list
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Surface plots of R R -> R functions

(: surface3d-render-proc (-> 2D-Sampler Positive-Integer
                             Plot-Color Plot-Brush-Style
                             Plot-Color Nonnegative-Real Plot-Pen-Style
                             Nonnegative-Real
                             (U #f String)
                             3D-Render-Proc))
(define ((surface3d-render-proc f samples color style line-color line-width line-style alpha label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (define num (animated-samples samples))
  (define sample (f (vector x-ivl y-ivl) (vector num num)))
  
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (for-2d-sample
   (xa xb ya yb z1 z2 z3 z4) sample
   (define vs (list (vector xa ya z1) (vector xb ya z2) (vector xb yb z3) (vector xa yb z4)))
   (send area put-polygon vs))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else   empty]))

(:: surface3d
    (->* [(-> Real Real Real)]
         [(U #f Real) (U #f Real) (U #f Real) (U #f Real)
          #:z-min (U #f Real) #:z-max (U #f Real)
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U #f String)]
         renderer3d))
(define (surface3d f [x-min #f] [x-max #f] [y-min #f] [y-max #f] #:z-min [z-min #f] #:z-max [z-max #f]
                   #:samples [samples (plot3d-samples)]
                   #:color [color (surface-color)]
                   #:style [style (surface-style)]
                   #:line-color [line-color (surface-line-color)]
                   #:line-width [line-width (surface-line-width)]
                   #:line-style [line-style (surface-line-style)]
                   #:alpha [alpha (surface-alpha)]
                   #:label [label #f])
  (define fail/pos (make-raise-argument-error 'surface3d f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'surface3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 2)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 3)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 4)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(< samples 2)  (fail/kw "integer >= 2" '#:samples samples)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define z-ivl (ivl z-min z-max))
     (define g (2d-function->sampler f (vector x-ivl y-ivl)))
     (renderer3d (vector x-ivl y-ivl z-ivl)
                 (surface3d-bounds-fun g samples)
                 default-ticks-fun
                 (surface3d-render-proc g samples color style
                                        line-color line-width line-style alpha label))]))
