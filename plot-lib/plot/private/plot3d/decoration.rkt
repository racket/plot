#lang typed/racket/base

(require typed/racket/class typed/racket/draw racket/match racket/list
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Labeled points

(: format-coordinate3d (-> (Vectorof Real) (Instance 3D-Plot-Area%) String))
(define (format-coordinate3d v area)
  (match-define (vector x y z) v)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
    (send area get-bounds-rect))
  (match-define (list x-str)
    (cond [(and x-min x-max)  (format-tick-labels (plot-x-ticks) x-min x-max (list x))]
          [else  "?"]))
  (match-define (list y-str)
    (cond [(and y-min y-max)  (format-tick-labels (plot-y-ticks) y-min y-max (list y))]
          [else  "?"]))
  (match-define (list z-str)
    (cond [(and z-min z-max)  (format-tick-labels (plot-z-ticks) z-min z-max (list z))]
          [else  "?"]))
  (format "(~a,~a,~a)" x-str y-str z-str))

(: label3d-render-proc (-> (U String #f) (Vectorof Real)
                           Plot-Color Nonnegative-Real (U String #f) Font-Family
                           Anchor Real
                           Plot-Color Plot-Color Nonnegative-Real Nonnegative-Real Point-Sym
                           Nonnegative-Real
                           3D-Render-Proc))
(define ((label3d-render-proc label v color size face family anchor angle
                              point-color point-fill-color point-size point-line-width point-sym
                              alpha)
         area)
  (let ([label  (if label label (format-coordinate3d v area))])
    (send area put-alpha alpha)
    ; label
    (send area put-text-foreground color)
    (send area put-font-attribs size face family)
    (send area put-text (string-append " " label " ") v anchor angle (* 1/2 point-size) #t
          plot3d-front-layer)
    ; point
    (send area put-pen point-color point-line-width 'solid)
    (send area put-brush point-fill-color 'solid)
    (send area put-glyphs (list v) point-sym point-size plot3d-front-layer))
  
  empty)

(:: point-label3d
    (->* [(Sequenceof Real)]
         [(U String #f)
          #:color Plot-Color
          #:size Nonnegative-Real
          #:face (U String #f)
          #:family Font-Family
          #:anchor Anchor
          #:angle Real
          #:point-color Plot-Color
          #:point-fill-color (U Plot-Color 'auto)
          #:point-size Nonnegative-Real
          #:point-line-width Nonnegative-Real
          #:point-sym Point-Sym
          #:alpha Nonnegative-Real]
         renderer3d))
(define (point-label3d v [label #f]
                       #:color [color (plot-foreground)]
                       #:size [size (plot-font-size)]
                       #:face [face (plot-font-face)]
                       #:family [family (plot-font-family)]
                       #:anchor [anchor (label-anchor)]
                       #:angle [angle (label-angle)]
                       #:point-color [point-color (point-color)]
                       #:point-fill-color [point-fill-color 'auto]
                       #:point-size [point-size (label-point-size)]
                       #:point-line-width [point-line-width (point-line-width)]
                       #:point-sym [point-sym 'fullcircle]
                       #:alpha [alpha (label-alpha)])
  (define fail/kw (make-raise-keyword-error 'point-label3d))
  (cond
    [(not (rational? size))  (fail/kw "rational?" '#:size size)]
    [(not (rational? angle))  (fail/kw "rational?" '#:angle angle)]
    [(not (rational? point-size))  (fail/kw "rational?" '#:point-size point-size)]
    [(not (rational? point-line-width))  (fail/kw "rational?" '#:point-line-width point-line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([v  (sequence-head-vector 'point-label3d v 3)])
       (match-define (vector x y z) v)
       (renderer3d (vector (ivl x x) (ivl y y) (ivl z z)) #f #f
                   (label3d-render-proc
                    label v color size face family anchor angle
                    point-color (cond [(eq? point-fill-color 'auto)  (->pen-color point-color)]
                                      [else  point-fill-color])
                    point-size point-line-width point-sym
                    alpha)))]))

