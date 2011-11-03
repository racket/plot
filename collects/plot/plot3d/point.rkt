#lang racket/base

(require racket/class racket/list racket/match racket/contract
         plot/utils
         "../common/contract-doc.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(define ((points3d-render-proc vs sym color size line-width alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color line-width 'solid)
  (send area put-glyphs vs sym size)
  
  (cond [label  (point-legend-entry label sym color size line-width)]
        [else   empty]))

(defproc (points3d
          [vs  (listof (vector/c real? real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:sym sym point-sym/c (point-sym)]
          [#:color color plot-color/c (point-color)]
          [#:size size (>=/c 0) (point-size)]
          [#:line-width line-width (>=/c 0) (point-line-width)]
          [#:alpha alpha (real-in 0 1) (point-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (let ([vs  (filter vregular? vs)])
    (cond [(empty? vs)  (renderer3d #f #f #f #f)]
          [else
           (match-define (list (vector xs ys zs) ...) vs)
           (let ([x-min  (if x-min x-min (apply min* xs))]
                 [x-max  (if x-max x-max (apply max* xs))]
                 [y-min  (if y-min y-min (apply min* ys))]
                 [y-max  (if y-max y-max (apply max* ys))]
                 [z-min  (if z-min z-min (apply min* zs))]
                 [z-max  (if z-max z-max (apply max* zs))])
             (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
                         default-ticks-fun
                         (points3d-render-proc vs sym color size line-width alpha label)))])))