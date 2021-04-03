#lang typed/racket/base #:no-optimize

(require typed/racket/unsafe
         typed/racket/class
         "../plot2d/plot-area.rkt"
         "../plot2d/renderer.rkt"
         (rename-in "plot2d-utils-typed.rkt" [plot-area plot-area*]))

(provide (except-out (all-from-out "plot2d-utils-typed.rkt")
                     plot-area*))
(unsafe-provide plot-area)

(: plot-area (-> (Instance 2D-Plot-Area%) (Listof renderer2d) Void))
(define (plot-area area renderer-list)
  (unless (is-a? area 2d-plot-area%)
    (raise-argument-error 'plot-area "(is-a? 2d-plot-area%)" 0 area renderer-list))
  (unless (and (list? renderer-list)
               (andmap renderer2d? renderer-list))
    (raise-argument-error 'plot-area "(listof renderer2d?)" 1 area renderer-list))
  (plot-area* area renderer-list))
