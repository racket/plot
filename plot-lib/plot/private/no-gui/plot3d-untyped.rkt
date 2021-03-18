#lang racket/base

(require "plot3d.rkt"  ; must visit
         racket/contract
         racket/class
         racket/draw
         (only-in pict pict?)
         (for-syntax racket/base "plot3d-evil-box.rkt")
         "../../utils.rkt"
         "../common/contract.rkt"
         "../common/nonrenderer.rkt"
         "../plot3d/renderer.rkt")

(provide
 (contract-out
  [untyped-plot3d/dc
   (->* [(treeof (or/c renderer3d? nonrenderer?))
         (is-a?/c dc<%>)
         real?
         real?
         (>=/c 0)
         (>=/c 0)]
        [#:x-min (or/c real? #f)
         #:x-max (or/c real? #f)
         #:y-min (or/c real? #f)
         #:y-max (or/c real? #f)
         #:z-min (or/c real? #f)
         #:z-max (or/c real? #f)
         #:angle real?
         #:altitude real?
         #:title (or/c string? pict? #f)
         #:x-label (or/c string? pict? #f)
         #:y-label (or/c string? pict? #f)
         #:z-label (or/c string? pict? #f)
         #:aspect-ratio (or/c (and/c rational? positive?) #f)
         #:legend-anchor legend-anchor/c]
        plot-metrics<%>/c)]))

(define-syntax untyped-plot3d/dc
  (make-rename-transformer (unbox plot3d/dc-box)))
