#lang racket/base

(require "plot3d.rkt"  ; must visit
         racket/contract
         racket/class
         racket/draw
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
         #:title (or/c string? #f)
         #:x-label (or/c string? #f)
         #:y-label (or/c string? #f)
         #:z-label (or/c string? #f)
         #:legend-anchor anchor/c]
        void?)]))

(define-syntax untyped-plot3d/dc
  (make-rename-transformer (unbox plot3d/dc-box)))
