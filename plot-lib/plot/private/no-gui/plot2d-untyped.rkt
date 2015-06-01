#lang racket/base

(require racket/contract
         racket/class
         racket/draw
         unstable/contract
         (for-syntax racket/base "plot2d-evil-box.rkt")
         "../common/contract.rkt"
         "../common/nonrenderer.rkt"
         "../plot2d/renderer.rkt"
         "plot2d.rkt")

(provide
 (contract-out
  [untyped-plot/dc
   (->* [(treeof (or/c renderer2d? nonrenderer?))
         (is-a?/c dc<%>)
         real?
         real?
         (>=/c 0)
         (>=/c 0)]
        [#:x-min (or/c real? #f)
         #:x-max (or/c real? #f)
         #:y-min (or/c real? #f)
         #:y-max (or/c real? #f)
         #:title (or/c string? #f)
         #:x-label (or/c string? #f)
         #:y-label (or/c string? #f)
         #:legend-anchor anchor/c]
        void?)]))

(define-syntax untyped-plot/dc
  (make-rename-transformer (unbox plot/dc-box)))
