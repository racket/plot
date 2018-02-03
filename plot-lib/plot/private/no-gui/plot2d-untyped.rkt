#lang racket/base

(require racket/contract
         racket/class
         racket/draw
         pict
         "../../utils.rkt"
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
        void?)]
   [untyped-plot-bitmap
    (->* [(treeof (or/c renderer2d? nonrenderer?))]
         [#:x-min (or/c real? #f)
          #:x-max (or/c real? #f)
          #:y-min (or/c real? #f)
          #:y-max (or/c real? #f)
          #:title (or/c string? #f)
          #:height (or/c real? #f)
          #:width (or/c real? #f)
          #:x-label (or/c string? #f)
          #:y-label (or/c string? #f)
          #:legend-anchor anchor/c]
         (is-a?/c bitmap%))]
    [untyped-plot-pict
     (->* [(treeof (or/c renderer2d? nonrenderer?))]
          [#:x-min (or/c real? #f)
           #:x-max (or/c real? #f)
           #:y-min (or/c real? #f)
           #:y-max (or/c real? #f)
           #:title (or/c string? #f)
           #:x-label (or/c string? #f)
           #:height (or/c real? #f)
           #:width (or/c real? #f)
           #:y-label (or/c string? #f)
           #:legend-anchor anchor/c]
          pict?)]))

(define untyped-plot/dc plot/dc)
(define untyped-plot-pict plot-pict)
(define untyped-plot-bitmap plot-bitmap)
