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
  [plot/dc
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
         #:title (or/c string? pict? #f)
         #:x-label (or/c string? pict? #f)
         #:y-label (or/c string? pict? #f)
         #:aspect-ratio (or/c (and/c rational? positive?) #f)
         #:legend-anchor legend-anchor/c]
        plot-metrics-object/c)]
   [plot-bitmap
    (->* [(treeof (or/c renderer2d? nonrenderer?))]
         [#:x-min (or/c real? #f)
          #:x-max (or/c real? #f)
          #:y-min (or/c real? #f)
          #:y-max (or/c real? #f)
          #:title (or/c string? pict? #f)
          #:height (or/c real? #f)
          #:width (or/c real? #f)
          #:x-label (or/c string? pict? #f)
          #:y-label (or/c string? pict? #f)
          #:aspect-ratio (or/c (and/c rational? positive?) #f)
          #:legend-anchor legend-anchor/c]
         (and/c (is-a?/c bitmap%) plot-metrics-object/c))]
    [plot-pict
     (->* [(treeof (or/c renderer2d? nonrenderer?))]
          [#:x-min (or/c real? #f)
           #:x-max (or/c real? #f)
           #:y-min (or/c real? #f)
           #:y-max (or/c real? #f)
           #:title (or/c string? pict? #f)
           #:x-label (or/c string? pict? #f)
           #:height (or/c real? #f)
           #:width (or/c real? #f)
           #:y-label (or/c string? pict? #f)
           #:aspect-ratio (or/c (and/c rational? positive?) #f)
           #:legend-anchor legend-anchor/c]
          plot-pict?)]))

