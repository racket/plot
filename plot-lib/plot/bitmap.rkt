#lang racket/base

(require typed/untyped-utils
         "no-gui.rkt"
         "private/no-gui/plot-bitmap.rkt")
(provide (all-from-out "no-gui.rkt"))

(module untyped racket/base
  (require "private/no-gui/plot-bitmap.rkt" racket/contract
           
           racket/class
           racket/draw
           pict
           "utils.rkt"
           "private/common/contract.rkt"
           "private/common/nonrenderer.rkt"
           "private/plot2d/renderer.rkt")
  (provide
   (contract-out
    [plot
     (->* [(treeof (or/c renderer2d? nonrenderer?))]
          [#:x-min (or/c real? #f)
           #:x-max (or/c real? #f)
           #:y-min (or/c real? #f)
           #:y-max (or/c real? #f)
           #:width (and/c exact-integer? (>/c 0))
           #:height (and/c exact-integer? (>/c 0))
           #:title (or/c string? #f)
           #:x-label (or/c string? #f)
           #:y-label (or/c string? #f)
           #:aspect-ratio (or/c (and/c rational? positive?) #f)
           #:legend-anchor anchor/c
           #:out-file (or/c path? string? output-port? #f)
           #:out-kind symbol?]
          (is-a?/c bitmap%))]
    [plot3d
     (->* [(treeof (or/c renderer3d? nonrenderer?))]
          [#:x-min (or/c real? #f)
           #:x-max (or/c real? #f)
           #:y-min (or/c real? #f)
           #:y-max (or/c real? #f)
           #:z-min (or/c real? #f)
           #:z-max (or/c real? #f)
           #:width (and/c exact-integer? (>/c 0))
           #:height (and/c exact-integer? (>/c 0))
           #:angle real? #:altitude real?
           #:title (or/c string? #f)
           #:x-label (or/c string? #f)
           #:y-label (or/c string? #f)
           #:z-label (or/c string? #f)
           #:aspect-ratio (or/c (and/c rational? positive?) #f)
           #:legend-anchor anchor/c
           #:out-file (or/c path? string? output-port? #f)
           #:out-kind symbol?]
          (is-a?/c bitmap%))])))


(require (rename-in "private/no-gui/plot-bitmap.rkt"
                    [plot typed-plot]
                    [plot3d typed-plot3d])
         (rename-in (submod "." untyped)
                    [plot untyped-plot]
                    [plot3d untyped-plot3d]))

(define-typed/untyped-identifier plot
  typed-plot
  untyped-plot)
(define-typed/untyped-identifier plot3d
  typed-plot3d
  untyped-plot3d)

(provide plot plot3d)

