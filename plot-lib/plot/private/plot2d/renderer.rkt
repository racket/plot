#lang typed/racket/base

(require "../common/plot-element.rkt"
         "../common/types.rkt"
         "../common/type-doc.rkt"
         "plot-area.rkt")

(provide (all-defined-out))

(deftype 2D-Render-Proc (-> (Instance 2D-Plot-Area%) (Treeof legend-entry)))

(struct renderer2d plot-element ([render-proc : (U #f 2D-Render-Proc)]) #:transparent)
