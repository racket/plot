#lang typed/racket/base

(require "../common/plot-element.rkt"
         "../common/types.rkt"
         "../common/type-doc.rkt"
         "plot-area.rkt")

(provide (all-defined-out))

(deftype 3D-Render-Proc (-> (Instance 3D-Plot-Area%) (Treeof legend-entry)))

(struct renderer3d plot-element ([render-proc : (U #f 3D-Render-Proc)]) #:transparent)
