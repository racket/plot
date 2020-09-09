#lang typed/racket/base

(require "../common/plot-element.rkt"
         "../common/types.rkt"
         "../common/type-doc.rkt"
         "../common/math.rkt"
         "plot-area.rkt")

(provide (all-defined-out))

(deftype 3D-Render-Proc (-> (Instance 3D-Plot-Area%) Void))

(struct renderer3d plot-element ([label : (U #f (-> Rect (Treeof legend-entry)))]
                                 [render-proc : (U #f 3D-Render-Proc)])
  #:transparent)

(define empty-renderer3d (renderer3d #f #f #f #f #f))
