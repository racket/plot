#lang typed/racket/base

(require typed/racket/class racket/match
         "../common/plot-element.rkt"
         "../common/types.rkt"
         "../common/type-doc.rkt"
         "../common/math.rkt"
         "plot-area.rkt")

(provide (all-defined-out))

(deftype 2D-Render-Proc (-> (Instance 2D-Plot-Area%) Void))

(struct renderer2d plot-element ([label : (U #f (-> Rect (Treeof legend-entry)))]
                                 [render-proc : (U #f 2D-Render-Proc)])
  #:transparent)

(define empty-renderer2d (renderer2d #f #f #f #f #f))
