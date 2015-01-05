#lang typed/racket/base

(require racket/list
         "../common/types.rkt")

(provide (all-defined-out))

(: flatten-legend-entries (-> (Treeof legend-entry) (Listof legend-entry)))
(define (flatten-legend-entries ls)
  (cond [(list? ls)  (append* (map flatten-legend-entries ls))]
        [else  (list ls)]))
