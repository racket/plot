#lang typed/racket/base

(require racket/list typed/racket/class racket/match
         "../common/math.rkt"
         "../common/plot-element.rkt"
         "../common/nonrenderer.rkt"
         "../common/format.rkt"
         "../common/types.rkt"
         "../common/ticks.rkt"
         "../plot2d/plot-area.rkt"
         "../plot2d/renderer.rkt"
         "utils.rkt")

(provide (all-defined-out))

(: get-renderer-list (-> (Treeof (U renderer2d nonrenderer)) (Listof renderer2d)))
(define (get-renderer-list renderer-tree)
  (cond [(list? renderer-tree)  (append* (map get-renderer-list renderer-tree))]
        [(nonrenderer? renderer-tree)
         (match-define (nonrenderer bounds-rect bounds-fun ticks-fun) renderer-tree)
         (list (renderer2d bounds-rect bounds-fun ticks-fun #f))]
        [else
         (list renderer-tree)]))

(: get-bounds-rect (-> (Listof renderer2d) (U #f Real) (U #f Real) (U #f Real) (U #f Real) Rect))
(define (get-bounds-rect renderer-list x-min x-max y-min y-max)
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max)))
  (let* ([plot-bounds-rect  (bounds-fixpoint renderer-list given-bounds-rect)]
         [plot-bounds-rect
          (cond [(not (rect-rational? plot-bounds-rect))
                 (match-define (vector x-ivl y-ivl) plot-bounds-rect)
                 (error 'plot "could not determine sensible plot bounds; got x ∈ ~a, y ∈ ~a"
                        (ivl->plot-label x-ivl) (ivl->plot-label y-ivl))]
                [(rect-zero-area? plot-bounds-rect)
                 (for/vector ([i  (in-vector plot-bounds-rect)]) : ivl
                   (match-define (ivl a b) i)
                   (with-asserts ([a values] [b values])
                     (cond [(= a b)  (cond [(zero? a)  (ivl -1 1)]
                                           [else  (ivl (* a (- 1 1e-2)) (* b (+ 1 1e-2)))])]
                           [else  i])))]
                [else
                 plot-bounds-rect])])
    (rect-inexact->exact plot-bounds-rect)))

(: get-ticks (-> (Listof renderer2d) Rect
                 (Values (Listof tick) (Listof tick) (Listof tick) (Listof tick))))
(define (get-ticks renderer-list bounds-rect)
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks)
    (for/lists ([all-x-ticks : (Listof (Listof tick))]
                [all-x-far-ticks : (Listof (Listof tick))]
                [all-y-ticks : (Listof (Listof tick))]
                [all-y-far-ticks : (Listof (Listof tick))]
                ) ([r  (in-list renderer-list)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (match-define (list ts1 ts2 ts3 ts4) (ticks-fun bounds-rect))
                        (values ts1 ts2 ts3 ts4)]
            [else       (values empty empty empty empty)])))
  (values (remove-duplicates (append* all-x-ticks))
          (remove-duplicates (append* all-x-far-ticks))
          (remove-duplicates (append* all-y-ticks))
          (remove-duplicates (append* all-y-far-ticks))))

(: plot-area (-> (Instance 2D-Plot-Area%) (Listof renderer2d) Void))
(define (plot-area area renderer-list)
  (send area start-plot)
  
  (define legend-entries
    (flatten-legend-entries
     (for/list : (Listof (Treeof legend-entry)) ([rend  (in-list renderer-list)])
       (match-define (renderer2d rend-bounds-rect _bf _tf render-proc) rend)
       (send area start-renderer (if rend-bounds-rect
                                     (rect-inexact->exact rend-bounds-rect)
                                     (unknown-rect 2)))
       (if render-proc (render-proc area) empty))))
  
  (send area end-renderers)
  
  (when (not (empty? legend-entries))
    (send area draw-legend legend-entries))
  
  (send area end-plot))
