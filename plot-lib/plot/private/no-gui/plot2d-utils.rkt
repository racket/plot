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
         "utils.rkt"
         typed/racket/unsafe)

(provide get-renderer-list get-bounds-rect get-ticks get-legend-list)
(unsafe-provide plot-area)

(: get-renderer-list (-> Any (Listof renderer2d)))
(define (get-renderer-list renderer-tree)
  (cond [(list? renderer-tree)  (append* (map get-renderer-list renderer-tree))]
        [(nonrenderer? renderer-tree)
         (match-define (nonrenderer bounds-rect bounds-fun ticks-fun) renderer-tree)
         (list (renderer2d bounds-rect bounds-fun ticks-fun #f #f))]
        [(renderer2d? renderer-tree)
         (list renderer-tree)]
        [else
         (raise-argument-error 'get-renderer-list "(or/c list? nonrenderer? renderer2d?)" renderer-tree)]))

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
  
  (for ([rend  (in-list renderer-list)])
    (match-define (renderer2d rend-bounds-rect _bf _tf label-proc render-proc) rend)
    (when render-proc
      (send area start-renderer (if rend-bounds-rect
                                    (rect-inexact->exact rend-bounds-rect)
                                    (unknown-rect 2)))
      (render-proc area)))
  
  (send area end-renderers)
  (send area end-plot))

(: get-legend-list (-> (Listof renderer2d) Rect (Listof legend-entry)))
(define (get-legend-list renderer-list outer-rect)
  (match-define (vector (ivl  x-min  x-max) (ivl  y-min  y-max)) outer-rect)

  (cond
    [(and x-min x-max y-min y-max)

     (: clip (-> Rect Rect))
     (define (clip rect)
       (match-define (vector (ivl rx-min rx-max) (ivl ry-min ry-max)) rect)
       (define cx-min (if rx-min (max* x-min rx-min) x-min))
       (define cx-max (if rx-max (min* x-max rx-max) x-max))
       (define cy-min (if ry-min (max* y-min ry-min) y-min))
       (define cy-max (if ry-max (min* y-max ry-max) y-max))
       (let ([cx-min  (min* cx-min cx-max)]
             [cx-max  (max* cx-min cx-max)]
             [cy-min  (min* cy-min cy-max)]
             [cy-max  (max* cy-min cy-max)])
         (vector (ivl cx-min cx-max)
                 (ivl cy-min cy-max))))

     (flatten-legend-entries
      (for*/list : (Listof (Treeof legend-entry))
        ([rend  (in-list renderer-list)]
         [rect (in-value (let ([r (plot-element-bounds-rect rend)])
                           (or (and r (clip r)) outer-rect)))]
         [label-proc (in-value (renderer2d-label rend))]
         #:when label-proc)
        (label-proc rect)))]
    [else '()]))
