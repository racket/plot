#lang typed/racket/base

(require racket/match racket/list typed/racket/class
         "../common/types.rkt"
         "../common/math.rkt"
         "../common/plot-element.rkt"
         "../common/nonrenderer.rkt"
         "../common/format.rkt"
         "../common/ticks.rkt"
         "../plot3d/plot-area.rkt"
         "../plot3d/renderer.rkt"
         "utils.rkt"
         typed/racket/unsafe)

(provide get-renderer-list get-bounds-rect get-ticks get-legend-list)
(unsafe-provide plot-area)

(: get-renderer-list (-> Any (Listof renderer3d)))
(define (get-renderer-list renderer-tree)
  (cond [(list? renderer-tree)  (append* (map get-renderer-list renderer-tree))]
        [(nonrenderer? renderer-tree)
         (match-define (nonrenderer bounds-rect bounds-fun ticks-fun) renderer-tree)
         (list (renderer3d bounds-rect bounds-fun ticks-fun #f #f))]
        [(renderer3d? renderer-tree)
         (list renderer-tree)]
        [else
         (raise-argument-error 'get-renderer-list "(or/c list? nonrenderer? renderer3d?)" renderer-tree)]))

(: get-bounds-rect (-> (Listof renderer3d)
                       (U #f Real) (U #f Real)
                       (U #f Real) (U #f Real)
                       (U #f Real) (U #f Real)
                       Rect))
(define (get-bounds-rect renderer-list x-min x-max y-min y-max z-min z-max)
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)))
  (let* ([plot-bounds-rect  (bounds-fixpoint renderer-list given-bounds-rect)]
         [plot-bounds-rect
          (cond [(not (rect-rational? plot-bounds-rect))
                 (match-define (vector x-ivl y-ivl z-ivl) plot-bounds-rect)
                 (error 'plot "could not determine sensible plot bounds; got x ∈ ~a, y ∈ ~a, z ∈ ~a"
                        (ivl->plot-label x-ivl) (ivl->plot-label y-ivl) (ivl->plot-label z-ivl))]
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

(: get-ticks (-> (Listof renderer3d) Rect
                 (Values (Listof tick) (Listof tick)
                         (Listof tick) (Listof tick)
                         (Listof tick) (Listof tick))))
(define (get-ticks renderer-list bounds-rect)
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks all-z-ticks all-z-far-ticks)
    (for/lists ([all-x-ticks : (Listof (Listof tick))]
                [all-x-far-ticks : (Listof (Listof tick))]
                [all-y-ticks : (Listof (Listof tick))]
                [all-y-far-ticks : (Listof (Listof tick))]
                [all-z-ticks : (Listof (Listof tick))]
                [all-z-far-ticks : (Listof (Listof tick))]
                ) ([r  (in-list renderer-list)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (match-define (list ts1 ts2 ts3 ts4 ts5 ts6) (ticks-fun bounds-rect))
                        (values ts1 ts2 ts3 ts4 ts5 ts6)]
            [else       (values empty empty empty empty empty empty)])))
  (values (remove-duplicates (append* all-x-ticks))
          (remove-duplicates (append* all-x-far-ticks))
          (remove-duplicates (append* all-y-ticks))
          (remove-duplicates (append* all-y-far-ticks))
          (remove-duplicates (append* all-z-ticks))
          (remove-duplicates (append* all-z-far-ticks))))

(: plot-area (-> (Instance 3D-Plot-Area%) (Listof renderer3d) Void))
(define (plot-area area renderer-list)
  (send area start-plot)
  
  (for ([rend  (in-list renderer-list)])
    (match-define (renderer3d rend-bounds-rect _bf _tf label-proc render-proc) rend)
    ;; next step could be moved into (when render-proc, but this generates different step files)
    (send area start-renderer (if rend-bounds-rect
                                  (rect-inexact->exact rend-bounds-rect)
                                  (unknown-rect 3)))
    (when render-proc
      (render-proc area)))
  
  (send area end-renderers)
  (send area end-plot))

(: get-legend-list (-> (Listof renderer3d) Rect (Listof legend-entry)))
(define (get-legend-list renderer-list outer-rect)
  (match-define (vector (ivl  x-min  x-max) (ivl  y-min  y-max) (ivl  z-min  z-max)) outer-rect)

  (cond
    [(and x-min x-max y-min y-max z-min z-max)

    (: clip (-> Rect Rect))
    (define (clip rect)
      (match-define (vector (ivl rx-min rx-max) (ivl ry-min ry-max) (ivl rz-min rz-max)) rect)
      (define cx-min (if rx-min (max* x-min rx-min) x-min))
      (define cx-max (if rx-max (min* x-max rx-max) x-max))
      (define cy-min (if ry-min (max* y-min ry-min) y-min))
      (define cy-max (if ry-max (min* y-max ry-max) y-max))
      (define cz-min (if rz-min (max* z-min rz-min) z-min))
      (define cz-max (if rz-max (min* z-max rz-max) z-max))
      (let ([cx-min  (min* cx-min cx-max)]
            [cx-max  (max* cx-min cx-max)]
            [cy-min  (min* cy-min cy-max)]
            [cy-max  (max* cy-min cy-max)]
            [cz-min  (min* cz-min cz-max)]
            [cz-max  (max* cz-min cz-max)])
        (vector (ivl cx-min cx-max)
                (ivl cy-min cy-max)
                (ivl cz-min cz-max))))
    (flatten-legend-entries
     (for*/list : (Listof (Treeof legend-entry))
       ([rend  (in-list renderer-list)]
        [label-proc (in-value (renderer3d-label rend))]
        #:when label-proc)
       (label-proc outer-rect)))]
    [else '()]))
