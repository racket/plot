#lang typed/racket/base

(require typed/racket/class racket/match racket/list
         (only-in typed/pict pict)
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
(: polygons3d-render-proc (-> (-> (Listof (Listof (Vectorof Real))))
                             Plot-Color Plot-Brush-Style
                             Plot-Color Nonnegative-Real Plot-Pen-Style
                             Nonnegative-Real
                             (U String pict #f)
                             3D-Render-Proc))
(define ((polygons3d-render-proc vs-fun color style line-color line-width line-style alpha label)
         area)
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (for ([v (in-list (vs-fun))])
    (send area put-polygon v))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else   empty]))

(: polygons3d-renderer (-> (-> (Listof (Listof (Vectorof Real))))
                           (U #f Real) (U #f Real) (U #f Real) (U #f Real) (U #f Real) (U #f Real)
                           Plot-Color Plot-Brush-Style
                           Plot-Color Nonnegative-Real Plot-Pen-Style
                           Nonnegative-Real
                           (U String pict #f)
                           renderer3d))
(define (polygons3d-renderer vs-thnk x-min x-max y-min y-max z-min z-max
                             color style line-color line-width line-style alpha label)
  (define rvs (filter vrational? (apply append (vs-thnk))))
  (cond
    [(empty? rvs) (renderer3d #f #f #f #f)]
    [else
     (match-define (list (vector #{rxs : (Listof Real)}
                                 #{rys : (Listof Real)}
                                 #{rzs : (Listof Real)})
                         ...)
       rvs)
     (let ([x-min (or x-min (apply min* rxs))]
           [x-max (or x-max (apply max* rxs))]
           [y-min (or y-min (apply min* rys))]
           [y-max (or y-max (apply max* rys))]
           [z-min (or z-min (apply min* rzs))]
           [z-max (or z-max (apply max* rzs))])
       (renderer3d (vector (ivl x-min x-max)(ivl y-min y-max)(ivl z-min z-max))
                   #f ;surface3d-bounds-fun
                   default-ticks-fun
                   (polygons3d-render-proc vs-thnk
                                           color style line-color line-width line-style alpha label)))]))
(:: polygons3d
    (->* [(Sequenceof (Sequenceof (Sequenceof Real)))]
         [#:x-min (U #f Real) #:x-max (U #f Real)
          #:y-min (U #f Real) #:y-max (U #f Real)
          #:z-min (U #f Real) #:z-max (U #f Real)
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer3d))
(define (polygons3d vs
                    #:x-min [x-min #f] #:x-max [x-max #f]
                    #:y-min [y-min #f] #:y-max [y-max #f]
                    #:z-min [z-min #f] #:z-max [z-max #f]
                    #:color [color (surface-color)]
                    #:style [style (surface-style)]
                    #:line-color [line-color (surface-line-color)]
                    #:line-width [line-width (surface-line-width)]
                    #:line-style [line-style (surface-line-style)]
                    #:alpha [alpha (surface-alpha)]
                    #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'polygons3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([vs (for/list : (Listof (Listof (Vectorof Real)))
                 ([v vs])
                 (sequence->listof-vector 'polygons3d v 3))])
       (polygons3d-renderer (λ () vs)
                            x-min x-max y-min y-max z-min z-max
                            color style line-color line-width line-style alpha label))]))

(:: parametric-surface3d
    (->* [(-> Real Real (Sequenceof Real)) (U #f Real) (U #f Real) (U #f Real) (U #f Real)]
         [#:x-min (U #f Real) #:x-max (U #f Real)
          #:y-min (U #f Real) #:y-max (U #f Real)
          #:z-min (U #f Real) #:z-max (U #f Real)
          #:samples Positive-Integer
          #:s-samples Positive-Integer
          #:t-samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer3d))
(define (parametric-surface3d f s-min s-max t-min t-max
                              #:x-min [x-min #f] #:x-max [x-max #f]
                              #:y-min [y-min #f] #:y-max [y-max #f]
                              #:z-min [z-min #f] #:z-max [z-max #f]
                   #:samples [samples (plot3d-samples)]
                   #:s-samples [s-samples samples]
                   #:t-samples [t-samples samples]
                   #:color [color (surface-color)]
                   #:style [style (surface-style)]
                   #:line-color [line-color (surface-line-color)]
                   #:line-width [line-width (surface-line-width)]
                   #:line-style [line-style (surface-line-style)]
                   #:alpha [alpha (surface-alpha)]
                   #:label [label #f])
  (define fail/pos (make-raise-argument-error 'parametric-surface3d f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'parametric-surface3d))
  (cond
    [(not (rational? t-min))  (fail/pos "rational?" 1)]
    [(not (rational? t-max))  (fail/pos "rational?" 2)]
    [(not (rational? s-min))  (fail/pos "rational?" 3)]
    [(not (rational? s-max))  (fail/pos "rational?" 4)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(< samples 2)  (fail/kw "integer >= 2" '#:samples samples)]
    [(< s-samples 2)  (fail/kw "integer >= 2" '#:s-samples s-samples)]
    [(< t-samples 2)  (fail/kw "integer >= 2" '#:t-samples t-samples)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define vs (for/list : (Listof (Listof (Vectorof Real)))
                  ([s (in-list (linear-seq s-min s-max s-samples))])
                  (for/list : (Listof (Vectorof Real))
                    ([t (in-list (linear-seq t-min t-max t-samples))])
                    (sequence-head-vector 'parametric-surface3d (f s t) 3))))
     (define vs+
       (apply
        append
        (for/list : (Listof (Listof (Listof (Vectorof Real))))
          ([s0 (in-list vs)]
           [s1 (in-list (cdr vs))])
          (for/list : (Listof (Listof (Vectorof Real)))
            ([t0 (in-list s0)]
             [t1 (in-list s1)]
             [t2 (in-list (cdr s0))]
             [t3 (in-list (cdr s1))])
            (list t0 t1 t3 t2)))))
     (polygons3d-renderer
      (λ () vs+)
      x-min x-max y-min y-max z-min z-max
      color style line-color line-width line-style alpha label)]))
