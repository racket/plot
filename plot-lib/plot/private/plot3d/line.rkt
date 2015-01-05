#lang typed/racket/base

(require typed/racket/class racket/match racket/list
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(: lines3d-render-proc (-> (-> (Listof (Vectorof Real)))
                           Plot-Color Nonnegative-Real Plot-Pen-Style
                           Nonnegative-Real
                           (U String #f)
                           3D-Render-Proc))
(define ((lines3d-render-proc vs-fun color width style alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines (vs-fun))
  
  (cond [label  (line-legend-entry label color width style)]
        [else  empty]))

(: lines3d-renderer (-> (-> (Listof (Vectorof Real)))
                        (U #f Real) (U #f Real) (U #f Real) (U #f Real) (U #f Real) (U #f Real)
                        Plot-Color Nonnegative-Real Plot-Pen-Style
                        Nonnegative-Real
                        (U String #f)
                        renderer3d))
(define (lines3d-renderer
         vs-thnk x-min x-max y-min y-max z-min z-max color width style alpha label)
  (define rvs (filter vrational? (vs-thnk)))
  (cond [(empty? rvs)  (renderer3d #f #f #f #f)]
        [else
         (match-define (list (vector #{rxs : (Listof Real)}
                                     #{rys : (Listof Real)}
                                     #{rzs : (Listof Real)})
                             ...)
           rvs)
         (let ([x-min  (if x-min x-min (apply min* rxs))]
               [x-max  (if x-max x-max (apply max* rxs))]
               [y-min  (if y-min y-min (apply min* rys))]
               [y-max  (if y-max y-max (apply max* rys))]
               [z-min  (if z-min z-min (apply min* rzs))]
               [z-max  (if z-max z-max (apply max* rzs))])
           (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
                       default-ticks-fun
                       (lines3d-render-proc vs-thnk color width style alpha label)))]))

(:: lines3d
    (->* [(Sequenceof (Sequenceof Real))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (lines3d vs
                 #:x-min [x-min #f] #:x-max [x-max #f]
                 #:y-min [y-min #f] #:y-max [y-max #f]
                 #:z-min [z-min #f] #:z-max [z-max #f]
                 #:color [color (line-color)]
                 #:width [width (line-width)]
                 #:style [style (line-style)]
                 #:alpha [alpha (line-alpha)]
                 #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'lines3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(not (rational? width))  (fail/kw "rational" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([vs  (sequence->listof-vector 'lines3d vs 3)])
       (lines3d-renderer (λ () vs)
                         x-min x-max y-min y-max z-min z-max
                         color width style
                         alpha label))]))

(:: parametric3d
    (->* [(-> Real (Sequenceof Real)) Real Real]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (parametric3d f t-min t-max
                      #:x-min [x-min #f] #:x-max [x-max #f]
                      #:y-min [y-min #f] #:y-max [y-max #f]
                      #:z-min [z-min #f] #:z-max [z-max #f]
                      #:samples [samples (line-samples)]
                      #:color [color (line-color)]
                      #:width [width (line-width)]
                      #:style [style (line-style)]
                      #:alpha [alpha (line-alpha)]
                      #:label [label #f])
  (define fail/pos (make-raise-argument-error 'parametric3d f t-min t-max))
  (define fail/kw (make-raise-keyword-error 'parametric3d))
  (cond
    [(not (rational? t-min))  (fail/pos "rational?" 1)]
    [(not (rational? t-max))  (fail/pos "rational?" 2)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? width))  (fail/kw "rational" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([f  (λ ([t : Real]) (sequence-head-vector 'parametric3d (f t) 3))])
       (lines3d-renderer (λ () (map f (linear-seq t-min t-max (animated-samples samples))))
                         x-min x-max y-min y-max z-min z-max color width style alpha label))]))
