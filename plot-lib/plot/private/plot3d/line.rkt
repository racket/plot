#lang typed/racket/base

(require typed/racket/class racket/match racket/list
         (only-in typed/pict pict)
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (except-out (all-defined-out) define-lines-like))

;; ===================================================================================================

(define-type Lines-Render-Proc
  (-> (-> (Listof (Vectorof Real)))
      Plot-Color Nonnegative-Real Plot-Pen-Style
      Nonnegative-Real
      (U String pict #f)
      3D-Render-Proc))

(: lines3d-render-proc Lines-Render-Proc)
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
                        (U String pict #f)
                        Lines-Render-Proc
                        renderer3d))
(define (lines3d-renderer
         vs-thnk x-min x-max y-min y-max z-min z-max color width style alpha label lines-render-proc)
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
                       (lines-render-proc vs-thnk color width style alpha label)))]))

(define-syntax-rule (define-lines-like name renderer)
  (begin
    (:: name
        (->* [(Sequenceof (Sequenceof Real))]
             [#:x-min (U Real #f) #:x-max (U Real #f)
              #:y-min (U Real #f) #:y-max (U Real #f)
              #:z-min (U Real #f) #:z-max (U Real #f)
              #:color Plot-Color
              #:width Nonnegative-Real
              #:style Plot-Pen-Style
              #:alpha Nonnegative-Real
              #:label (U String pict #f)]
             renderer3d))
    (define (name vs
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
                             alpha label
                             renderer))]))))

(define-lines-like lines3d lines3d-render-proc)

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
          #:label (U String pict #f)]
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
                         x-min x-max y-min y-max z-min z-max color width style alpha label
                         lines3d-render-proc))]))

;; ===================================================================================================
;; Arrows

(: arrows3d-render-fun
   (-> (-> (Listof (Vectorof Real)))
       Plot-Color Nonnegative-Real Plot-Pen-Style
       Nonnegative-Real
       (U String pict #f)
       3D-Render-Proc))
(define ((arrows3d-render-fun vs-fct
                              color line-width line-style
                              alpha
                              label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)(ivl z-min z-max)) (send area get-bounds-rect))
  
  (cond
    [(and x-min x-max y-min y-max z-min z-max)  
     (send area put-alpha alpha)
     (send area put-pen color line-width line-style)
     (define vs (vs-fct))
     (for ([x0 (in-list vs)]
           [x1 (in-list (cdr vs))])
       (send area put-arrow x0 x1))
     (cond [label  (arrow-legend-entry label color line-width line-style)]
           [else   empty])]
    [else  empty])
  )

(define-lines-like arrows3d arrows3d-render-fun)
#|
(: get-min-max3d (-> (Pairof (Vector (Vectorof Real) (Vectorof Real)) (Listof (Vector (Vectorof Real) (Vectorof Real))))
                   (values Real Real Real Real Real Real)))
(define(get-min-max3d vs)
  (define a0 (car vs))
  (define a0-start (vector-ref a0 0))
  (define a0-end (vector-ref a0 1))
  (for/fold([x-min (min (vector-ref a0-start 0) (vector-ref a0-end 0))]
            [y-min (min (vector-ref a0-start 1) (vector-ref a0-end 1))]
            [z-min (min (vector-ref a0-start 2) (vector-ref a0-end 2))]
            [x-max (max (vector-ref a0-start 0) (vector-ref a0-end 0))]
            [y-max (max (vector-ref a0-start 1) (vector-ref a0-end 1))]
            [z-max (max (vector-ref a0-start 2) (vector-ref a0-end 2))])
           ([t (in-list (cdr vs))])
    (define t-start (vector-ref t 0))
    (define t-end (vector-ref t 1))
    (values (min x-min (vector-ref t-start 0) (vector-ref t-end 0))
            (min y-min (vector-ref t-start 1) (vector-ref t-end 1))
            (min z-min (vector-ref t-start 2) (vector-ref t-end 2))
            (max x-max (vector-ref t-start 0) (vector-ref t-end 0))
            (max y-max (vector-ref t-start 1) (vector-ref t-end 1))
            (max z-max (vector-ref t-start 2) (vector-ref t-end 2)))))

 (: arrows3d
    (->* [(Pairof (Vector (Vectorof Real) (Vectorof Real))
                  (Listof (Vector  (Vectorof Real)(Vectorof Real))))]
         [#:color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))	 
(define (arrows3d vs #:color [color (vector-field-color)]
                  #:line-width [line-width (vector-field-line-width)]
                  #:line-style [line-style (vector-field-line-style)]
                  #:alpha [alpha (vector-field-alpha)]
                  #:label [label #f])
  (define fail/pos (make-raise-argument-error 'arrows vs))
  (define fail/kw (make-raise-keyword-error 'arrows))
  (cond
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
    (define-values (x-min y-min z-min x-max y-max z-max)
      (get-min-max3d vs))
    (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f default-ticks-fun
                (arrows3d-render-fun
                 vs color line-width line-style alpha label))]))
|#

