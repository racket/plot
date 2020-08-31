#lang typed/racket/base

(require typed/racket/class racket/match racket/list racket/sequence
         (only-in typed/pict pict)
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide arrows3d-render-fun
         arrows3d)
;; ===================================================================================================
;; Arrows

(: arrows3d-render-fun
   (-> (Listof (Pair (Vectorof Real) (Vectorof Real)))
       Plot-Color Nonnegative-Real Plot-Pen-Style
       Nonnegative-Real
       (U (List '= Nonnegative-Real) Nonnegative-Real) Nonnegative-Real
       (U String pict #f)
       3D-Render-Proc))
(define ((arrows3d-render-fun vs
                              color line-width line-style
                              alpha
                              arrow-head-size-or-scale arrow-head-angle
                              label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) (send area get-bounds-rect))
  
  (cond
    [(and x-min x-max y-min y-max z-min z-max)  
     (send area put-alpha alpha)
     (send area put-pen color line-width line-style)
     (send area put-arrow-head arrow-head-size-or-scale arrow-head-angle)
     (for ([x (in-list vs)])
       (send area put-arrow (car x) (cdr x) #t))
     (cond [label  (arrow-legend-entry label color line-width line-style)]
           [else   empty])]
    [else  empty])
  )


(define-type LVof (All (A) (U (Listof A)(Vectorof A))))
(:: arrows3d
    (->* [(U (LVof (LVof Real))
             (LVof (LVof (LVof Real))))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:arrow-head-size-or-scale (U (List '= Nonnegative-Real) Nonnegative-Real)
          #:arrow-head-angle Nonnegative-Real
          #:label (U String pict #f)]
         renderer3d))
(define (arrows3d vs
                  #:x-min [x-min #f] #:x-max [x-max #f]
                  #:y-min [y-min #f] #:y-max [y-max #f]
                  #:z-min [z-min #f] #:z-max [z-max #f]
                  #:color [color (arrows-color)]
                  #:width [width (arrows-line-width)]
                  #:style [style (arrows-line-style)]
                  #:alpha [alpha (arrows-alpha)]
                  #:arrow-head-size-or-scale [arrow-head-size-or-scale (arrow-head-size-or-scale)]
                  #:arrow-head-angle [arrow-head-angle (arrow-head-angle)]
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
     (define (argument-error)
       (raise-argument-error
        'arrows3d
        "(U (Sequenceof (Sequence Real Real Real)) (Sequenceof (Sequence (Sequence Real Real Real) (Sequence Real Real Real))))"
        vs))
     
     ;; check if we have head/tail or pair vectors, and put in standard format
     (define-values (S1 S2)
       (for/fold ([S1 : (Listof (Vectorof Real)) '()]
                  [S2 : (Listof (Pair (Vectorof Real) (Vectorof Real))) '()])
                 ([s vs])
         (define l (sequence->list s))
         (cond
           [(andmap real? l)
            (values (cons (sequence-head-vector 'arrows3d l 3) S1) S2)]
           [(and (andmap sequence? l) (= (length l) 2))
            (define v1 (sequence-head-vector 'arrows3d (car l) 3))
            (define v2 (sequence-head-vector 'arrows3d (cadr l) 3))
            (define v3 (vector (+ (vector-ref v1 0) (vector-ref v2 0))
                               (+ (vector-ref v1 1) (vector-ref v2 1))
                               (+ (vector-ref v1 2) (vector-ref v2 2))))
            (values S1
                    (cons (cons v1 v3) S2))]
           [else (argument-error)])))

     (define vs*
       (cond
         [(empty? S2)
          (define S1* (reverse S1))
          (for/list : (Listof (Pair (Vectorof Real) (Vectorof Real)))
            ([v1 (in-list S1*)]
             [v2 (in-list (cdr S1*))])
            (cons v1 v2))]
         [else S2]))

     ;; calculate bound and pick right render-fun
     (define rvs
       (let ()
         (match-define (list (cons #{p1 : (Listof (Vectorof Real))}
                                   #{p2 : (Listof (Vectorof Real))}) ...)
           vs*)
         (filter vrational? (append p1 p2))))

     (cond
       [(empty? rvs) (renderer3d #f #f #f #f)]
       [else
        (define-values (x- x+ y- y+ z- z+) (get-bounds x-min x-max y-min y-max z-min z-max rvs))
        (renderer3d (vector (ivl x- x+) (ivl y- y+) (ivl z- z+)) #f default-ticks-fun
                    (arrows3d-render-fun vs*
                                         color width style alpha
                                         arrow-head-size-or-scale arrow-head-angle
                                         label))])]))


(define (get-bounds [x-min : (Option Real)][x-max : (Option Real)]
                    [y-min : (Option Real)][y-max : (Option Real)]
                    [z-min : (Option Real)][z-max : (Option Real)]
                    [rvs : (Listof (Vectorof Real))])
  (match-define (list (vector #{rxs : (Listof Real)}
                              #{rys : (Listof Real)}
                              #{rzs : (Listof Real)}) ...) rvs)
  (values (if x-min x-min (apply min* rxs))
          (if x-max x-max (apply max* rxs))
          (if y-min y-min (apply min* rys))
          (if y-max y-max (apply max* rys))
          (if z-min z-min (apply min* rzs))
          (if z-max z-max (apply max* rzs))))