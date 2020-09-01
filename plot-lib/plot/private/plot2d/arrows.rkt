#lang typed/racket/base

(require typed/racket/class racket/match racket/math racket/list racket/sequence
         plot/utils
         (only-in typed/pict pict)
         (only-in math/statistics stddev)
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide arrows-render-fun
         arrows)

;; ===================================================================================================
;; Arrows

(: arrows-render-fun
   (-> (Listof (Pair (Vectorof Real) (Vectorof Real)))
       Plot-Color Nonnegative-Real Plot-Pen-Style
       Nonnegative-Real
       (U (List '= Nonnegative-Real) Nonnegative-Real) Nonnegative-Real
       (U String pict #f)
       2D-Render-Proc))
(define ((arrows-render-fun vs
                            color line-width line-style
                            alpha
                            arrow-head-size-or-scale arrow-head-angle
                            label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  
  (cond
    [(and x-min x-max y-min y-max)  
     (send area put-alpha alpha)
     (send area put-pen color line-width line-style)
     (send area put-arrow-head arrow-head-size-or-scale arrow-head-angle)
     (for ([x (in-list vs)])
       (match-define (cons (vector v1x v1y) (vector v2x v2y)) x)
       (define (f [x : Real][y : Real])
         (- (* (- v2x v1x) (- y v1y))
            (* (- v2y v1y) (- x v1x))))
       (cond
        ;only draw the arrow head if the endpoint is visible
        ;other option makes little sense since size and angle can be/is plot units-absolute
        [(and (<= x-min v2x x-max) (<= y-min v2y y-max))
         (send area put-arrow (car x) (cdr x) #t)]
        ;only draw the vector line if it is visible
        [(or (<= (* (f x-min y-min) (f x-max y-max)) 0)
             (<= (* (f x-min y-max) (f x-max y-min)) 0))
         (send area put-lines (list (car x) (cdr x)))]))
     (cond [label  (arrow-legend-entry label color line-width line-style)]
           [else   empty])]
    [else  empty])
  )

;(Sequenceof (Sequence (Sequenceof Real))) does not work because (sequence? 5) = #t
;and so the contract can not know if '((2 2)) is three or two nested sequences
(define-type LVof (All (A) (U (Listof A) (Vectorof A))))
(:: arrows
    (->* [(U (LVof (LVof Real))
             (LVof (LVof (LVof Real))))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:arrow-head-size-or-scale (U (List '= Nonnegative-Real) Nonnegative-Real)
          #:arrow-head-angle Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (arrows vs
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f]
                #:color [color (arrows-color)]
                #:width [width (arrows-line-width)]
                #:style [style (arrows-line-style)]
                #:alpha [alpha (arrows-alpha)]
                #:arrow-head-size-or-scale [arrow-head-size-or-scale (arrow-head-size-or-scale)]
                #:arrow-head-angle [arrow-head-angle (arrow-head-angle)]
                #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'lines))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]

    [else
     (define (argument-error)
       (raise-argument-error
        'arrows
        "(U (Sequenceof (Sequence Real Real)) (Sequenceof (Sequence (Sequence Real Real) (Sequence Real Real))))"
        vs))
     ;; check if we have head/tail or pair vectors, and put in standard format
     (define-values (S1 S2)
       (for/fold ([S1 : (Listof (Vectorof Real)) '()]
                  [S2 : (Listof (Pair (Vectorof Real) (Vectorof Real))) '()])
                 ([s vs])
         (define l (sequence->list s))
         (cond
           [(not  (= (length l) 2))
            (argument-error)]
           ;order important because integers are sequence?
           [(andmap real? l)
            (values (cons (sequence-head-vector 'arrows l 2) S1) S2)]
           [(andmap sequence? l)
            (define v1 (sequence-head-vector 'arrows (car l) 2))
            (define v2 (sequence-head-vector 'arrows (cadr l) 2))
            (define v3 (vector (+ (vector-ref v1 0) (vector-ref v2 0))
                               (+ (vector-ref v1 1) (vector-ref v2 1))))
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
       [(empty? rvs) (renderer2d #f #f #f #f)]
       [else
        (define-values (x- x+ y- y+) (get-bounds x-min x-max y-min y-max rvs))
        (renderer2d (vector (ivl x- x+) (ivl y- y+)) #f default-ticks-fun
                    (arrows-render-fun vs*
                                       color width style alpha
                                       arrow-head-size-or-scale arrow-head-angle
                                       label))])]))

(define (get-bounds [x-min : (Option Real)][x-max : (Option Real)]
                    [y-min : (Option Real)][y-max : (Option Real)]
                    [rvs : (Listof (Vectorof Real))])
  (match-define (list (vector #{rxs : (Listof Real)} #{rys : (Listof Real)}) ...) rvs)
  (values (if x-min x-min (apply min* rxs))
          (if x-max x-max (apply max* rxs))
          (if y-min y-min (apply min* rys))
          (if y-max y-max (apply max* rys))))