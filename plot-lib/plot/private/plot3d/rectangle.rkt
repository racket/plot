#lang typed/racket/base

(require racket/match racket/list typed/racket/class racket/sequence
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Rectangles

(: rectangles3d-render-proc (-> (Listof (Vectorof ivl))
                                Plot-Color Plot-Brush-Style
                                Plot-Color Nonnegative-Real Plot-Pen-Style
                                Nonnegative-Real
                                (U String #f)
                                3D-Render-Proc))
(define ((rectangles3d-render-proc rects color style line-color line-width line-style alpha label)
         area)
  (send area put-pen line-color line-width line-style)
  (send area put-brush color style)
  (send area put-alpha alpha)
  (for ([rect  (in-list rects)])
    (send area put-rect rect))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else  empty]))

(:: rectangles3d
    (->* [(Sequenceof (Sequenceof ivl))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (rectangles3d rects
                      #:x-min [x-min #f] #:x-max [x-max #f]
                      #:y-min [y-min #f] #:y-max [y-max #f]
                      #:z-min [z-min #f] #:z-max [z-max #f]
                      #:color [color (rectangle-color)]
                      #:style [style (rectangle-style)]
                      #:line-color [line-color (rectangle-line-color)]
                      #:line-width [line-width (rectangle3d-line-width)]
                      #:line-style [line-style (rectangle-line-style)]
                      #:alpha [alpha (rectangle-alpha)]
                      #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'rectangles3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([rects  (sequence->listof-vector 'rectangles3d rects 3)])
       (match-define (list (vector (ivl #{x1s : (Listof (U Real #f))}
                                        #{x2s : (Listof (U Real #f))})
                                   (ivl #{y1s : (Listof (U Real #f))}
                                        #{y2s : (Listof (U Real #f))})
                                   (ivl #{z1s : (Listof (U Real #f))}
                                        #{z2s : (Listof (U Real #f))}))
                           ...)
         rects)
       (define rxs (filter rational? (append x1s x2s)))
       (define rys (filter rational? (append y1s y2s)))
       (define rzs (filter rational? (append z1s z2s)))
       (cond
         [(or (empty? rxs) (empty? rys) (empty? rzs))  (renderer3d #f #f #f #f)]
         [else
          (let ([x-min  (if x-min x-min (apply min* rxs))]
                [x-max  (if x-max x-max (apply max* rxs))]
                [y-min  (if y-min y-min (apply min* rys))]
                [y-max  (if y-max y-max (apply max* rys))]
                [z-min  (if z-min z-min (apply min* rzs))]
                [z-max  (if z-max z-max (apply max* rzs))])
            (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
                        default-ticks-fun
                        (rectangles3d-render-proc rects color style line-color line-width line-style
                                                  alpha label)))]))]))

;; ===================================================================================================
;; Discrete histograms

(: discrete-histogram3d-ticks-fun
   (-> (Listof Any) (Listof Any) (Listof Real) (Listof Real) Boolean Boolean Boolean Boolean
       Ticks-Fun))
(define ((discrete-histogram3d-ticks-fun
          c1s c2s tick-xs tick-ys add-x-ticks? add-y-ticks? x-far-ticks? y-far-ticks?) r)
  (match-define (vector _xi _yi (ivl z-min z-max)) r)
  (define-values (x-ticks x-far-ticks)
    (let ([ts  (cond [add-x-ticks?  (for/list : (Listof tick) ([cat  (in-list c1s)]
                                                               [x  (in-list tick-xs)])
                                      (tick x #t (->plot-label cat)))]
                     [else  empty])])
      (if x-far-ticks? (values empty ts) (values ts empty))))
  (define-values (y-ticks y-far-ticks)
    (let ([ts  (cond [add-y-ticks?  (for/list : (Listof tick) ([cat  (in-list c2s)]
                                                               [y  (in-list tick-ys)])
                                      (tick y #t (->plot-label cat)))]
                     [else  empty])])
      (if y-far-ticks? (values empty ts) (values ts empty))))
  (list x-ticks x-far-ticks
        y-ticks y-far-ticks
        (if (and z-min z-max) (ticks-generate (plot-z-ticks) z-min z-max) empty)
        (if (and z-min z-max) (ticks-generate (plot-z-far-ticks) z-min z-max) empty)))

(: ivl/gap (-> Real Real Real ivl))
(define (ivl/gap x1 x2 gap)
  (define 1/2-gap-size (* 1/2 gap (- x2 x1)))
  (ivl (+ x1 1/2-gap-size) (- x2 1/2-gap-size)))

(:: discrete-histogram3d
    (->* [(Sequenceof (U (Vector Any Any (U Real ivl #f))
                         (List Any Any (U Real ivl #f))))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:gap Nonnegative-Real
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)
          #:add-x-ticks? Boolean
          #:add-y-ticks? Boolean
          #:x-far-ticks? Boolean
          #:y-far-ticks? Boolean]
         renderer3d))
(define (discrete-histogram3d
         cat-vals
         #:x-min [x-min 0] #:x-max [x-max #f]
         #:y-min [y-min 0] #:y-max [y-max #f]
         #:z-min [z-min 0] #:z-max [z-max #f]
         #:gap [gap (discrete-histogram-gap)]
         #:color [color (rectangle-color)]
         #:style [style (rectangle-style)]
         #:line-color [line-color (rectangle-line-color)]
         #:line-width [line-width (rectangle3d-line-width)]
         #:line-style [line-style (rectangle-line-style)]
         #:alpha [alpha (rectangle-alpha)]
         #:label [label #f]
         #:add-x-ticks? [add-x-ticks? #t]
         #:add-y-ticks? [add-y-ticks? #t]
         #:x-far-ticks? [x-far-ticks? #f]
         #:y-far-ticks? [y-far-ticks? #f])
  (define fail/kw (make-raise-keyword-error 'discrete-histogram3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(or (> gap 1) (not (rational? gap)))  (fail/kw "real in [0,1]" '#:gap gap)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([cat-vals  (sequence->list cat-vals)])
       (match-define (list (or (vector #{cat1s : (Listof Any)}
                                       #{cat2s : (Listof Any)}
                                       #{zs : (Listof (U Real ivl #f))})
                               (list #{cat1s : (Listof Any)}
                                     #{cat2s : (Listof Any)}
                                     #{zs : (Listof (U Real ivl #f))}))
                           ...)
         cat-vals)
       (define rzs
         (filter rational?
                 (append*
                  (for/list : (Listof (Listof (U Real #f))) ([z  (in-list zs)])
                    (cond [(ivl? z)  (match-define (ivl z1 z2) z)
                                     (list z1 z2)]
                          [else  (list z)])))))
       (cond
         [(empty? rzs)  (renderer3d #f #f #f #f)]
         [else
          (define c1s (remove-duplicates cat1s))
          (define c2s (remove-duplicates cat2s))
          (define x-num (length c1s))
          (define y-num (length c2s))
          (let* ([x-min  (if x-min x-min 0)]
                 [x-max  (if x-max x-max (+ x-min x-num))]
                 [y-min  (if y-min y-min 0)]
                 [y-max  (if y-max y-max (+ y-min y-num))]
                 [z-min  (if z-min z-min (apply min* rzs))]
                 [z-max  (if z-max z-max (apply max* rzs))])
            (define xs (linear-seq x-min x-max (add1 x-num)))
            (define ys (linear-seq y-min y-max (add1 y-num)))
            (: h (HashTable (Pair Any Any) (U Real ivl #f)))
            (define h
              (make-immutable-hash
               (for/list : (Listof (Pair (Pair Any Any) (U Real ivl #f))) ([c1 (in-list cat1s)]
                                                                           [c2 (in-list cat2s)]
                                                                           [z (in-list zs)])
                 (pair (pair c1 c2) z))))
            (match-define (list (vector #{x1s : (Listof Real)}
                                        #{x2s : (Listof Real)}
                                        #{y1s : (Listof Real)}
                                        #{y2s : (Listof Real)}
                                        #{all-zs : (Listof (U Real ivl #f))})
                                ...)
              (append*
               (for/list : (Listof (Listof (Vector Real Real Real Real (U Real ivl #f))))
                 ([y1  (in-list ys)]
                  [y2  (in-list (rest ys))]
                  [c2  (in-list c2s)])
                 (for/list : (Listof (Vector Real Real Real Real (U Real ivl #f)))
                   ([x1  (in-list xs)]
                    [x2  (in-list (rest xs))]
                    [c1  (in-list c1s)])
                   (vector x1 x2 y1 y2 (hash-ref h (cons c1 c2) (λ () +nan.0)))))))
            (define tick-xs (linear-seq x-min x-max x-num #:start? #f #:end? #f))
            (define tick-ys (linear-seq y-min y-max y-num #:start? #f #:end? #f))
            (define rects
              (map (λ ([x1 : Real] [x2 : Real] [y1 : Real] [y2 : Real] [z : (U Real ivl #f)])
                     (vector (ivl/gap x1 x2 gap)
                             (ivl/gap y1 y2 gap)
                             (if (ivl? z) z (ivl 0 z))))
                   x1s x2s y1s y2s all-zs))
            (renderer3d
             (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
             (discrete-histogram3d-ticks-fun c1s c2s tick-xs tick-ys
                                             add-x-ticks? add-y-ticks? x-far-ticks? y-far-ticks?)
             (rectangles3d-render-proc rects color style line-color line-width line-style
                                       alpha label)))]))]))

(:: stacked-histogram3d
    (->* [(Sequenceof (U (Vector Any Any (Sequenceof Real))
                         (List Any Any (Sequenceof Real))))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:gap Nonnegative-Real
          #:colors (Plot-Colors Natural)
          #:styles (Plot-Brush-Styles Natural)
          #:line-colors (Plot-Colors Natural)
          #:line-widths (Pen-Widths Natural)
          #:line-styles (Plot-Pen-Styles Natural)
          #:alphas (Alphas Natural)
          #:labels (Labels Natural)
          #:add-x-ticks? Boolean
          #:add-y-ticks? Boolean
          #:x-far-ticks? Boolean
          #:y-far-ticks? Boolean]
         (Listof renderer3d)))
(define (stacked-histogram3d
         cat-vals
         #:x-min [x-min 0] #:x-max [x-max #f]
         #:y-min [y-min 0] #:y-max [y-max #f]
         #:z-min [z-min 0] #:z-max [z-max #f]
         #:gap [gap (discrete-histogram-gap)]
         #:colors [colors (stacked-histogram-colors)]
         #:styles [styles (stacked-histogram-styles)]
         #:line-colors [line-colors (stacked-histogram-line-colors)]
         #:line-widths [line-widths (stacked-histogram-line-widths)]
         #:line-styles [line-styles (stacked-histogram-line-styles)]
         #:alphas [alphas (stacked-histogram-alphas)]
         #:labels [labels '(#f)]
         #:add-x-ticks? [add-x-ticks? #t]
         #:add-y-ticks? [add-y-ticks? #t]
         #:x-far-ticks? [x-far-ticks? #f]
         #:y-far-ticks? [y-far-ticks? #f])
  (let ([cat-vals  (sequence->list cat-vals)])
    (match-define (list (or (vector #{cat1s : (Listof Any)}
                                    #{cat2s : (Listof Any)}
                                    #{zs : (Listof (U #f (Sequenceof Real)))})
                            (list #{cat1s : (Listof Any)}
                                  #{cat2s : (Listof Any)}
                                  #{zs : (Listof (U #f (Sequenceof Real)))}))
                        ...)
      cat-vals)
    (let ([zs  (map (λ ([z : (U #f (Sequenceof Real))]) (if z (sequence->list z) empty)) zs)])
      (define zss (map cumulative-sum zs))
      (define z-ivlss (for/list : (Listof (Listof ivl)) ([zs  (in-list zss)])
                        (for/list  : (Listof ivl) ([z1  (in-list zs)]
                                                   [z2  (in-list (rest zs))])
                          (ivl z1 z2))))
      (define max-num (apply max (map (λ ([zs : (Listof Real)]) (length zs)) zss)))
      (for/list ([z-ivls  (in-list (transpose z-ivlss))]
                 [color   (in-cycle* (generate-list colors max-num))]
                 [style   (in-cycle* (generate-list styles max-num))]
                 [line-color  (in-cycle* (generate-list line-colors max-num))]
                 [line-width : Nonnegative-Real  (in-cycle* (generate-list line-widths max-num))]
                 [line-style  (in-cycle* (generate-list line-styles max-num))]
                 [alpha : Nonnegative-Real  (in-cycle* (generate-list alphas max-num))]
                 [label   (in-cycle* (generate-list labels max-num))])
        (discrete-histogram3d
         (map (λ ([cat1 : Any] [cat2 : Any] [z-ivl : (U ivl #f)])
                (list cat1 cat2 z-ivl))
              cat1s cat2s z-ivls)
         #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max #:gap gap
         #:color color #:style style #:line-color line-color #:line-width line-width
         #:line-style line-style #:alpha alpha #:label label
         #:add-x-ticks? add-x-ticks? #:add-y-ticks? add-y-ticks?
         #:x-far-ticks? x-far-ticks? #:y-far-ticks? y-far-ticks?)))))
