#lang typed/racket/base

(require racket/match typed/racket/class racket/list racket/sequence
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Rectangles

(: rectangles-render-proc (-> (Listof Rect)
                              Plot-Color Plot-Brush-Style
                              Plot-Color Nonnegative-Real Plot-Pen-Style
                              Nonnegative-Real
                              (U String #f)
                              2D-Render-Proc))
(define ((rectangles-render-proc rects color style line-color line-width line-style alpha label)
         area)
  (send area put-pen line-color line-width line-style)
  (send area put-brush color style)
  (send area put-alpha alpha)
  (for ([rect  (in-list rects)])
    (send area put-rect rect))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else  empty]))

(:: rectangles
    (->* [(Sequenceof (Sequenceof ivl))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer2d))
(define (rectangles rects
                    #:x-min [x-min #f] #:x-max [x-max #f]
                    #:y-min [y-min #f] #:y-max [y-max #f]
                    #:color [color (rectangle-color)]
                    #:style [style (rectangle-style)]
                    #:line-color [line-color (rectangle-line-color)]
                    #:line-width [line-width (rectangle-line-width)]
                    #:line-style [line-style (rectangle-line-style)]
                    #:alpha [alpha (rectangle-alpha)]
                    #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'rectangles))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([rects  (sequence->listof-vector 'rectangles rects 2)])
       (match-define (list (vector (ivl #{x1s : (Listof (U Real #f))}
                                        #{x2s : (Listof (U Real #f))})
                                   (ivl #{y1s : (Listof (U Real #f))}
                                        #{y2s : (Listof (U Real #f))}))
                           ...)
         rects)
       (define rxs (filter rational? (append x1s x2s)))
       (define rys (filter rational? (append y1s y2s)))
       (cond
         [(or (empty? rxs) (empty? rys))  (renderer2d #f #f #f #f)]
         [else
          (let ([x-min  (if x-min x-min (apply min* rxs))]
                [x-max  (if x-max x-max (apply max* rxs))]
                [y-min  (if y-min y-min (apply min* rys))]
                [y-max  (if y-max y-max (apply max* rys))])
            (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                        (rectangles-render-proc rects color style line-color line-width line-style
                                                alpha label)))]))]))

;; ===================================================================================================
;; Real histograms (or histograms on the real line)

(:: area-histogram
    (->* [(-> Real Real) (Sequenceof Real)]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer2d))
(define (area-histogram f bin-bounds
                        #:x-min [x-min #f] #:x-max [x-max #f]
                        #:y-min [y-min 0] #:y-max [y-max #f]
                        #:samples [samples (line-samples)]
                        #:color [color (rectangle-color)]
                        #:style [style (rectangle-style)]
                        #:line-color [line-color (rectangle-line-color)]
                        #:line-width [line-width (rectangle-line-width)]
                        #:line-style [line-style (rectangle-line-style)]
                        #:alpha [alpha (rectangle-alpha)]
                        #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'area-histogram))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let* ([bin-bounds  (sequence->list bin-bounds)]
            [bin-bounds  (filter rational? bin-bounds)]
            [bin-bounds  (sort bin-bounds <)])
       (cond
         [((length bin-bounds) . < . 2)  (renderer2d #f #f #f #f)]
         [else
          (define xs (linear-seq (apply min* bin-bounds) (apply max* bin-bounds) samples
                                 #:start? #f #:end? #f))
          (define xss (bin-samples bin-bounds xs))
          (define heights
            (for/list : (Listof Real) ([xs  (in-list xss)]
                                       [x1  (in-list bin-bounds)]
                                       [x2  (in-list (rest bin-bounds))])
              (define x-size (- x2 x1))
              (define ys (map f xs))
              (/ (apply + ys) (length xs))))
          (rectangles (map (λ ([x-ivl : ivl] [h : Real]) (vector x-ivl (ivl 0 h)))
                           (bounds->intervals bin-bounds)
                           heights)
                      #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                      #:color color #:style style #:line-color line-color #:line-width line-width
                      #:line-style line-style #:alpha alpha #:label label)]))]))

;; ===================================================================================================
;; Discrete histograms

(: discrete-histogram-ticks-fun (-> (Listof Any) (Listof Real) Boolean Boolean
                                    (All (A) (-> A A (Vectorof A)))
                                    Ticks-Fun))
(define ((discrete-histogram-ticks-fun cats tick-xs add-ticks? far-ticks? maybe-invert) r)
  (match-define (vector x-ivl y-ivl) r)
  (match-define (vector _ (ivl y-min y-max)) (maybe-invert x-ivl y-ivl))
  (define-values (x-ticks x-far-ticks)
    (let ([ticks  (cond [add-ticks?  (for/list : (Listof tick) ([cat  (in-list cats)]
                                                                [x  (in-list tick-xs)])
                                       (tick x #t (->plot-label cat)))]
                        [else  empty])])
      (if far-ticks? (values empty ticks) (values ticks empty))))
  (match-let*
      ([(vector plot-x-ticks plot-y-ticks)
        (maybe-invert (plot-x-ticks) (plot-y-ticks))]
       [(vector plot-x-far-ticks plot-y-far-ticks)
        (maybe-invert (plot-x-far-ticks) (plot-y-far-ticks))]
       [(vector x-ticks y-ticks)
        (maybe-invert x-ticks
                      (if (and y-min y-max) (ticks-generate plot-y-ticks y-min y-max) empty))]
       [(vector x-far-ticks y-far-ticks)
        (maybe-invert x-far-ticks
                      (if (and y-min y-max) (ticks-generate plot-y-far-ticks y-min y-max) empty))])
    (list x-ticks x-far-ticks y-ticks y-far-ticks)))

(:: discrete-histogram
    (->* [(Sequenceof (U (Vector Any (U Real ivl #f))
                         (List Any (U Real ivl #f))))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:gap Nonnegative-Real
          #:skip Nonnegative-Real
          #:invert? Boolean
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)
          #:add-ticks? Boolean
          #:far-ticks? Boolean]
         renderer2d))
(define (discrete-histogram
         cat-vals
         #:x-min [x-min 0] #:x-max [x-max #f]
         #:y-min [y-min 0] #:y-max [y-max #f]
         #:gap [gap (discrete-histogram-gap)]
         #:skip [skip (discrete-histogram-skip)]
         #:invert? [invert? (discrete-histogram-invert?)]
         #:color [color (rectangle-color)]
         #:style [style (rectangle-style)]
         #:line-color [line-color (rectangle-line-color)]
         #:line-width [line-width (rectangle-line-width)]
         #:line-style [line-style (rectangle-line-style)]
         #:alpha [alpha (rectangle-alpha)]
         #:label [label #f]
         #:add-ticks? [add-ticks? #t]
         #:far-ticks? [far-ticks? #f])
  (define fail/kw (make-raise-keyword-error 'discrete-histogram))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(or (> gap 1) (not (rational? gap)))  (fail/kw "real in [0,1]" '#:gap gap)]
    [(not (rational? skip))  (fail/kw "rational?" '#:skip skip)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([cat-vals  (sequence->list cat-vals)])
       (match-define (list (or (vector #{cats : (Listof Any)}
                                       #{ys : (Listof (U Real ivl #f))})
                               (list #{cats : (Listof Any)}
                                     #{ys : (Listof (U Real ivl #f))}))
                           ...)
         cat-vals)
       (define rys
         (filter rational?
                 (append*
                  (for/list : (Listof (Listof (U Real #f))) ([y  (in-list ys)])
                    (cond [(ivl? y)  (match-define (ivl y1 y2) y)
                                     (list y1 y2)]
                          [else  (list y)])))))
       (cond
         [(empty? rys)  (renderer2d #f #f #f #f)]
         [else
          (define n (length cats))
          (let* ([x-min  (if x-min x-min 0)]
                 [x-max  (if x-max x-max (max x-min (+ x-min (* (- n 1) skip) 1)))]
                 [y-min  (if y-min y-min (apply min* rys))]
                 [y-max  (if y-max y-max (apply max* rys))])
            (define xs (build-list n (λ ([i : Index]) (+ x-min (* i skip)))))
            (define x-ivls (for/list : (Listof ivl) ([x  (in-list xs)])
                             (ivl (+ x (* 1/2 gap)) (- (+ x 1) (* 1/2 gap)))))
            (define tick-xs (for/list : (Listof Real) ([x  (in-list xs)]) (+ x 1/2)))
            (define y-ivls (map (λ ([y : (U Real ivl #f)]) (if (ivl? y) y (ivl 0 y))) ys))
            (: maybe-invert (All (A) (-> A A (Vectorof A))))
            (define maybe-invert (if invert? (λ (x y) (vector y x)) vector))
            (renderer2d
             (maybe-invert (ivl x-min x-max) (ivl y-min y-max)) #f
             (discrete-histogram-ticks-fun cats tick-xs add-ticks? far-ticks? maybe-invert)
             (rectangles-render-proc (map (λ ([x-ivl : ivl] [y-ivl : ivl])
                                            (maybe-invert x-ivl y-ivl))
                                          x-ivls y-ivls)
                                     color style line-color line-width line-style alpha label)))]))]))

(:: stacked-histogram
    (->* [(Sequenceof (U (Vector Any (Sequenceof Real))
                         (List Any (Sequenceof Real))))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:gap Nonnegative-Real
          #:skip Nonnegative-Real
          #:invert? Boolean
          #:colors (Plot-Colors Natural)
          #:styles (Plot-Brush-Styles Natural)
          #:line-colors (Plot-Colors Natural)
          #:line-widths (Pen-Widths Natural)
          #:line-styles (Plot-Pen-Styles Natural)
          #:alphas (Alphas Natural)
          #:labels (Labels Natural)
          #:add-ticks? Boolean
          #:far-ticks? Boolean]
         (Listof renderer2d)))
(define (stacked-histogram
         cat-vals
         #:x-min [x-min 0] #:x-max [x-max #f]
         #:y-min [y-min 0] #:y-max [y-max #f]
         #:gap [gap (discrete-histogram-gap)]
         #:skip [skip (discrete-histogram-skip)]
         #:invert? [invert? (discrete-histogram-invert?)]
         #:colors [colors (stacked-histogram-colors)]
         #:styles [styles (stacked-histogram-styles)]
         #:line-colors [line-colors (stacked-histogram-line-colors)]
         #:line-widths [line-widths (stacked-histogram-line-widths)]
         #:line-styles [line-styles (stacked-histogram-line-styles)]
         #:alphas [alphas (stacked-histogram-alphas)]
         #:labels [labels '(#f)]
         #:add-ticks? [add-ticks? #t]
         #:far-ticks? [far-ticks? #f])
  (let ([cat-vals  (sequence->list cat-vals)])
    (match-define (list (or (vector #{cats : (Listof Any)}
                                    #{ys : (Listof (U #f (Sequenceof Real)))})
                            (list #{cats : (Listof Any)}
                                  #{ys : (Listof (U #f (Sequenceof Real)))}))
                        ...)
      cat-vals)
    (let ([ys  (map (λ ([y : (U #f (Sequenceof Real))]) (if y (sequence->list y) empty)) ys)])
      (define yss (map cumulative-sum ys))
      (define y-ivlss (for/list : (Listof (Listof ivl)) ([ys  (in-list yss)])
                        (for/list : (Listof ivl) ([y1  (in-list ys)] [y2  (in-list (rest ys))])
                          (ivl y1 y2))))
      (define max-num (apply max (map (λ ([ys : (Listof Real)]) (length ys)) yss)))
      (for/list ([y-ivls  (in-list (transpose y-ivlss))]
                 [color   (in-cycle* (generate-list colors max-num))]
                 [style   (in-cycle* (generate-list styles max-num))]
                 [line-color  (in-cycle* (generate-list line-colors max-num))]
                 [line-width : Nonnegative-Real  (in-cycle* (generate-list line-widths max-num))]
                 [line-style  (in-cycle* (generate-list line-styles max-num))]
                 [alpha : Nonnegative-Real  (in-cycle* (generate-list alphas max-num))]
                 [label   (in-cycle* (generate-list labels max-num))])
        (discrete-histogram
         (map (λ ([cat : Any] [y-ivl : (U ivl #f)]) (list cat y-ivl)) cats y-ivls)
         #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
         #:gap gap #:skip skip #:invert? invert?
         #:color color #:style style #:line-color line-color #:line-width line-width
         #:line-style line-style #:alpha alpha #:label label
         #:add-ticks? add-ticks? #:far-ticks? far-ticks?)))))
