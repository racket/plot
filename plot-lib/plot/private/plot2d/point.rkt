#lang typed/racket/base

;; Renderers for points and other point-like things.

(require typed/racket/class racket/match racket/math racket/list
         (only-in typed/pict pict)
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(require/typed
 "../common/untyped-utils.rkt"
 [fix-vector-field-fun  (-> Symbol
                            (U (-> Real Real (Sequenceof Real))
                               (-> (Vector Real Real) (Sequenceof Real)))
                            (-> Real Real (Vectorof Real)))])

(provide (all-defined-out))

;; ===================================================================================================
;; Points (scatter plots)

(: points-render-fun (-> (Listof (Vectorof Real)) Point-Sym
                         Plot-Color Plot-Color Nonnegative-Real Nonnegative-Real
                         Nonnegative-Real
                         2D-Render-Proc))
(define ((points-render-fun vs sym color fill-color size line-width alpha) area)
  (send area put-alpha alpha)
  (send area put-pen color line-width 'solid)
  (send area put-brush fill-color 'solid)
  (send area put-glyphs vs sym size))

(:: points
    (->* [(Sequenceof (Sequenceof Real))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:sym Point-Sym
          #:color Plot-Color
          #:fill-color (U Plot-Color 'auto)
          #:x-jitter Nonnegative-Real
          #:y-jitter Nonnegative-Real
          #:size Nonnegative-Real
          #:line-width Nonnegative-Real
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (points vs
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f]
                #:sym [sym (point-sym)]
                #:color [color (point-color)]
                #:fill-color [fill-color 'auto]
                #:x-jitter [x-jitter (point-x-jitter)]
                #:y-jitter [y-jitter (point-y-jitter)]
                #:size [size (point-size)]
                #:line-width [line-width (point-line-width)]
                #:alpha [alpha (point-alpha)]
                #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'points))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(not (rational? size))  (fail/kw "rational?" '#:size size)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let* ([vs  (sequence->listof-vector 'points vs 2)]
            [vs  (filter vrational? vs)])
       (cond
         [(empty? vs)  empty-renderer2d]
         [else
          (unless (= 0 x-jitter y-jitter)
            (points-apply-jitters vs (vector x-jitter y-jitter) #:ivls (vector (ivl x-min x-max) (ivl y-min y-max))))
          (match-define (list (vector #{xs : (Listof Real)} #{ys : (Listof Real)}) ...) vs)
          (let ([x-min  (if x-min x-min (apply min* xs))]
                [x-max  (if x-max x-max (apply max* xs))]
                [y-min  (if y-min y-min (apply min* ys))]
                [y-max  (if y-max y-max (apply max* ys))]
                [fill-color  (if (eq? fill-color 'auto) (->pen-color color) fill-color)])
            (renderer2d
             (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
             (and label (λ (_) (point-legend-entry label sym color fill-color size line-width)))
             (points-render-fun vs sym color fill-color
                                size line-width alpha)))]))]))

;; ===================================================================================================
;; Vector fields

(: vector-field-render-fun
   (-> (-> Real Real (Vectorof Real))
       Positive-Integer (U Real 'auto 'normalized)
       Plot-Color Nonnegative-Real Plot-Pen-Style
       Nonnegative-Real
       2D-Render-Proc))
(define ((vector-field-render-fun f samples scale color line-width line-style alpha) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  
  (when (and x-min x-max y-min y-max)
    (define xs0 (linear-seq x-min x-max samples #:start? #t #:end? #t))
    (define ys0 (linear-seq y-min y-max samples #:start? #t #:end? #t))
     
    (define-values (xs ys dxs dys angles mags)
      (for*/lists ([xs : (Listof Real)]
                   [ys : (Listof Real)]
                   [dxs : (Listof Real)]
                   [dys : (Listof Real)]
                   [angles : (Listof Real)]
                   [mags : (Listof Nonnegative-Real)]
                   ) ([x   (in-list xs0)]
                      [y   (in-list ys0)]
                      [dv  (in-value (f x y))] #:when (vrational? dv))
        (match-define (vector dx dy) dv)
        (values x y dx dy (atan2 dy dx) (sqrt (+ (sqr dx) (sqr dy))))))
     
    (unless (empty? xs)
      (define box-x-size (/ (- x-max x-min) samples))
      (define box-y-size (/ (- y-max y-min) samples))
                 
      (define new-mags
        (match scale
          [(? real?)  (map (λ ([mag : Real]) (* scale mag)) mags)]
          ['normalized  (define box-size (min box-x-size box-y-size))
                        (build-list (length dxs) (λ _ box-size))]
          ['auto
           ;; When all dxs or dys are (exact) zero, the calculation of scale
           ;; will raise a 'division by zero' error. If we convert the values
           ;; to flonums, the partial result will be +inf.0, and the correct
           ;; scale can be calculated.
           (define dx-max (real->double-flonum (apply max (map abs dxs))))
           (define dy-max (real->double-flonum (apply max (map abs dys))))
           (define scale (min (/ box-x-size dx-max)
                              (/ box-y-size dy-max)))
           (map (λ ([mag : Real]) (* scale mag)) mags)]))
                 
      (send area put-alpha alpha)
      (send area put-pen color line-width line-style)
      (for ([x      (in-list xs)]
            [y      (in-list ys)]
            [angle  (in-list angles)]
            [mag    (in-list new-mags)])
        (send area put-arrow
              (vector x y)
              (vector (+ x (* mag (cos angle))) (+ y (* mag (sin angle)))))))))

(:: vector-field
    (->* [(U (-> Real Real (Sequenceof Real))
             (-> (Vector Real Real) (Sequenceof Real)))]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:samples Positive-Integer
          #:scale (U Real 'auto 'normalized)
          #:color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (vector-field f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                      #:samples [samples (vector-field-samples)]
                      #:scale [scale (vector-field-scale)]
                      #:color [color (vector-field-color)]
                      #:line-width [line-width (vector-field-line-width)]
                      #:line-style [line-style (vector-field-line-style)]
                      #:alpha [alpha (vector-field-alpha)]
                      #:label [label #f])
  (define fail/pos (make-raise-argument-error 'vector-field3d f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'vector-field3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 2)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 3)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 4)]
    [(and (real? scale) (not (rational? scale)))
     (fail/kw "'auto, 'normalized or rational" '#:scale scale)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([f  (fix-vector-field-fun 'vector-field f)])
       (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                   (and label (λ (_) (arrow-legend-entry label color line-width line-style)))
                   (vector-field-render-fun
                    f samples scale color line-width line-style alpha)))]))

;; ===================================================================================================
;; Error bars

(: error-bars-render-fun (-> (Listof Real) (Listof Real) (Listof Real)
                             Plot-Color Nonnegative-Real Plot-Pen-Style
                             Nonnegative-Real Nonnegative-Real Boolean
                             2D-Render-Proc))
(define ((error-bars-render-fun xs ys hs color line-width line-style width alpha invert?) area)
  (define clip-rect (send area get-clip-rect))
  (define radius (* 1/2 width))
  (define angle (if invert? (/ pi 2) 0))

  (: maybe-invert (All (A) (-> A A (Vectorof A))))
  (define maybe-invert (if invert? (λ (x y) (vector y x)) vector))
  
  (send area put-alpha alpha)
  (send area put-pen color line-width line-style)
  (for ([x  (in-list xs)] [y  (in-list ys)] [h  (in-list hs)])
    (when (rect-contains? clip-rect (maybe-invert x y))
      (define v1 (maybe-invert x (- y h)))
      (define v2 (maybe-invert x (+ y h)))
      (send area put-line v1 v2)
      (send area put-tick v1 radius angle)
      (send area put-tick v2 radius angle))))

(:: error-bars
    (->* [(Sequenceof (Sequenceof Real))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:width Nonnegative-Real
          #:alpha Nonnegative-Real
          #:invert? Boolean]
         renderer2d))
(define (error-bars bars
                    #:x-min [x-min #f] #:x-max [x-max #f]
                    #:y-min [y-min #f] #:y-max [y-max #f]
                    #:color [color (error-bar-color)]
                    #:line-width [line-width (error-bar-line-width)]
                    #:line-style [line-style (error-bar-line-style)]
                    #:width [width (error-bar-width)]
                    #:alpha [alpha (error-bar-alpha)]
                    #:invert? [invert? #f])
  (define fail/kw (make-raise-keyword-error 'error-bars))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let* ([bars  (sequence->listof-vector 'error-bars bars 3)]
            [bars  (filter vrational? bars)])
       (cond [(empty? bars)  empty-renderer2d]
             [else
              (match-define (list (vector #{xs : (Listof Real)}
                                          #{ys : (Listof Real)}
                                          #{hs : (Listof Real)})
                                  ...)
                bars)
              (let ([x-min  (if x-min x-min (apply min* xs))]
                    [x-max  (if x-max x-max (apply max* xs))]
                    [y-min  (if y-min y-min (apply min* (map - ys hs)))]
                    [y-max  (if y-max y-max (apply max* (map + ys hs)))])
                (: maybe-invert (All (A) (-> A A (Vectorof A))))
                (define maybe-invert (if invert? (λ (x y) (vector y x)) vector))
                (renderer2d
                 (maybe-invert (ivl x-min x-max) (ivl y-min y-max))
                 #f default-ticks-fun #f
                 (error-bars-render-fun xs ys hs
                                        color line-width line-style width alpha invert?)))]))]))

;; ===================================================================================================
;; Candlesticks

(: candlesticks-render-fun (-> (Listof Real) (Listof Real) (Listof Real) (Listof Real) (Listof Real)
                               Plot-Color Plot-Color Nonnegative-Real Plot-Pen-Style
                               Nonnegative-Real Nonnegative-Real
                               2D-Render-Proc))
(define ((candlesticks-render-fun xs opens highs lows closes up-color down-color line-width line-style width alpha) area)
  (define clip-rect (send area get-clip-rect))
  (define radius (* 1/2 width))
  
  (send area put-alpha alpha)
  (send area put-pen up-color line-width line-style)
  (for ([x  (in-list xs)] [open  (in-list opens)] [high  (in-list highs)] [low  (in-list lows)] [close  (in-list closes)])
      (define v1 (vector x open))
      (define v2 (vector x high))
      (define v3 (vector x low))
      (define v4 (vector x close))
      (define r1 (vector (ivl (- x radius) (+ x radius)) (ivl open close)))
      (cond [(> open close) (send area put-pen down-color line-width line-style)
                            (send area put-line v2 v1)
                            (send area put-line v4 v3)
                            (send area put-brush down-color 'solid)
                            (send area put-rect r1)]
            [else (send area put-pen up-color line-width line-style)
                  (send area put-line v2 v4)
                  (send area put-line v1 v3)
                  (send area put-brush up-color 'solid)
                  (send area put-rect r1)])))

(:: candlesticks
    (->* [(Sequenceof (Sequenceof Real))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:up-color Plot-Color
          #:down-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:width Nonnegative-Real
          #:alpha Nonnegative-Real]
         renderer2d))
(define (candlesticks candles
                      #:x-min [x-min #f] #:x-max [x-max #f]
                      #:y-min [y-min #f] #:y-max [y-max #f]
                      #:up-color [up-color (candlestick-up-color)]
                      #:down-color [down-color (candlestick-down-color)]
                      #:line-width [line-width (candlestick-line-width)]
                      #:line-style [line-style (candlestick-line-style)]
                      #:width [width (candlestick-width)]
                      #:alpha [alpha (candlestick-alpha)])
  (define fail/kw (make-raise-keyword-error 'candlesticks))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let* ([candles  (sequence->listof-vector 'candlesticks candles 5)]
            [candles  (filter vrational? candles)])
       (cond [(empty? candles)  empty-renderer2d]
             [else
              (match-define (list (vector #{xs : (Listof Real)}
                                          #{opens : (Listof Real)}
                                          #{highs : (Listof Real)}
                                          #{lows : (Listof Real)}
                                          #{closes : (Listof Real)})
                                  ...)
                candles)
              (let ([x-min  (if x-min x-min (- (apply min* xs) width))]
                    [x-max  (if x-max x-max (+ (apply max* xs) width))]
                    [y-min  (if y-min y-min (apply min* lows))]
                    [y-max  (if y-max y-max (apply max* highs))])
                (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun #f
                            (candlesticks-render-fun xs opens highs lows closes
                                                     up-color down-color line-width line-style width alpha)))]))]))
