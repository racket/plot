#lang typed/racket/base

;; Renderers for intervals between functions.

(require typed/racket/class racket/match racket/math racket/list racket/sequence
         (only-in math/statistics stddev quantile)
         (only-in typed/pict pict)
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt"
         "rectangle.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Lines, parametric, polar

(: lines-interval-render-proc (-> (Listof (Vectorof Real)) (Listof (Vectorof Real))
                                  Plot-Color Plot-Brush-Style
                                  Plot-Color Nonnegative-Real Plot-Pen-Style
                                  Plot-Color Nonnegative-Real Plot-Pen-Style
                                  Nonnegative-Real
                                  2D-Render-Proc))
(define ((lines-interval-render-proc v1s v2s color style
                                     line1-color line1-width line1-style
                                     line2-color line2-width line2-style
                                     alpha)
         area)
  (send area put-alpha alpha)
  (send area put-pen 0 0 'transparent)
  (send area put-brush color style)
  (send area put-polygon (append v1s (reverse v2s)))
  
  (send area put-pen line1-color line1-width line1-style)
  (send area put-lines v1s)
  
  (send area put-pen line2-color line2-width line2-style)
  (send area put-lines v2s))

(:: lines-interval
    (->* [(Sequenceof (Sequenceof Real))
          (Sequenceof (Sequenceof Real))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line1-color Plot-Color
          #:line1-width Nonnegative-Real
          #:line1-style Plot-Pen-Style
          #:line2-color Plot-Color
          #:line2-width Nonnegative-Real
          #:line2-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (lines-interval v1s v2s
                        #:x-min [x-min #f] #:x-max [x-max #f]
                        #:y-min [y-min #f] #:y-max [y-max #f]
                        #:color [color (interval-color)]
                        #:style [style (interval-style)]
                        #:line1-color [line1-color (interval-line1-color)]
                        #:line1-width [line1-width (interval-line1-width)]
                        #:line1-style [line1-style (interval-line1-style)]
                        #:line2-color [line2-color (interval-line2-color)]
                        #:line2-width [line2-width (interval-line2-width)]
                        #:line2-style [line2-style (interval-line2-style)]
                        #:alpha [alpha (interval-alpha)]
                        #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'lines-interval))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(not (rational? line1-width))  (fail/kw "rational?" '#:line1-width line1-width)]
    [(not (rational? line2-width))  (fail/kw "rational?" '#:line2-width line2-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([v1s  (sequence->listof-vector 'lines-interval v1s 2)]
           [v2s  (sequence->listof-vector 'lines-interval v2s 2)])
       (define rvs (filter vrational? (append v1s v2s)))
       (cond
         [(empty? rvs)  empty-renderer2d]
         [else
          (match-define (list (vector #{rxs : (Listof Real)} #{rys : (Listof Real)}) ...) rvs)
          (let ([x-min  (if x-min x-min (apply min* rxs))]
                [x-max  (if x-max x-max (apply max* rxs))]
                [y-min  (if y-min y-min (apply min* rys))]
                [y-max  (if y-max y-max (apply max* rys))])
            (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                        (and label (λ (_)
                                     (interval-legend-entry label color style 0 0 'transparent
                                                            line1-color line1-width line1-style
                                                            line2-color line2-width line2-style)))
                        (lines-interval-render-proc v1s v2s color style
                                                    line1-color line1-width line1-style
                                                    line2-color line2-width line2-style
                                                    alpha)))]))]))

(:: parametric-interval
    (->* [(-> Real (Sequenceof Real)) (-> Real (Sequenceof Real)) Real Real]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line1-color Plot-Color
          #:line1-width Nonnegative-Real
          #:line1-style Plot-Pen-Style
          #:line2-color Plot-Color
          #:line2-width Nonnegative-Real
          #:line2-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (parametric-interval
         f1 f2 t-min t-max
         #:x-min [x-min #f] #:x-max [x-max #f]
         #:y-min [y-min #f] #:y-max [y-max #f]
         #:samples [samples (line-samples)]
         #:color [color (interval-color)]
         #:style [style (interval-style)]
         #:line1-color [line1-color (interval-line1-color)]
         #:line1-width [line1-width (interval-line1-width)]
         #:line1-style [line1-style (interval-line1-style)]
         #:line2-color [line2-color (interval-line2-color)]
         #:line2-width [line2-width (interval-line2-width)]
         #:line2-style [line2-style (interval-line2-style)]
         #:alpha [alpha (interval-alpha)]
         #:label [label #f])
  (define fail/pos (make-raise-argument-error 'parametric-interval f1 f2 t-min t-max))
  (define fail/kw (make-raise-keyword-error 'parametric-interval))
  (cond
    [(not (rational? t-min))  (fail/pos "rational?" 2)]
    [(not (rational? t-max))  (fail/pos "rational?" 3)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line1-width))  (fail/kw "rational?" '#:line1-width line1-width)]
    [(not (rational? line2-width))  (fail/kw "rational?" '#:line2-width line2-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([f1  (λ ([t : Real]) (sequence-head-vector 'parametric-interval (f1 t) 2))]
           [f2  (λ ([t : Real]) (sequence-head-vector 'parametric-interval (f2 t) 2))])
       (lines-interval
        (map f1 (linear-seq t-min t-max samples))
        (map f2 (linear-seq t-min t-max samples))
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
        #:color color #:style style
        #:line1-color line1-color #:line1-width line1-width #:line1-style line1-style
        #:line2-color line2-color #:line2-width line2-width #:line2-style line2-style
        #:alpha alpha #:label label))]))

(:: polar-interval
    (->* [(-> Real Real) (-> Real Real)]
         [Real Real
          #:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line1-color Plot-Color
          #:line1-width Nonnegative-Real
          #:line1-style Plot-Pen-Style
          #:line2-color Plot-Color
          #:line2-width Nonnegative-Real
          #:line2-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (polar-interval
         f1 f2 [θ-min 0] [θ-max (* 2 pi)]
         #:x-min [x-min #f] #:x-max [x-max #f]
         #:y-min [y-min #f] #:y-max [y-max #f]
         #:samples [samples (line-samples)]
         #:color [color (interval-color)]
         #:style [style (interval-style)]
         #:line1-color [line1-color (interval-line1-color)]
         #:line1-width [line1-width (interval-line1-width)]
         #:line1-style [line1-style (interval-line1-style)]
         #:line2-color [line2-color (interval-line2-color)]
         #:line2-width [line2-width (interval-line2-width)]
         #:line2-style [line2-style (interval-line2-style)]
         #:alpha [alpha (interval-alpha)]
         #:label [label #f])
  (define fail/pos (make-raise-argument-error 'polar-interval f1 f2 θ-min θ-max))
  (define fail/kw (make-raise-keyword-error 'polar-interval))
  (cond
    [(not (rational? θ-min))  (fail/pos "rational?" 2)]
    [(not (rational? θ-max))  (fail/pos "rational?" 3)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line1-width))  (fail/kw "rational?" '#:line1-width line1-width)]
    [(not (rational? line2-width))  (fail/kw "rational?" '#:line2-width line2-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define θs (linear-seq θ-min θ-max samples))
     (lines-interval
      (map polar->cartesian θs (map f1 θs))
      (map polar->cartesian θs (map f2 θs))
      #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
      #:color color #:style style
      #:line1-color line1-color #:line1-width line1-width #:line1-style line1-style
      #:line2-color line2-color #:line2-width line2-width #:line2-style line2-style
      #:alpha alpha #:label label)]))

;; ===================================================================================================
;; Function

(: function-interval-render-proc (-> Sampler Sampler Positive-Integer
                                     Plot-Color Plot-Brush-Style
                                     Plot-Color Nonnegative-Real Plot-Pen-Style
                                     Plot-Color Nonnegative-Real Plot-Pen-Style
                                     Nonnegative-Real
                                     2D-Render-Proc))
(define ((function-interval-render-proc f1 f2 samples color style
                                        line1-color line1-width line1-style
                                        line2-color line2-width line2-style
                                        alpha)
         area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample x1s y1s _ _) (f1 x-ivl samples))
  (match-define (sample x2s y2s _ _) (f2 x-ivl samples))
  (define v1s (map (λ ([x : Real] [y : Real]) (vector x y)) x1s y1s))
  (define v2s (map (λ ([x : Real] [y : Real]) (vector x y)) x2s y2s))
  
  ((lines-interval-render-proc v1s v2s color style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style
                               alpha)
   area))

(:: function-interval
    (->* [(-> Real Real) (-> Real Real)]
         [(U Real #f) (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line1-color Plot-Color
          #:line1-width Nonnegative-Real
          #:line1-style Plot-Pen-Style
          #:line2-color Plot-Color
          #:line2-width Nonnegative-Real
          #:line2-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (function-interval
         f1 f2 [x-min #f] [x-max #f]
         #:y-min [y-min #f] #:y-max [y-max #f]
         #:samples [samples (line-samples)]
         #:color [color (interval-color)]
         #:style [style (interval-style)]
         #:line1-color [line1-color (interval-line1-color)]
         #:line1-width [line1-width (interval-line1-width)]
         #:line1-style [line1-style (interval-line1-style)]
         #:line2-color [line2-color (interval-line2-color)]
         #:line2-width [line2-width (interval-line2-width)]
         #:line2-style [line2-style (interval-line2-style)]
         #:alpha [alpha (interval-alpha)]
         #:label [label #f])
  (define fail/pos (make-raise-argument-error 'function-interval f1 f2 x-min x-max))
  (define fail/kw (make-raise-keyword-error 'function-interval))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 2)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 3)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line1-width))  (fail/kw "rational?" '#:line1-width line1-width)]
    [(not (rational? line2-width))  (fail/kw "rational?" '#:line2-width line2-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define g1 (function->sampler f1 x-ivl))
     (define g2 (function->sampler f2 x-ivl))
     (renderer2d (vector x-ivl y-ivl)
                 (function-interval-bounds-fun g1 g2 samples)
                 default-ticks-fun
                 (and label (λ (_)
                              (interval-legend-entry label color style 0 0 'transparent
                                                     line1-color line1-width line1-style
                                                     line2-color line2-width line2-style)))
                 (function-interval-render-proc g1 g2 samples color style
                                                line1-color line1-width line1-style
                                                line2-color line2-width line2-style
                                                alpha))]))

;; ===================================================================================================
;; Inverse function

(: inverse-interval-render-proc (-> Sampler Sampler Positive-Integer
                                    Plot-Color Plot-Brush-Style
                                    Plot-Color Nonnegative-Real Plot-Pen-Style
                                    Plot-Color Nonnegative-Real Plot-Pen-Style
                                    Nonnegative-Real
                                    2D-Render-Proc))
(define ((inverse-interval-render-proc f1 f2 samples color style
                                       line1-color line1-width line1-style
                                       line2-color line2-width line2-style
                                       alpha)
         area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample y1s x1s _ _) (f1 y-ivl samples))
  (match-define (sample y2s x2s _ _) (f2 y-ivl samples))
  (define v1s (map (λ ([x : Real] [y : Real]) (vector x y)) x1s y1s))
  (define v2s (map (λ ([x : Real] [y : Real]) (vector x y)) x2s y2s))
  
  ((lines-interval-render-proc v1s v2s color style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style
                               alpha)
   area))

(:: inverse-interval
    (->* [(-> Real Real) (-> Real Real)]
         [(U Real #f) (U Real #f)
          #:x-min (U Real #f) #:x-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line1-color Plot-Color
          #:line1-width Nonnegative-Real
          #:line1-style Plot-Pen-Style
          #:line2-color Plot-Color
          #:line2-width Nonnegative-Real
          #:line2-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (inverse-interval
         f1 f2 [y-min #f] [y-max #f]
         #:x-min [x-min #f] #:x-max [x-max #f]
         #:samples [samples (line-samples)]
         #:color [color (interval-color)]
         #:style [style (interval-style)]
         #:line1-color [line1-color (interval-line1-color)]
         #:line1-width [line1-width (interval-line1-width)]
         #:line1-style [line1-style (interval-line1-style)]
         #:line2-color [line2-color (interval-line2-color)]
         #:line2-width [line2-width (interval-line2-width)]
         #:line2-style [line2-style (interval-line2-style)]
         #:alpha [alpha (interval-alpha)]
         #:label [label #f])
  (define fail/pos (make-raise-argument-error 'inverse-interval f1 f2 y-min y-max))
  (define fail/kw (make-raise-keyword-error 'inverse-interval))
  (cond
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 2)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 3)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line1-width))  (fail/kw "rational?" '#:line1-width line1-width)]
    [(not (rational? line2-width))  (fail/kw "rational?" '#:line2-width line2-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define g1 (inverse->sampler f1 y-ivl))
     (define g2 (inverse->sampler f2 y-ivl))
     (renderer2d (vector x-ivl y-ivl)
                 (inverse-interval-bounds-fun g1 g2 samples)
                 default-ticks-fun
                 (and label (λ (_)
                              (interval-legend-entry label color style 0 0 'transparent
                                                     line1-color line1-width line1-style
                                                     line2-color line2-width line2-style)))
                 (inverse-interval-render-proc g1 g2 samples color style
                                               line1-color line1-width line1-style
                                               line2-color line2-width line2-style
                                               alpha))]))

;; ===================================================================================================
;; Violin

(:: violin
    (->* [(Sequenceof Real)]
         [#:x Real
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:gap Nonnegative-Real
          #:skip Nonnegative-Real
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line1-color Plot-Color
          #:line1-width Nonnegative-Real
          #:line1-style Plot-Pen-Style
          #:line2-color Plot-Color
          #:line2-width Nonnegative-Real
          #:line2-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)
          #:add-ticks? Boolean
          #:far-ticks? Boolean
          #:bandwidth (U Real #f)
          #:invert? Boolean]
         renderer2d))
(define (violin
         ys
         #:x [x 0]
         #:y-min [y-min #f] #:y-max [y-max #f]
         #:gap [gap (discrete-histogram-gap)]
         #:skip [skip (discrete-histogram-skip)]
         #:samples [samples (line-samples)]
         #:color [color (interval-color)]
         #:style [style (interval-style)]
         #:line1-color [line1-color (interval-line1-color)]
         #:line1-width [line1-width (interval-line1-width)]
         #:line1-style [line1-style (interval-line1-style)]
         #:line2-color [line2-color (interval-line2-color)]
         #:line2-width [line2-width (interval-line2-width)]
         #:line2-style [line2-style (interval-line2-style)]
         #:alpha [alpha (interval-alpha)]
         #:label [label #f]
         #:add-ticks? [add-ticks? #t]
         #:far-ticks? [far-ticks? #f]
         #:bandwidth [bandwidth #f]
         #:invert? [invert? #f])
  (define fail/kw (make-raise-keyword-error 'violin))
  (cond
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(or (> gap 1) (not (rational? gap)))  (fail/kw "real in [0,1]" '#:gap gap)]
    [(not (rational? skip))  (fail/kw "rational?" '#:skip skip)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line1-width))  (fail/kw "rational?" '#:line1-width line1-width)]
    [(not (rational? line2-width))  (fail/kw "rational?" '#:line2-width line2-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define ys* (sequence->list ys))
     (define-values (f1 y-low y-high)
       (kde ys* (or bandwidth (silverman-bandwidth ys*))))
     (define y-min* (or y-min y-low))
     (define y-max* (or y-max y-high))
     (define half-width (/ (- skip gap) 2))
     (define x-ivl (ivl (- x half-width) (+ x half-width)))
     (define y-ivl (ivl y-min* y-max*))
     (define (f1* [y : Real]) (+ (f1 y) x))
     (define (f2* [y : Real]) (+ (- (f1 y)) x))

     (define-values (g1 g2 bounds bounds-fun render-proc)
       (if invert?
           (values (function->sampler f1* y-ivl)
                   (function->sampler f2* y-ivl)
                   (vector y-ivl x-ivl)
                   function-interval-bounds-fun
                   function-interval-render-proc)
           (values (inverse->sampler f1* y-ivl)
                   (inverse->sampler f2* y-ivl)
                   (vector x-ivl y-ivl)
                   inverse-interval-bounds-fun
                   inverse-interval-render-proc)))

     (: maybe-invert (All (A) (-> A A (Vectorof A))))
     (define maybe-invert (if invert? (λ (x y) (vector y x)) vector))

     (renderer2d bounds
                 (bounds-fun g1 g2 samples)
                 (if label
                     (discrete-histogram-ticks-fun (list label) (list x)
                                                   add-ticks? far-ticks?
                                                   maybe-invert)
                     default-ticks-fun)
                 (and label (λ (_)
                              (interval-legend-entry label color style 0 0 'transparent
                                                     line1-color line1-width line1-style
                                                     line2-color line2-width line2-style)))
                 (render-proc g1 g2 samples color style
                              line1-color line1-width line1-style
                              line2-color line2-width line2-style
                              alpha))]))
