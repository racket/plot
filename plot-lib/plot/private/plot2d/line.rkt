#lang typed/racket/base

;; Line renderers.

(require typed/racket/class racket/match racket/math racket/list racket/sequence
         plot/utils
         (only-in typed/pict pict)
         (only-in math/statistics stddev)
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Lines, parametric, polar

(: lines-render-proc (-> (Listof (Vectorof Real))
                         Plot-Color
                         Nonnegative-Real
                         Plot-Pen-Style
                         Nonnegative-Real
                         Point-Sym
                         Plot-Color
                         Plot-Color
                         Nonnegative-Real
                         Nonnegative-Real
                         Nonnegative-Real
                         Boolean
                         2D-Render-Proc))
(define ((lines-render-proc
          vs
          color width style alpha
          marker marker-color marker-fill-color marker-size marker-line-width marker-alpha
          ignore-axis-transforms?)
         area)
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines vs ignore-axis-transforms?)

  (unless (eq? marker 'none)
    (send area put-alpha marker-alpha)
    (send area put-pen marker-color marker-line-width 'solid)
    (send area put-brush marker-fill-color 'solid)
    (send area put-glyphs vs marker marker-size)))

(:: lines
    (->* [(Sequenceof (Sequenceof Real))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)
          #:marker Point-Sym
          #:marker-color (U Plot-Color 'auto)
          #:marker-fill-color (U Plot-Color 'auto)
          #:marker-size Nonnegative-Real
          #:marker-line-width Nonnegative-Real
          #:marker-alpha Nonnegative-Real
          #:ignore-axis-transforms? Boolean]
         renderer2d))
(define (lines vs
               #:x-min [x-min #f] #:x-max [x-max #f]
               #:y-min [y-min #f] #:y-max [y-max #f]
               #:color [color (line-color)]
               #:width [width (line-width)]
               #:style [style (line-style)]
               #:alpha [alpha (line-alpha)]
               #:label [label #f]
               #:marker [marker 'none]
               #:marker-color [marker-color 'auto]
               #:marker-fill-color [marker-fill-color 'auto]
               #:marker-size [marker-size (point-size)]
               #:marker-line-width [marker-line-width (point-line-width)]
               #:marker-alpha [marker-alpha (point-alpha)]
               #:ignore-axis-transforms? [ignore-axis-transforms? #f])
  (define fail/kw (make-raise-keyword-error 'lines))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [(not (rational? marker-size))  (fail/kw "rational?" '#:marker-size marker-size)]
    [(not (rational? marker-line-width))  (fail/kw "rational?" '#:marker-line-width marker-line-width)]
    [(or (> marker-alpha 1) (not (rational? marker-alpha)))  (fail/kw "real in [0,1]" '#:marker-alpha marker-alpha)]
    [else
     (let ([vs  (sequence->listof-vector 'lines vs 2)])
       (define rvs (filter vrational? vs))
       (cond [(empty? rvs)  empty-renderer2d]
             [else
              (match-define (list (vector #{rxs : (Listof Real)} #{rys : (Listof Real)}) ...) rvs)
              (let* ([x-min  (if x-min x-min (apply min* rxs))]
                     [x-max  (if x-max x-max (apply max* rxs))]
                     [y-min  (if y-min y-min (apply min* rys))]
                     [y-max  (if y-max y-max (apply max* rys))]
                     [marker-color (if (eq? marker-color 'auto)
                                       (->pen-color color)
                                       marker-color)]
                     [marker-fill-color (if (eq? marker-fill-color 'auto)
                                            (->pen-color marker-color)
                                            marker-fill-color)])
                (define render-fun
                  (lines-render-proc
                   vs
                   color width style alpha
                   marker marker-color marker-fill-color marker-size marker-line-width marker-alpha
                   ignore-axis-transforms?))

                (: legend-entries (U #f (-> Rect (Treeof legend-entry))))
                (define legend-entries
                  (and label
                       (let ([lle (line-legend-entry label color width style)])
                         (if (eq? marker 'none)
                             (lambda (_) lle)
                             (let ([mle (point-legend-entry
                                         label marker marker-color marker-fill-color marker-size
                                         marker-line-width)])
                               (lambda (_)
                                 (legend-entry
                                  label
                                  (lambda ([pd : (Instance Plot-Device%)]
                                           [w : Real]
                                           [h : Real])
                                    ((legend-entry-draw lle) pd w h)
                                    ((legend-entry-draw mle) pd w h)))))))))
                
                (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max))
                            #f
                            default-ticks-fun
                            legend-entries
                            render-fun))]))]))

(:: parametric
    (->* [(-> Real (Sequenceof Real)) Real Real]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (parametric f t-min t-max
                    #:x-min [x-min #f] #:x-max [x-max #f]
                    #:y-min [y-min #f] #:y-max [y-max #f]
                    #:samples [samples (line-samples)]
                    #:color [color (line-color)]
                    #:width [width (line-width)]
                    #:style [style (line-style)]
                    #:alpha [alpha (line-alpha)]
                    #:label [label #f])
  (define fail/pos (make-raise-argument-error 'parametric f t-min t-max))
  (define fail/kw (make-raise-keyword-error 'parametric))
  (cond
    [(not (rational? t-min))  (fail/pos "rational?" 1)]
    [(not (rational? t-max))  (fail/pos "rational?" 2)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([f  (λ ([t : Real]) (sequence-head-vector 'parametric (f t) 2))])
       (lines (map f (linear-seq t-min t-max samples))
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:color color #:width width #:style style #:alpha alpha
              #:label label))]))

(:: polar
    (->* [(-> Real Real)]
         [Real Real
          #:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (polar f [θ-min 0] [θ-max (* 2 pi)]
               #:x-min [x-min #f] #:x-max [x-max #f]
               #:y-min [y-min #f] #:y-max [y-max #f]
               #:samples [samples (line-samples)]
               #:color [color (line-color)]
               #:width [width (line-width)]
               #:style [style (line-style)]
               #:alpha [alpha (line-alpha)]
               #:label [label #f])
  (define fail/pos (make-raise-argument-error 'polar f θ-min θ-max))
  (define fail/kw (make-raise-keyword-error 'polar))
  (cond
    [(not (rational? θ-min))  (fail/pos "rational?" 1)]
    [(not (rational? θ-max))  (fail/pos "rational?" 2)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (lines (let ([θs  (linear-seq θ-min θ-max samples)])
              (map polar->cartesian θs (map f θs)))
            #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
            #:color color #:width width #:style style #:alpha alpha
            #:label label)]))

;; ===================================================================================================
;; Rules are straight lines drawn with a square pen

(: rule-render-proc (-> Real (U Real #f) (U Real #f)
                        (U 'h 'v)
                        Plot-Color
                        Nonnegative-Real
                        Plot-Pen-Style
                        Nonnegative-Real
                        2D-Render-Proc))
(define ((rule-render-proc v v-min v-max h/v color width style alpha) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  ;; This is the error that `get-bounds-rect` should raise
  (unless (and x-min x-max y-min y-max)
    (error 'plot "could not determine sensible plot bounds; got x ∈ ~a, y ∈ ~a"
           (ivl->plot-label (ivl x-min x-max)) (ivl->plot-label (ivl y-min y-max))))

  (send area put-alpha alpha)
  (send area put-pen color width style)
  (case h/v
    [(h) (send area put-line (vector (or v-min x-min) v) (vector (or v-max x-max) v))]
    [(v) (send area put-line (vector v (or v-min y-min)) (vector v (or v-max y-max)))]))

(:: vrule
    (->* [Real]
         [(U Real #f) (U Real #f)
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
          renderer2d))
(define (vrule x [y-min #f] [y-max #f]
               #:color [color (line-color)]
               #:width [width (line-width)]
               #:style [style (line-style)]
               #:alpha [alpha (line-alpha)]
               #:label [label #f])
  (define fail/pos (make-raise-argument-error 'vrule x y-min y-max))
  (define fail/kw (make-raise-keyword-error 'vrule))
  (cond
    [(and y-min (not (rational? y-min)))  (fail/pos "rational?" 1)]
    [(and y-max (not (rational? y-max)))  (fail/pos "rational?" 2)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
      (renderer2d #f #f default-ticks-fun
                  (and label (λ (_) (line-legend-entry label color width style)))
                  (rule-render-proc x y-min y-max 'v color width style alpha))]))

(:: hrule
    (->* [Real]
         [(U Real #f) (U Real #f)
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
          renderer2d))
(define (hrule y [x-min #f] [x-max #f]
               #:color [color (line-color)]
               #:width [width (line-width)]
               #:style [style (line-style)]
               #:alpha [alpha (line-alpha)]
               #:label [label #f])
  (define fail/pos (make-raise-argument-error 'hrule y x-min x-max))
  (define fail/kw (make-raise-keyword-error 'hrule))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "rational?" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "rational?" 2)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
      (renderer2d #f #f default-ticks-fun
                  (and label (λ (_) (line-legend-entry label color width style)))
                  (rule-render-proc y x-min x-max 'h color width style alpha))]))

;; ===================================================================================================
;; Function

(: function-render-proc (-> Sampler
                            Positive-Integer
                            Plot-Color
                            Nonnegative-Real
                            Plot-Pen-Style
                            Nonnegative-Real
                            Point-Sym
                            Plot-Color
                            Plot-Color
                            Nonnegative-Real
                            Nonnegative-Real
                            Nonnegative-Real
                            Positive-Integer
                            2D-Render-Proc))
(define ((function-render-proc
          f samples
          color width style alpha
          marker marker-color marker-fill-color marker-size marker-line-width marker-alpha marker-count)
         area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample xs ys y-min y-max) (f x-ivl samples))
  (define vs (map (λ ([x : Real] [y : Real]) (vector x y)) xs ys))
  
  (send area put-alpha alpha)
    (send area put-pen color width style)
  (send area put-lines vs)

  (unless (eq? marker 'none)

    ;; Get a subset of the points from the samples to draw markers on.
    ;; Normally `samples` is a large number (500 by default) and, without
    ;; reducing their number, this draws too many markers on the plot.
    (define mvs
      (let ([vs-count (length vs)])
        (if (<= vs-count marker-count)
            vs
            (let ([segment-length : Integer (exact-round (/ vs-count (sub1 marker-count)))])
              (for/list : (Listof (Vector Real Real))
                        ([v : (Vector Real Real) (in-list vs)]
                         [n : Integer (in-naturals)]
                         #:when (or (= 0 (remainder n segment-length))
                                    ;; Always get the last point
                                    (= n (sub1 vs-count))))
                v)))))
    
    (send area put-alpha marker-alpha)
    (send area put-pen marker-color marker-line-width 'solid)
    (send area put-brush marker-fill-color 'solid)
    (send area put-glyphs mvs marker marker-size)))

(:: function
    (->* [(-> Real Real)]
         [(U Real #f) (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)
          #:marker Point-Sym
          #:marker-color (U Plot-Color 'auto)
          #:marker-fill-color (U Plot-Color 'auto)
          #:marker-size Nonnegative-Real
          #:marker-line-width Nonnegative-Real
          #:marker-alpha Nonnegative-Real
          #:marker-count Positive-Integer]
         renderer2d))
(define (function f [x-min #f] [x-max #f]
                  #:y-min [y-min #f] #:y-max [y-max #f]
                  #:samples [samples (line-samples)]
                  #:color [color (line-color)]
                  #:width [width (line-width)]
                  #:style [style (line-style)]
                  #:alpha [alpha (line-alpha)]
                  #:label [label #f]
                  #:marker [marker 'none]
                  #:marker-color [marker-color 'auto]
                  #:marker-fill-color [marker-fill-color 'auto]
                  #:marker-size [marker-size (point-size)]
                  #:marker-line-width [marker-line-width (point-line-width)]
                  #:marker-alpha [marker-alpha (point-alpha)]
                  #:marker-count [marker-count 20])
  (define fail/pos (make-raise-argument-error 'function f x-min x-max))
  (define fail/kw (make-raise-keyword-error 'function))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "rational?" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "rational?" 2)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [(not (rational? marker-size))  (fail/kw "rational?" '#:marker-size marker-size)]
    [(not (rational? marker-line-width))  (fail/kw "rational?" '#:marker-line-width marker-line-width)]
    [(or (> marker-alpha 1) (not (rational? marker-alpha)))  (fail/kw "real in [0,1]" '#:marker-alpha marker-alpha)]
    [(not (positive-integer? marker-count)) (fail/kw "positive-integer?" '#:marker-count marker-count)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (let* ([f  (function->sampler f x-ivl)]
            [marker-color
             (if (eq? marker-color 'auto)
                 (->pen-color color)
                 marker-color)]
            [marker-fill-color
             (if (eq? marker-fill-color 'auto)
                 (->pen-color marker-color)
                 marker-fill-color)])

       (define render-fun
         (function-render-proc f samples color width style alpha
                               marker marker-color marker-fill-color marker-size marker-line-width marker-alpha marker-count))

       (: legend-entries (U #f (-> Rect (Treeof legend-entry))))
       (define legend-entries
         (and label
              (let ([lle (line-legend-entry label color width style)])
                (if (eq? marker 'none)
                    (lambda (_) lle)
                    (let ([mle (point-legend-entry
                                label marker marker-color marker-fill-color marker-size
                                marker-line-width)])
                      (lambda (_)
                        (legend-entry
                         label
                         (lambda ([pd : (Instance Plot-Device%)]
                                  [w : Real]
                                  [h : Real])
                           ((legend-entry-draw lle) pd w h)
                           ((legend-entry-draw mle) pd w h)))))))))
       
       (renderer2d (vector x-ivl y-ivl)
                   (function-bounds-fun f samples)
                   default-ticks-fun
                   legend-entries
                   render-fun))]))

;; ===================================================================================================
;; Inverse function

(: inverse-render-proc (-> Sampler
                           Positive-Integer
                           Plot-Color
                           Nonnegative-Real
                           Plot-Pen-Style
                           Nonnegative-Real
                           Point-Sym
                           Plot-Color
                           Plot-Color
                           Nonnegative-Real
                           Nonnegative-Real
                           Nonnegative-Real
                           Positive-Integer
                           2D-Render-Proc))
(define ((inverse-render-proc
          f samples
          color width style alpha
          marker marker-color marker-fill-color marker-size marker-line-width marker-alpha marker-count)
         area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample ys xs x-min x-max) (f y-ivl samples))
  (define vs (map (λ ([x : Real] [y : Real]) (vector x y)) xs ys))
  
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines vs)

  (unless (eq? marker 'none)

    ;; Get a subset of the points from the samples to draw markers on.
    ;; Normally `samples` is a large number (500 by default) and, without
    ;; reducing their number, this draws too many markers on the plot.
    (define mvs
      (let ([vs-count (length vs)])
        (if (<= vs-count marker-count)
            vs
            (let ([segment-length : Integer (exact-round (/ vs-count (sub1 marker-count)))])
              (for/list : (Listof (Vector Real Real))
                        ([v : (Vector Real Real) (in-list vs)]
                         [n : Integer (in-naturals)]
                         #:when (or (= 0 (remainder n segment-length))
                                    ;; Always get the last point
                                    (= n (sub1 vs-count))))
                v)))))
    
    (send area put-alpha marker-alpha)
    (send area put-pen marker-color marker-line-width 'solid)
    (send area put-brush marker-fill-color 'solid)
    (send area put-glyphs mvs marker marker-size)))

(: inverse
   (->* [(-> Real Real)]
         [(U Real #f) (U Real #f)
          #:x-min (U Real #f) #:x-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:marker Point-Sym
          #:marker-color (U Plot-Color 'auto)
          #:marker-fill-color (U Plot-Color 'auto)
          #:marker-size Nonnegative-Real
          #:marker-line-width Nonnegative-Real
          #:marker-alpha Nonnegative-Real
          #:marker-count Positive-Integer
          #:label (U String pict #f)]
         renderer2d))
(define (inverse f [y-min #f] [y-max #f]
                 #:x-min [x-min #f] #:x-max [x-max #f]
                 #:samples [samples (line-samples)]
                 #:color [color (line-color)]
                 #:width [width (line-width)]
                 #:style [style (line-style)]
                 #:alpha [alpha (line-alpha)]
                 #:marker [marker 'none]
                 #:marker-color [marker-color 'auto]
                 #:marker-fill-color [marker-fill-color 'auto]
                 #:marker-size [marker-size (point-size)]
                 #:marker-line-width [marker-line-width (point-line-width)]
                 #:marker-alpha [marker-alpha (point-alpha)]
                 #:marker-count [marker-count 20]
                 #:label [label #f])
  (define fail/pos (make-raise-argument-error 'inverse f y-min y-max))
  (define fail/kw (make-raise-keyword-error 'inverse))
  (cond
    [(and y-min (not (rational? y-min)))  (fail/pos "rational?" 1)]
    [(and y-max (not (rational? y-max)))  (fail/pos "rational?" 2)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [(not (rational? marker-size))  (fail/kw "rational?" '#:marker-size marker-size)]
    [(not (rational? marker-line-width))  (fail/kw "rational?" '#:marker-line-width marker-line-width)]
    [(or (> marker-alpha 1) (not (rational? marker-alpha)))  (fail/kw "real in [0,1]" '#:marker-alpha marker-alpha)]
    [(not (positive-integer? marker-count)) (fail/kw "positive-integer?" '#:marker-count marker-count)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))

     (let* ([g (inverse->sampler f y-ivl)]
            [marker-color
             (if (eq? marker-color 'auto)
                 (->pen-color color)
                 marker-color)]
            [marker-fill-color
             (if (eq? marker-fill-color 'auto)
                 (->pen-color marker-color)
                 marker-fill-color)])

       (define render-fun
         (inverse-render-proc
          g samples color width style alpha
          marker marker-color marker-fill-color marker-size marker-line-width marker-alpha marker-count))

       (: legend-entries (U #f (-> Rect (Treeof legend-entry))))
       (define legend-entries
         (and label
              (let ([lle (line-legend-entry label color width style)])
                (if (eq? marker 'none)
                    (lambda (_) lle)
                    (let ([mle (point-legend-entry
                                label marker marker-color marker-fill-color marker-size
                                marker-line-width)])
                      (lambda (_)
                        (legend-entry
                         label
                         (lambda ([pd : (Instance Plot-Device%)]
                                  [w : Real]
                                  [h : Real])
                           ((legend-entry-draw lle) pd w h)
                           ((legend-entry-draw mle) pd w h)))))))))
       
       (renderer2d (vector x-ivl y-ivl)
                   (inverse-bounds-fun g samples)
                   default-ticks-fun
                   legend-entries
                   render-fun))]))

;; ===================================================================================================
;; Kernel density estimation

(:: density
    (->* [(Sequenceof Real)]
         [Nonnegative-Real
          (U (Sequenceof Real) #f)
          #:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String pict #f)]
         renderer2d))
(define (density xs [bw-adjust 1] [orig-ws #f]
                 #:x-min [x-min #f] #:x-max [x-max #f]
                 #:y-min [y-min #f] #:y-max [y-max #f]
                 #:samples [samples (line-samples)]
                 #:color [color (line-color)]
                 #:width [width (line-width)]
                 #:style [style (line-style)]
                 #:alpha [alpha (line-alpha)]
                 #:label [label #f])
  (define fail/pos (make-raise-argument-error 'density xs bw-adjust orig-ws))
  (define fail/kw (make-raise-keyword-error 'density))
  (define ws (if orig-ws (sequence->list orig-ws) #f))
  (cond
    [(not (rational? bw-adjust))  (fail/pos "rational?" 1)]
    [(and ws (not (andmap (λ ([w : Real]) (and (not (negative? w)) (rational? w))) ws)))
     (fail/pos "sequence of nonnegative rationals" 2)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([xs  (sequence->list xs)])
       (define n (length xs))
       (define sd (stddev xs ws))
       (define h (max 1e-308
                      (* 1e-14 (apply max (map abs (filter rational? xs))))
                      (* bw-adjust 1.06 sd (assert (expt n -0.2) real?))))
       (define-values (f fx-min fx-max) (kde xs h ws))
       (let ([x-min  (if x-min x-min fx-min)]
             [x-max  (if x-max x-max fx-max)])
         (function f x-min x-max #:y-min y-min #:y-max y-max #:samples samples
                   #:color color #:width width #:style style #:alpha alpha #:label label)))]))
