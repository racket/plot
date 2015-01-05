#lang typed/racket/base

(require typed/racket/class racket/match racket/list math/flonum racket/math
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Surfaces of constant value (isosurfaces)

(: isosurface3d-render-proc (-> 3D-Sampler Real Positive-Integer
                                Plot-Color Plot-Brush-Style
                                Plot-Color Nonnegative-Real Plot-Pen-Style
                                Nonnegative-Real
                                (U String #f)
                                3D-Render-Proc))
(define ((isosurface3d-render-proc
          f d samples color style line-color line-width line-style alpha label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (define num (animated-samples samples))
  (define sample (f (vector x-ivl y-ivl z-ivl) (vector num num num)))
  (match-define (3d-sample xs ys zs dsss d-min d-max) sample)
  
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (for-3d-sample
   (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample
   (for ([vs  (in-list (heights->cube-polys xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))])
     (send area put-polygon vs)))
  
  (cond [label  (rectangle-legend-entry
                 label color style line-color line-width line-style)]
        [else  empty]))

(:: isosurface3d
    (->* [(-> Real Real Real Real) Real]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (isosurface3d f d [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
                      #:samples [samples (plot3d-samples)]
                      #:color [color (surface-color)]
                      #:style [style (surface-style)]
                      #:line-color [line-color (surface-line-color)]
                      #:line-width [line-width (surface-line-width)]
                      #:line-style [line-style (surface-line-style)]
                      #:alpha [alpha (surface-alpha)]
                      #:label [label #f])
  (define fail/pos (make-raise-argument-error 'isosurface3d f d x-min x-max y-min y-max z-min z-max))
  (define fail/kw (make-raise-keyword-error 'isosurface3d))
  (cond
    [(not (rational? d))  (fail/pos "rational?" 1)]
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 2)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 3)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 4)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 5)]
    [(and z-min (not (rational? z-min)))  (fail/pos "#f or rational" 6)]
    [(and z-max (not (rational? z-max)))  (fail/pos "#f or rational" 7)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define z-ivl (ivl z-min z-max))
     (define g (3d-function->sampler f (vector x-ivl y-ivl z-ivl)))
     (renderer3d (vector x-ivl y-ivl z-ivl) #f default-ticks-fun
                 (isosurface3d-render-proc
                  g d samples color style line-color line-width line-style alpha label))]))

;; ===================================================================================================
;; Nested isosurfaces

(: isosurfaces3d-render-proc
   (-> 3D-Sampler (U Real #f) (U Real #f) Contour-Levels Positive-Integer
       (Plot-Colors (Listof Real)) (Plot-Brush-Styles (Listof Real))
       (Plot-Colors (Listof Real)) (Pen-Widths (Listof Real)) (Plot-Pen-Styles (Listof Real))
       (Alphas (Listof Real))
       (U String #f)
       3D-Render-Proc))
(define ((isosurfaces3d-render-proc f rd-min rd-max levels samples colors styles
                                    line-colors line-widths line-styles alphas label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (define num (animated-samples samples))
  (define sample (f (vector x-ivl y-ivl z-ivl) (vector num num num)))
  (match-define (3d-sample xs ys zs dsss fd-min fd-max) sample)
  
  (define d-min (if rd-min rd-min fd-min))
  (define d-max (if rd-max rd-max fd-max))
  
  (match-define (list (tick #{ds : (Listof Real)}
                            #{_ : (Listof Bolean)}
                            #{labels : (Listof String)})
                      ...)
    (cond [(and d-min d-max)  (contour-ticks (plot-d-ticks) d-min d-max levels #f)]
          [else  empty]))
  
  (let* ([colors  (generate-list colors ds)]
         [styles  (generate-list styles ds)]
         [alphas  (generate-list alphas ds)]
         [line-colors  (generate-list line-colors ds)]
         [line-widths  (generate-list line-widths ds)]
         [line-styles  (generate-list line-styles ds)])
    (for ([d      (in-list ds)]
          [color  (in-cycle* colors)]
          [style  (in-cycle* styles)]
          [alpha : Nonnegative-Real  (in-cycle* alphas)]
          [line-color  (in-cycle* line-colors)]
          [line-width : Nonnegative-Real  (in-cycle* line-widths)]
          [line-style  (in-cycle* line-styles)])
      (send area put-alpha alpha)
      (send area put-brush color style)
      (send area put-pen line-color line-width line-style)
      (for-3d-sample
       (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample
       (for ([vs  (in-list (heights->cube-polys xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))])
         (send area put-polygon vs)))))
  
  (cond
    [(and label (not (empty? ds)))  (rectangle-legend-entries
                                     label ds colors styles line-colors line-widths line-styles)]
    [else  empty]))

(:: isosurfaces3d
    (->* [(-> Real Real Real Real)]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:d-min (U Real #f) #:d-max (U Real #f)
          #:samples Positive-Integer
          #:levels Contour-Levels
          #:colors (Plot-Colors (Listof Real))
          #:styles (Plot-Brush-Styles (Listof Real))
          #:line-colors (Plot-Colors (Listof Real))
          #:line-widths (Pen-Widths (Listof Real))
          #:line-styles (Plot-Pen-Styles (Listof Real))
          #:alphas (Alphas (Listof Real))
          #:label (U String #f)]
         renderer3d))
(define (isosurfaces3d f [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
                       #:d-min [d-min #f] #:d-max [d-max #f]
                       #:samples [samples (plot3d-samples)]
                       #:levels [levels (isosurface-levels)]
                       #:colors [colors (isosurface-colors)]
                       #:styles [styles (isosurface-styles)]
                       #:line-colors [line-colors (isosurface-line-colors)]
                       #:line-widths [line-widths (isosurface-line-widths)]
                       #:line-styles [line-styles (isosurface-line-styles)]
                       #:alphas [alphas (isosurface-alphas)]
                       #:label [label #f])
  (define fail/pos (make-raise-argument-error 'isosurfaces3d f x-min x-max y-min y-max z-min z-max))
  (define fail/kw (make-raise-keyword-error 'isosurfaces3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 2)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 3)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 4)]
    [(and z-min (not (rational? z-min)))  (fail/pos "#f or rational" 5)]
    [(and z-max (not (rational? z-max)))  (fail/pos "#f or rational" 6)]
    [(and d-min (not (rational? d-min)))  (fail/kw "#f or rational" '#:d-min d-min)]
    [(and d-max (not (rational? d-max)))  (fail/kw "#f or rational" '#:d-max d-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define z-ivl (ivl z-min z-max))
     (define g (3d-function->sampler f (vector x-ivl y-ivl z-ivl)))
     (renderer3d (vector x-ivl y-ivl z-ivl) #f default-ticks-fun
                 (isosurfaces3d-render-proc g d-min d-max levels samples colors styles
                                            line-colors line-widths line-styles alphas
                                            label))]))

;; ===================================================================================================

(: polar3d-render-proc (-> (-> Real Real Real Real) 3D-Sampler Positive-Integer
                           Plot-Color Plot-Brush-Style
                           Plot-Color Nonnegative-Real Plot-Pen-Style
                           Nonnegative-Real
                           (U String #f)
                           3D-Render-Proc))
(define ((polar3d-render-proc f g samples color style line-color line-width line-style alpha label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (define num (animated-samples samples))
  (define sample (g (vector x-ivl y-ivl z-ivl) (vector num num num)))
  (match-define (3d-sample xs ys zs dsss d-min d-max) sample)
  
  (: draw-cube (-> Real Real Real Real Real Real Real Real Real Real Real Real Real Real Real Void))
  (define (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8)
    (for ([vs  (in-list (heights->cube-polys xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))])
      (send area put-polygon vs)))
  
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (define d 0)
  (for-3d-sample
   (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample
   (cond [(and (xb . > . 0) (ya . < . 0) (yb . > . 0))
          (let* ([yb  -0.00001]
                 [d3  (f xb yb za)]
                 [d4  (f xa yb za)]
                 [d7  (f xb yb zb)]
                 [d8  (f xa yb zb)])
            (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))
          (let* ([ya  0.00001]
                 [d1  (f xa ya za)]
                 [d2  (f xb ya za)]
                 [d5  (f xa ya zb)]
                 [d6  (f xb ya zb)])
            (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))]
         [else
          (draw-cube xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8)]))
  
  (cond [label  (rectangle-legend-entry
                 label color style line-color line-width line-style)]
        [else  empty]))

(define 2pi (* 2 pi))

(: flmodulo (-> Flonum Flonum Flonum))
(define (flmodulo x y)
  (fl- x (fl* y (flfloor (fl/ x y)))))

(: 2d-polar->3d-function (-> (-> Real Real Real) (-> Real Real Real Real)))
(define ((2d-polar->3d-function f) x y z)
  (let ([x  (fl x)]
        [y  (fl y)]
        [z  (fl z)])
    (define-values (θ ρ)
      (cond [(and (fl= x 0.0) (fl= y 0.0))  (values 0.0 0.0)]
            [else  (values (flmodulo (atan y x) 2pi)
                           (flatan (fl/ z (fldistance x y))))]))
    (fl- (fl (f θ ρ)) (fldistance x y z))))

(:: polar3d
    (->* [(-> Real Real Real)]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:style Plot-Brush-Style
          #:line-color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (polar3d f
                 #:x-min [x-min #f] #:x-max [x-max #f]
                 #:y-min [y-min #f] #:y-max [y-max #f]
                 #:z-min [z-min #f] #:z-max [z-max #f]
                 #:samples [samples (plot3d-samples)]
                 #:color [color (surface-color)]
                 #:style [style (surface-style)]
                 #:line-color [line-color (surface-line-color)]
                 #:line-width [line-width (surface-line-width)]
                 #:line-style [line-style (surface-line-style)]
                 #:alpha [alpha (surface-alpha)]
                 #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'polar3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:x-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:x-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:x-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:x-max z-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define vs
       (for*/list : (Listof (Vectorof Real))
         ([θ  (in-list (linear-seq 0.0 2pi (* 4 samples)))]
          [ρ  (in-list (linear-seq (* -1/2 pi) (* 1/2 pi) (* 2 samples)))])
         (3d-polar->3d-cartesian θ ρ (f θ ρ))))
     (define rvs (filter vrational? vs))
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
              (define x-ivl (ivl x-min x-max))
              (define y-ivl (ivl y-min y-max))
              (define z-ivl (ivl z-min z-max))
              (define new-f (2d-polar->3d-function f))
              (define g (3d-function->sampler new-f (vector x-ivl y-ivl z-ivl)))
              (renderer3d (vector x-ivl y-ivl z-ivl) #f
                          default-ticks-fun
                          (polar3d-render-proc new-f g samples color style
                                               line-color line-width line-style alpha label)))])]))
