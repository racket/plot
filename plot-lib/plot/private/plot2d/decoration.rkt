#lang typed/racket/base

;; Renderers for plot decorations: axes, grids, labeled points, etc.

(require typed/racket/class typed/racket/draw racket/match racket/math racket/list
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt"
         "clip.rkt"
         "plot-area.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; X and Y axes

(: x-axis-render-proc (-> Real Boolean Boolean Boolean Nonnegative-Real 2D-Render-Proc))
(define ((x-axis-render-proc y ticks? labels? far? alpha) area)
  (match-define (vector (ivl x-min x-max) y-ivl) (send area get-bounds-rect))
  (define x-ticks (if far? (send area get-x-far-ticks) (send area get-x-ticks)))
  (define radius (if ticks? (* 1/2 (plot-tick-size)) 0))
  
  (when (and x-min x-max)
    (send area put-alpha alpha)
    (send area put-minor-pen)
    (send area put-line (vector x-min y) (vector x-max y)))
  
  (when ticks?
    (for ([t  (in-list x-ticks)])
      (match-define (tick x major? _) t)
      (if major? (send area put-major-pen) (send area put-minor-pen))
      (send area put-tick (vector x y) (if major? radius (* 1/2 radius)) (* 1/2 pi))))
  
  (when labels?
    (define dist (+ radius (pen-gap)))
    (for ([t  (in-list x-ticks)] #:when (pre-tick-major? t))
      (match-define (tick x _ label) t)
      (send area put-text label (vector x y) (if far? 'bottom 'top) 0 dist)))
  
  empty)

(:: x-axis
    (->* [] [Real #:ticks? Boolean #:labels? Boolean #:far? Boolean #:alpha Nonnegative-Real]
         renderer2d))
(define (x-axis [y 0]
                #:ticks? [ticks? (x-axis-ticks?)]
                #:labels? [labels? (x-axis-labels?)]
                #:far? [far? (x-axis-far?)]
                #:alpha [alpha (x-axis-alpha)])
  (cond
    [(not (rational? y))  (raise-argument-error 'x-axis "rational?" y)]
    [(or (> alpha 1) (not (rational? alpha)))  (raise-keyword-error 'x-axis "real in [0,1]"
                                                                    '#:alpha alpha)]
    [else
     (renderer2d #f #f #f (x-axis-render-proc y ticks? labels? far? alpha))]))

;; ---------------------------------------------------------------------------------------------------

(: y-axis-render-proc (-> Real Boolean Boolean Boolean Nonnegative-Real 2D-Render-Proc))
(define ((y-axis-render-proc x ticks? labels? far? alpha) area)
  (match-define (vector x-ivl (ivl y-min y-max)) (send area get-bounds-rect))
  (define y-ticks (if far? (send area get-y-far-ticks) (send area get-y-ticks)))
  (define radius (if ticks? (* 1/2 (plot-tick-size)) 0))
  
  (when (and y-min y-max)
    (send area put-alpha alpha)
    (send area put-minor-pen)
    (send area put-line (vector x y-min) (vector x y-max)))
  
  (when ticks?
    (for ([t  (in-list y-ticks)])
      (match-define (tick y major? _) t)
      (if major? (send area put-major-pen) (send area put-minor-pen))
      (send area put-tick (vector x y) (if major? radius (* 1/2 radius)) 0)))
  
  (when labels?
    (define dist (+ radius (pen-gap)))
    (for ([t  (in-list y-ticks)] #:when (pre-tick-major? t))
      (match-define (tick y _ label) t)
      (send area put-text label (vector x y) (if far? 'left 'right) 0 dist)))
  
  empty)

(:: y-axis
    (->* [] [Real #:ticks? Boolean #:labels? Boolean #:far? Boolean #:alpha Nonnegative-Real]
         renderer2d))
(define (y-axis [x 0]
                #:ticks? [ticks? (y-axis-ticks?)]
                #:labels? [labels? (y-axis-labels?)]
                #:far? [far? (y-axis-far?)]
                #:alpha [alpha (y-axis-alpha)])
  (cond
    [(not (rational? x))  (raise-argument-error 'y-axis "rational?" x)]
    [(or (> alpha 1) (not (rational? alpha)))  (raise-keyword-error 'y-axis "real in [0,1]"
                                                                    '#:alpha alpha)]
    [else
     (renderer2d #f #f #f (y-axis-render-proc x ticks? labels? far? alpha))]))

(:: axes
    (->* []
         [Real Real
          #:x-ticks? Boolean
          #:y-ticks? Boolean
          #:x-labels? Boolean
          #:y-labels? Boolean
          #:x-alpha Nonnegative-Real
          #:y-alpha Nonnegative-Real]
         (Listof renderer2d)))
(define (axes [x 0] [y 0]
              #:x-ticks? [x-ticks? (x-axis-ticks?)]
              #:y-ticks? [y-ticks? (y-axis-ticks?)]
              #:x-labels? [x-labels? (x-axis-labels?)]
              #:y-labels? [y-labels? (y-axis-labels?)]
              #:x-alpha [x-alpha (x-axis-alpha)]
              #:y-alpha [y-alpha (y-axis-alpha)])
  (list (x-axis y #:ticks? x-ticks? #:labels? x-labels? #:alpha x-alpha)
        (y-axis x #:ticks? y-ticks? #:labels? y-labels? #:alpha y-alpha)))

;; ===================================================================================================
;; Polar axes

(: build-polar-axes (->* [Natural Real Real Real Real] [Real]
                         (Values (Listof Real) (Listof Real) (Listof Real))))
(define (build-polar-axes num x-min x-max y-min y-max [start-θ 0])
  (define step (/ (* 2 pi) num))
  (define θs (build-list num (λ ([n : Index]) (+ start-θ (* n step)))))
  (define max-r (max (vmag (vector x-min y-min)) (vmag (vector x-min y-max))
                     (vmag (vector x-max y-max)) (vmag (vector x-max y-min))))
  (define-values (r-mins r-maxs)
    (for/lists ([r-mins : (Listof (U Real #f))]
                [r-maxs : (Listof (U Real #f))]
                ) ([θ  (in-list θs)])
      (define-values (v1 v2)
        (clip-line/bounds (vector 0 0) (vector (* max-r (cos θ)) (* max-r (sin θ)))
                          x-min x-max y-min y-max))
      (values (if v1 (vmag v1) #f)
              (if v2 (vmag v2) #f))))
  (for/lists ([θs : (Listof Real)]
              [r-mins : (Listof Real)]
              [r-maxs : (Listof Real)]
              ) ([θ  (in-list θs)]
                 [r-min  (in-list r-mins)]
                 [r-max  (in-list r-maxs)]
                 #:when (and r-min r-max (not (= r-min r-max))))
    (values θ r-min r-max)))

(: draw-polar-axis-ticks (-> Natural Boolean (Instance 2D-Plot-Area%) Void))
(define (draw-polar-axis-ticks num labels? area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  (when (and x-min x-max y-min y-max)
    (define-values (θs r-mins r-maxs) (build-polar-axes num x-min x-max y-min y-max
                                                        (* 1/2 (/ (* 2 pi) num))))
    (define corner-rs
      (list (vmag (vector x-min y-min)) (vmag (vector x-min y-max))
            (vmag (vector x-max y-max)) (vmag (vector x-max y-min))))
    (define r-min (if (and (<= x-min 0 x-max) (<= y-min 0 y-max)) 0 (apply min corner-rs)))
    (define r-max (apply max corner-rs))
    (define ts (filter (λ ([t : tick]) (not (zero? (pre-tick-value t))))
                       (ticks-generate (plot-r-ticks) r-min r-max)))
    ;; Draw the tick lines
    (for ([t  (in-list ts)])
      (match-define (tick r major? label) t)
      (if major? (send area put-minor-pen) (send area put-minor-pen 'long-dash))
      (define pts (for/list : (Listof (Vectorof Real)) ([θ  (in-list (linear-seq 0 (* 2 pi) 500))])
                    (vector (* r (cos θ)) (* r (sin θ)))))
      (send area put-lines pts))
    ;; Draw the labels
    (when (and labels? (not (empty? θs)))
      ;; Find the longest half-axis, rounded to drown out floating-point error
      (define mag (expt 10 (- (digits-for-range r-min r-max))))
      (match-define (list mθ mr-min mr-max)
        (argmax (λ ([lst : (Listof Real)]) (* (round (/ (- (third lst) (second lst)) mag)) mag))
                (map (λ ([θ : Real] [r-min : Real] [r-max : Real])
                       (list θ r-min r-max))
                     θs r-mins r-maxs)))
      ;; Actually draw the labels
      (for ([t  (in-list ts)])
        (match-define (tick r major? label) t)
        (when (and major? (<= mr-min r mr-max))
          (send area put-text label (vector (* r (cos mθ)) (* r (sin mθ))) 'center 0 0 #t))))))

(: draw-polar-axis-lines (-> Natural (Instance 2D-Plot-Area%) Void))
(define (draw-polar-axis-lines num area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  (when (and x-min x-max y-min y-max)
    (define-values (θs r-mins r-maxs) (build-polar-axes num x-min x-max y-min y-max))
    
    (send area put-minor-pen)
    (for ([θ  (in-list θs)] [r-min  (in-list r-mins)] [r-max  (in-list r-maxs)])
      (send area put-line
            (vector (* r-min (cos θ)) (* r-min (sin θ)))
            (vector (* r-max (cos θ)) (* r-max (sin θ)))))))

(: polar-axes-render-proc (-> Natural Boolean Boolean Nonnegative-Real 2D-Render-Proc))
(define ((polar-axes-render-proc num ticks? labels? alpha) area)
  (send area put-alpha alpha)
  (when (num . > . 0) (draw-polar-axis-lines num area))
  (when ticks? (draw-polar-axis-ticks (if (num . > . 0) num 12) labels? area))
  empty)

(:: polar-axes
    (->* [] [#:number Natural #:ticks? Boolean #:labels? Boolean #:alpha Nonnegative-Real]
         renderer2d))
(define (polar-axes #:number [num (polar-axes-number)]
                    #:ticks? [ticks? (polar-axes-ticks?)]
                    #:labels? [labels? (polar-axes-labels?)]
                    #:alpha [alpha (polar-axes-alpha)])
  (cond
    [(or (> alpha 1) (not (rational? alpha)))  (raise-keyword-error 'polar-axes "real in [0,1]"
                                                                    '#:alpha alpha)]
    [else
     (renderer2d #f #f #f (polar-axes-render-proc num ticks? labels? alpha))]))

;; ===================================================================================================
;; Grid

(: x-tick-lines-render-proc (-> 2D-Render-Proc))
(define ((x-tick-lines-render-proc) area)
  (match-define (vector x-ivl (ivl y-min y-max)) (send area get-bounds-rect))
  (when (and y-min y-max)
    (define x-ticks (send area get-x-ticks))
    (send area put-alpha 1/2)
    (for ([t  (in-list x-ticks)])
      (match-define (tick x major? _) t)
      (if major? (send area put-minor-pen) (send area put-minor-pen 'long-dash))
      (send area put-line (vector x y-min) (vector x y-max))))
  empty)

(: y-tick-lines-render-proc (-> 2D-Render-Proc))
(define ((y-tick-lines-render-proc) area)
  (match-define (vector (ivl x-min x-max) y-ivl) (send area get-bounds-rect))
  (when (and x-min x-max)
    (define y-ticks (send area get-y-ticks))
    (send area put-alpha 1/2)
    (for ([t  (in-list y-ticks)])
      (match-define (tick y major? _) t)
      (if major? (send area put-minor-pen) (send area put-minor-pen 'long-dash))
      (send area put-line (vector x-min y) (vector x-max y))))
  empty)

(:: x-tick-lines (-> renderer2d))
(define (x-tick-lines)
  (renderer2d #f #f #f (x-tick-lines-render-proc)))

(:: y-tick-lines (-> renderer2d))
(define (y-tick-lines)
  (renderer2d #f #f #f (y-tick-lines-render-proc)))

(:: tick-grid (-> (Listof renderer2d)))
(define (tick-grid)
  (list (x-tick-lines) (y-tick-lines)))

;; ===================================================================================================
;; Labeled points

(: format-coordinate (-> (Vectorof Real) (Instance 2D-Plot-Area%) String))
(define (format-coordinate v area)
  (match-define (vector x y) v)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  (match-define (list x-str)
    (cond [(and x-min x-max)  (format-tick-labels (plot-x-ticks) x-min x-max (list x))]
          [else  "?"]))
  (match-define (list y-str)
    (cond [(and y-min y-max)  (format-tick-labels (plot-y-ticks) y-min y-max (list y))]
          [else  "?"]))
  (format "(~a,~a)" x-str y-str))

(: label-render-proc (-> (U String #f) (Vectorof Real)
                         Plot-Color Nonnegative-Real (U String #f) Font-Family
                         Anchor Real
                         Plot-Color Plot-Color Nonnegative-Real Nonnegative-Real Point-Sym
                         Nonnegative-Real
                         2D-Render-Proc))
(define ((label-render-proc label v color size face family anchor angle
                            point-color point-fill-color point-size point-line-width point-sym
                            alpha)
         area)
  (let ([label  (if label label (format-coordinate v area))])
    (send area put-alpha alpha)
    ; label
    (send area put-text-foreground color)
    (send area put-font-attribs size face family)
    (send area put-text (string-append " " label " ") v anchor angle (* 1/2 point-size) #t)
    ; point
    (send area put-pen point-color point-line-width 'solid)
    (send area put-brush point-fill-color 'solid)
    (send area put-glyphs (list v) point-sym point-size))
  
  empty)

(:: point-label
    (->* [(Sequenceof Real)]
         [(U String #f)
          #:color Plot-Color
          #:size Nonnegative-Real
          #:face (U String #f)
          #:family Font-Family
          #:anchor Anchor
          #:angle Real
          #:point-color Plot-Color
          #:point-fill-color (U Plot-Color 'auto)
          #:point-size Nonnegative-Real
          #:point-line-width Nonnegative-Real
          #:point-sym Point-Sym
          #:alpha Nonnegative-Real]
         renderer2d))
(define (point-label v [label #f]
                     #:color [color (plot-foreground)]
                     #:size [size (plot-font-size)]
                     #:face [face (plot-font-face)]
                     #:family [family (plot-font-family)]
                     #:anchor [anchor (label-anchor)]
                     #:angle [angle (label-angle)]
                     #:point-color [point-color (point-color)]
                     #:point-fill-color [point-fill-color 'auto]
                     #:point-size [point-size (label-point-size)]
                     #:point-line-width [point-line-width (point-line-width)]
                     #:point-sym [point-sym 'fullcircle]
                     #:alpha [alpha (label-alpha)])
  (define fail/kw (make-raise-keyword-error 'point-label))
  (cond
    [(not (rational? size))  (fail/kw "rational?" '#:size size)]
    [(not (rational? angle))  (fail/kw "rational?" '#:angle angle)]
    [(not (rational? point-size))  (fail/kw "rational?" '#:point-size point-size)]
    [(not (rational? point-line-width))  (fail/kw "rational?" '#:point-line-width point-line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([v  (sequence-head-vector 'point-label v 2)])
       (match-define (vector x y) v)
       (renderer2d (vector (ivl x x) (ivl y y)) #f #f
                   (label-render-proc
                    label v color size face family anchor angle
                    point-color (cond [(eq? point-fill-color 'auto)  (->pen-color point-color)]
                                      [else  point-fill-color])
                    point-size point-line-width point-sym
                    alpha)))]))

(:: parametric-label
    (->* [(-> Real (Sequenceof Real)) Real]
         [(U String #f)
          #:color Plot-Color
          #:size Nonnegative-Real
          #:face (U String #f)
          #:family Font-Family
          #:anchor Anchor
          #:angle Real
          #:point-color Plot-Color
          #:point-fill-color (U Plot-Color 'auto)
          #:point-size Nonnegative-Real
          #:point-line-width Nonnegative-Real
          #:point-sym Point-Sym
          #:alpha Nonnegative-Real]
         renderer2d))
(define (parametric-label
         f t [label #f]
         #:color [color (plot-foreground)]
         #:size [size (plot-font-size)]
         #:face [face (plot-font-face)]
         #:family [family (plot-font-family)]
         #:anchor [anchor (label-anchor)]
         #:angle [angle (label-angle)]
         #:point-color [point-color (point-color)]
         #:point-fill-color [point-fill-color 'auto]
         #:point-size [point-size (label-point-size)]
         #:point-line-width [point-line-width (point-line-width)]
         #:point-sym [point-sym 'fullcircle]
         #:alpha [alpha (label-alpha)])
  (cond
    [(not (rational? t))  (raise-argument-error 'parametric-label "rational?" 1 f t label)]
    [else
     (point-label
      (sequence-head-vector 'parametric-label (f t) 2) 
      label
      #:color color #:size size #:face face #:family family #:anchor anchor #:angle angle
      #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
      #:point-line-width point-line-width #:point-sym point-sym
      #:alpha alpha)]))

(:: polar-label
    (->* [(-> Real Real) Real]
         [(U String #f)
          #:color Plot-Color
          #:size Nonnegative-Real
          #:face (U String #f)
          #:family Font-Family
          #:anchor Anchor
          #:angle Real
          #:point-color Plot-Color
          #:point-fill-color (U Plot-Color 'auto)
          #:point-size Nonnegative-Real
          #:point-line-width Nonnegative-Real
          #:point-sym Point-Sym
          #:alpha Nonnegative-Real]
         renderer2d))
(define (polar-label
         f θ [label #f]
         #:color [color (plot-foreground)]
         #:size [size (plot-font-size)]
         #:face [face (plot-font-face)]
         #:family [family (plot-font-family)]
         #:anchor [anchor (label-anchor)]
         #:angle [angle (label-angle)]
         #:point-color [point-color (point-color)]
         #:point-fill-color [point-fill-color 'auto]
         #:point-size [point-size (label-point-size)]
         #:point-line-width [point-line-width (point-line-width)]
         #:point-sym [point-sym 'fullcircle]
         #:alpha [alpha (label-alpha)])
  (cond
    [(not (rational? θ))  (raise-argument-error 'polar-label "rational?" 1 f θ label)]
    [else
     (point-label
      (polar->cartesian θ (f θ)) label
      #:color color #:size size #:face face #:family family #:anchor anchor #:angle angle
      #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
      #:point-line-width point-line-width #:point-sym point-sym
      #:alpha alpha)]))

(:: function-label
    (->* [(-> Real Real) Real]
         [(U String #f)
          #:color Plot-Color
          #:size Nonnegative-Real
          #:face (U String #f)
          #:family Font-Family
          #:anchor Anchor
          #:angle Real
          #:point-color Plot-Color
          #:point-fill-color (U Plot-Color 'auto)
          #:point-size Nonnegative-Real
          #:point-line-width Nonnegative-Real
          #:point-sym Point-Sym
          #:alpha Nonnegative-Real]
         renderer2d))
(define (function-label
         f x [label #f]
         #:color [color (plot-foreground)]
         #:size [size (plot-font-size)]
         #:face [face (plot-font-face)]
         #:family [family (plot-font-family)]
         #:anchor [anchor (label-anchor)]
         #:angle [angle (label-angle)]
         #:point-color [point-color (point-color)]
         #:point-fill-color [point-fill-color 'auto]
         #:point-size [point-size (label-point-size)]
         #:point-line-width [point-line-width (point-line-width)]
         #:point-sym [point-sym 'fullcircle]
         #:alpha [alpha (label-alpha)])
  (cond
    [(not (rational? x))  (raise-argument-error 'function-label "rational" 1 f x label)]
    [else
     (point-label
      (vector x (f x)) label
      #:color color #:size size #:face face #:family family #:anchor anchor #:angle angle
      #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
      #:point-line-width point-line-width #:point-sym point-sym
      #:alpha alpha)]))

(:: inverse-label
    (->* [(-> Real Real) Real]
         [(U String #f)
          #:color Plot-Color
          #:size Nonnegative-Real
          #:face (U String #f)
          #:family Font-Family
          #:anchor Anchor
          #:angle Real
          #:point-color Plot-Color
          #:point-fill-color (U Plot-Color 'auto)
          #:point-size Nonnegative-Real
          #:point-line-width Nonnegative-Real
          #:point-sym Point-Sym
          #:alpha Nonnegative-Real]
         renderer2d))
(define (inverse-label
         f y [label #f]
         #:color [color (plot-foreground)]
         #:size [size (plot-font-size)]
         #:face [face (plot-font-face)]
         #:family [family (plot-font-family)]
         #:anchor [anchor (label-anchor)]
         #:angle [angle (label-angle)]
         #:point-color [point-color (point-color)]
         #:point-fill-color [point-fill-color 'auto]
         #:point-size [point-size (label-point-size)]
         #:point-line-width [point-line-width (point-line-width)]
         #:point-sym [point-sym 'fullcircle]
         #:alpha [alpha (label-alpha)])
  (cond
    [(not (rational? y))  (raise-argument-error 'inverse-label "rational?" 1 f y label)]
    [else
     (point-label
      (vector (f y) y) label
      #:color color #:size size #:face face #:family family #:anchor anchor #:angle angle
      #:point-color point-color #:point-fill-color point-fill-color #:point-size point-size
      #:point-line-width point-line-width #:point-sym point-sym
      #:alpha alpha)]))
