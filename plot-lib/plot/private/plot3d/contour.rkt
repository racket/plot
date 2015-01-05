#lang typed/racket/base

(require typed/racket/class racket/match racket/list
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; One contour line in 3D (using marching squares)

(: isoline3d-render-proc (-> 2D-Sampler Real Positive-Integer
                             Plot-Color Nonnegative-Real Plot-Pen-Style
                             Nonnegative-Real
                             (U #f String)
                             3D-Render-Proc))
(define ((isoline3d-render-proc f z samples color width style alpha label) area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl z-min z-max) z-ivl)
  (define num (animated-samples samples))
  (define sample (f (vector x-ivl y-ivl) (vector num num)))
  
  (when (and z-min z-max (<= z-min z z-max))
    (send area put-alpha alpha)
    (send area put-pen color width style)
    (for-2d-sample
     (xa xb ya yb z1 z2 z3 z4) sample
     (for ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
       (match-define (list v1 v2) line)
       (send area put-line v1 v2))))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(:: isoline3d
    (->* [(-> Real Real Real) Real]
         [(U #f Real) (U #f Real)
          (U #f Real) (U #f Real)
          #:z-min (U #f Real) #:z-max (U #f Real)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (isoline3d f z
                   [x-min #f] [x-max #f]
                   [y-min #f] [y-max #f]
                   #:z-min [z-min #f] #:z-max [z-max #f]
                   #:samples [samples (plot3d-samples)]
                   #:color [color (line-color)]
                   #:width [width (line-width)]
                   #:style [style (line-style)]
                   #:alpha [alpha (line-alpha)]
                   #:label [label #f])
  (define fail/pos (make-raise-argument-error 'isoline3d f z x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'isoline3d))
  (cond
    [(not (rational? z))  (fail/pos "rational?" 1)]
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 2)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 3)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 4)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 5)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-min z-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([z-min  (if z-min z-min z)]
           [z-max  (if z-max z-max z)])
       (define x-ivl (ivl x-min x-max))
       (define y-ivl (ivl y-min y-max))
       (define z-ivl (ivl z-min z-max))
       (define g (2d-function->sampler f (vector x-ivl y-ivl)))
       (renderer3d (vector x-ivl y-ivl z-ivl)
                   #f default-ticks-fun
                   (isoline3d-render-proc g z samples color width style alpha label)))]))

;; ===================================================================================================
;; Contour lines in 3D (using marching squares)

(: contours3d-render-proc (-> 2D-Sampler Contour-Levels Positive-Integer
                              (Plot-Colors (Listof Real))
                              (Pen-Widths (Listof Real))
                              (Plot-Pen-Styles (Listof Real))
                              (Alphas (Listof Real))
                              (U String #f)
                              3D-Render-Proc))
(define ((contours3d-render-proc f levels samples colors widths styles alphas label) area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (cond
    [(and z-min z-max)
     (define num (animated-samples samples))
     (define sample (f (vector x-ivl y-ivl) (vector num num)))
     ;; can't use the actual z ticks because some or all could be collapsed
     (match-define (list (tick #{zs : (Listof Real)}
                               #{_ : (Listof Boolean)}
                               #{labels : (Listof String)})
                         ...)
       (contour-ticks (plot-z-ticks) z-min z-max levels #f))
     
     (let* ([colors  (generate-list colors zs)]
            [widths  (generate-list widths zs)]
            [styles  (generate-list styles zs)]
            [alphas  (generate-list alphas zs)])
       (for ([z  (in-list zs)]
             [color  (in-cycle* (in-list colors))]
             [width : Nonnegative-Real  (in-cycle* (in-list widths))]
             [style  (in-cycle* (in-list styles))]
             [alpha : Nonnegative-Real  (in-cycle* (in-list alphas))])
         (send area put-alpha alpha)
         (send area put-pen color width style)
         (for-2d-sample
          (xa xb ya yb z1 z2 z3 z4) sample
          (for ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
            (match-define (list v1 v2) line)
            (send area put-line v1 v2))))
       
       (cond [label  (line-legend-entries label zs labels colors widths styles)]
             [else   empty]))]
    [else  empty]))

(:: contours3d
    (->* [(-> Real Real Real)]
         [(U #f Real) (U #f Real)
          (U #f Real) (U #f Real)
          #:z-min (U #f Real) #:z-max (U #f Real)
          #:samples Positive-Integer
          #:levels Contour-Levels
          #:colors (Plot-Colors (Listof Real))
          #:widths (Pen-Widths (Listof Real))
          #:styles (Plot-Pen-Styles (Listof Real))
          #:alphas (Alphas (Listof Real))
          #:label (U String #f)]
         renderer3d))
(define (contours3d f
                    [x-min #f] [x-max #f]
                    [y-min #f] [y-max #f]
                    #:z-min [z-min #f] #:z-max [z-max #f]
                    #:samples [samples (plot3d-samples)]
                    #:levels [levels (contour-levels)]
                    #:colors [colors (contour-colors)]
                    #:widths [widths (contour-widths)]
                    #:styles [styles (contour-styles)]
                    #:alphas [alphas (contour-alphas)]
                    #:label [label #f])
  (define fail/pos (make-raise-argument-error 'contours3d f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'contours3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 2)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 3)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 4)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-min z-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define z-ivl (ivl z-min z-max))
     (define g (2d-function->sampler f (vector x-ivl y-ivl)))
     (renderer3d (vector x-ivl y-ivl z-ivl)
                 (surface3d-bounds-fun g samples)
                 default-ticks-fun
                 (contours3d-render-proc g levels samples colors widths styles alphas label))]))

;; ===================================================================================================
;; Contour intervals in 3D (using marching squares)

(: contour-intervals3d-render-proc (-> 2D-Sampler Contour-Levels Positive-Integer
                                       (Plot-Colors (Listof ivl))
                                       (Plot-Brush-Styles (Listof ivl))
                                       (Plot-Colors (Listof ivl))
                                       (Pen-Widths (Listof ivl))
                                       (Plot-Pen-Styles (Listof ivl))
                                       (Plot-Colors (Listof Real))
                                       (Pen-Widths (Listof Real))
                                       (Plot-Pen-Styles (Listof Real))
                                       (Alphas (Listof ivl))
                                       (U String #f)
                                       3D-Render-Proc))
(define ((contour-intervals3d-render-proc
          f levels samples colors styles line-colors line-widths line-styles
          contour-colors contour-widths contour-styles alphas label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)
  (cond
    [(and z-min z-max)
     (define num (animated-samples samples))
     (define sample (f (vector x-ivl y-ivl) (vector num num)))
     ;; can't use the actual z ticks because some or all could be collapsed
     (match-define (list (tick #{zs : (Listof Real)}
                               #{_ : (Listof Boolean)}
                               #{labels : (Listof String)})
                         ...)
       (contour-ticks (plot-z-ticks) z-min z-max levels #t))
     
     (define-values (z-ivls ivl-labels)
       (for/lists ([z-ivls : (Listof ivl)]
                   [ivl-labels : (Listof String)]
                   ) ([za  (in-list zs)]
                      [zb  (in-list (rest zs))]
                      [la  (in-list labels)]
                      [lb  (in-list (rest labels))])
         (values (ivl za zb) (format "[~a,~a]" la lb))))
     
     (let* ([colors  (generate-list colors z-ivls)]
            [styles  (generate-list styles z-ivls)]
            [alphas  (generate-list alphas z-ivls)]
            [line-colors  (generate-list line-colors z-ivls)]
            [line-widths  (generate-list line-widths z-ivls)]
            [line-styles  (generate-list line-styles z-ivls)])
       (for ([za  (in-list zs)]
             [zb  (in-list (rest zs))]
             [color  (in-cycle* (in-list colors))]
             [style  (in-cycle* (in-list styles))]
             [alpha : Nonnegative-Real  (in-cycle* (in-list alphas))]
             [line-color  (in-cycle* (in-list line-colors))]
             [line-width : Nonnegative-Real  (in-cycle* (in-list line-widths))]
             [line-style  (in-cycle* (in-list line-styles))])
         (send area put-alpha alpha)
         (send area put-pen line-color line-width line-style)
         (send area put-brush color style)
         (for-2d-sample
          (xa xb ya yb z1 z2 z3 z4) sample
          (for ([vs  (in-list (heights->polys xa xb ya yb za zb z1 z2 z3 z4))])
            (define ls
              (for/list : (Listof Boolean) ([v1  (in-list (cons (last vs) vs))]
                                            [v2  (in-list vs)])
                (define z1 (vector-ref v1 2))
                (define z2 (vector-ref v2 2))
                (not (or (and (= z1 za) (= z2 za))
                         (and (= z1 zb) (= z2 zb))))))
            (send area put-polygon vs 'both ls))))
       
       ((contours3d-render-proc f levels samples contour-colors contour-widths contour-styles
                                alphas #f)
        area)
       
       (define n (- (length zs) 2))
       (define contour-colors*
         (append (list 0) (sequence-take (in-cycle* (generate-list contour-colors zs)) 0 n) (list 0)))
       (define contour-widths*
         (append (list 0) (sequence-take (in-cycle* (generate-list contour-widths zs)) 0 n) (list 0)))
       (define contour-styles*
         (append '(transparent) (sequence-take (in-cycle* (generate-list contour-styles zs)) 0 n)
                 '(transparent)))
       
       (cond [label  (interval-legend-entries
                      label z-ivls ivl-labels
                      colors styles line-colors line-widths line-styles
                      contour-colors* contour-widths* contour-styles*
                      (rest contour-colors*) (rest contour-widths*) (rest contour-styles*))]
             [else  empty]))]
    [else  empty]))

(:: contour-intervals3d
    (->* [(-> Real Real Real)]
         [(U #f Real) (U #f Real)
          (U #f Real) (U #f Real)
          #:z-min (U #f Real) #:z-max (U #f Real)
          #:samples Positive-Integer
          #:levels Contour-Levels
          #:colors (Plot-Colors (Listof ivl))
          #:styles (Plot-Brush-Styles (Listof ivl))
          #:line-colors (Plot-Colors (Listof ivl))
          #:line-widths (Pen-Widths (Listof ivl))
          #:line-styles (Plot-Pen-Styles (Listof ivl))
          #:contour-colors (Plot-Colors (Listof Real))
          #:contour-widths (Pen-Widths (Listof Real))
          #:contour-styles (Plot-Pen-Styles (Listof Real))
          #:alphas (Alphas (Listof ivl))
          #:label (U String #f)]
         renderer3d))
(define (contour-intervals3d f
                             [x-min #f] [x-max #f]
                             [y-min #f] [y-max #f]
                             #:z-min [z-min #f] #:z-max [z-max #f]
                             #:samples [samples (plot3d-samples)]
                             #:levels [levels (contour-levels)]
                             #:colors [colors (contour-interval-colors)]
                             #:styles [styles (contour-interval-styles)]
                             #:line-colors [line-colors (contour-interval-line-colors)]
                             #:line-widths [line-widths (contour-interval-line-widths)]
                             #:line-styles [line-styles (contour-interval-line-styles)]
                             #:contour-colors [contour-colors (contour-colors)]
                             #:contour-widths [contour-widths (contour-widths)]
                             #:contour-styles [contour-styles (contour-styles)]
                             #:alphas [alphas (contour-interval-alphas)]
                             #:label [label #f])
  (define fail/pos (make-raise-argument-error 'contour-intervals3d f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'contour-intervals3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 2)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 3)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 4)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-min z-max)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define z-ivl (ivl z-min z-max))
     (define g (2d-function->sampler f (vector x-ivl y-ivl)))
     (renderer3d (vector x-ivl y-ivl z-ivl)
                 (surface3d-bounds-fun g samples)
                 default-ticks-fun
                 (contour-intervals3d-render-proc g levels samples colors styles
                                                  line-colors line-widths line-styles
                                                  contour-colors contour-widths contour-styles
                                                  alphas label))]))
