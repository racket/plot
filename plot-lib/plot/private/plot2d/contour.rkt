#lang typed/racket/base

;; Renderers for contour lines and contour intervals

(require typed/racket/class racket/match racket/list racket/vector
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; One contour line

(: isoline-render-proc (-> 2D-Sampler Real Positive-Integer
                           Plot-Color Nonnegative-Real Plot-Pen-Style
                           Nonnegative-Real
                           (U String #f)
                           2D-Render-Proc))
(define ((isoline-render-proc g z samples color width style alpha label) area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (define num (animated-samples samples))
  (define sample (g (vector x-ivl y-ivl) (vector num num)))
  (match-define (2d-sample xs ys zss z-min z-max) sample)
  
  (when (and z-min z-max (<= z-min z z-max))
    (send area put-alpha alpha)
    (send area put-pen color width style)
    (for-2d-sample
     (xa xb ya yb z1 z2 z3 z4) sample
     (for ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
       (match-define (list v1 v2) (map (λ ([v : (Vectorof Real)]) (vector-take v 2)) line))
       (send area put-line v1 v2))))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(:: isoline
    (->* [(-> Real Real Real) Real]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:samples Positive-Integer
          #:color Plot-Color
          #:width Nonnegative-Real
          #:style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer2d))
(define (isoline f z [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                 #:samples [samples (contour-samples)]
                 #:color [color (line-color)]
                 #:width [width (line-width)]
                 #:style [style (line-style)]
                 #:alpha [alpha (line-alpha)]
                 #:label [label #f])
  (define fail/pos (make-raise-argument-error 'isoline f z x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'isoline))
  (cond
    [(not (rational? z))  (fail/pos "rational?" 1)]
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 2)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 3)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 4)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 5)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [(not (rational? width))  (fail/kw "rational?" '#:width width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define g (2d-function->sampler f (vector x-ivl y-ivl)))
     (renderer2d (vector x-ivl y-ivl) #f default-ticks-fun
                 (isoline-render-proc g z samples color width style alpha label))]))

;; ===================================================================================================
;; Contour lines

(: contours-render-proc (-> 2D-Sampler Contour-Levels Positive-Integer
                            (Plot-Colors (Listof Real))
                            (Pen-Widths (Listof Real))
                            (Plot-Pen-Styles (Listof Real))
                            (Alphas (Listof Real))
                            (U String #f)
                            2D-Render-Proc))
(define ((contours-render-proc g levels samples colors widths styles alphas label) area)
  (let/ec return : (Treeof legend-entry)
    (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
    (match-define (ivl x-min x-max) x-ivl)
    (match-define (ivl y-min y-max) y-ivl)
    (define num (animated-samples samples))
    (define sample (g (vector x-ivl y-ivl) (vector num num)))
    (match-define (2d-sample xs ys zss z-min z-max) sample)
    
    (unless (and z-min z-max) (return empty))
    
    (match-define (list (tick #{zs : (Listof Real)}
                              #{_ : (Listof Boolean)}
                              #{labels : (Listof String)})
                        ...)
      (contour-ticks (plot-z-ticks) (assert z-min values) (assert z-max values) levels #f))
    
    (let* ([colors  (generate-list colors zs)]
           [widths  (generate-list widths zs)]
           [styles  (generate-list styles zs)]
           [alphas  (generate-list alphas zs)])
      (for ([z      (in-list zs)]
            [color  (in-cycle* colors)]
            [width : Nonnegative-Real  (in-cycle* widths)]
            [style  (in-cycle* styles)]
            [alpha : Nonnegative-Real  (in-cycle* alphas)])
        (send area put-alpha alpha)
        (send area put-pen color width style)
        (for-2d-sample
         (xa xb ya yb z1 z2 z3 z4) sample
         (for ([line  (in-list (heights->lines xa xb ya yb z z1 z2 z3 z4))])
           (match-define (list v1 v2) (map (λ ([v : (Vectorof Real)]) (vector-take v 2)) line))
           (send area put-line v1 v2)))))
    
    (cond [(and label (not (empty? zs)))  (line-legend-entries label zs labels colors widths styles)]
          [else  empty])))

(:: contours
    (->* [(-> Real Real Real)]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:samples Positive-Integer
          #:levels Contour-Levels
          #:colors (Plot-Colors (Listof Real))
          #:widths (Pen-Widths (Listof Real))
          #:styles (Plot-Pen-Styles (Listof Real))
          #:alphas (Alphas (Listof Real))
          #:label (U String #f)]
         renderer2d))
(define (contours f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                  #:samples [samples (contour-samples)]
                  #:levels [levels (contour-levels)]
                  #:colors [colors (contour-colors)]
                  #:widths [widths (contour-widths)]
                  #:styles [styles (contour-styles)]
                  #:alphas [alphas (contour-alphas)]
                  #:label [label #f])
  (define fail/pos (make-raise-argument-error 'contours f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'contours))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 2)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 3)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 4)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 5)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define g (2d-function->sampler f (vector x-ivl y-ivl)))
     (renderer2d (vector x-ivl y-ivl) #f default-ticks-fun
                 (contours-render-proc g levels samples colors widths styles alphas label))]))

;; ===================================================================================================
;; Contour intervals

(: contour-intervals-render-proc
   (-> 2D-Sampler Contour-Levels Positive-Integer
       (Plot-Colors (Listof ivl)) (Plot-Brush-Styles (Listof ivl))
       (Plot-Colors (Listof Real)) (Pen-Widths (Listof Real)) (Plot-Pen-Styles (Listof Real))
       (Alphas (Listof ivl))
       (U String #f)
       2D-Render-Proc))
(define ((contour-intervals-render-proc
          g levels samples colors styles contour-colors contour-widths contour-styles alphas label)
         area)
  (let/ec return : (Treeof legend-entry)
    (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
    (match-define (ivl x-min x-max) x-ivl)
    (match-define (ivl y-min y-max) y-ivl)
    (define num (animated-samples samples))
    (define sample (g (vector x-ivl y-ivl) (vector num num)))
    (match-define (2d-sample xs ys zss z-min z-max) sample)
    
    (unless (and z-min z-max) (return empty))
    
    (match-define (list (tick #{zs : (Listof Real)}
                              #{_ : (Listof Boolean)}
                              #{labels : (Listof String)})
                        ...)
      (contour-ticks (plot-z-ticks) (assert z-min values) (assert z-max values) levels #t))
    
    (define-values (z-ivls ivl-labels)
      (for/lists ([z-ivls : (Listof ivl)]
                  [ivl-labels : (Listof String)]
                  ) ([za  (in-list zs)]
                     [zb  (in-list (rest zs))]
                     [la  (in-list labels)]
                     [lb  (in-list (rest labels))])
        (values (ivl za zb) (format "[~a,~a]" la lb))))
    
    (send area put-pen 0 1 'transparent)
    (let* ([colors  (map ->brush-color (generate-list colors z-ivls))]
           [styles  (map ->brush-style (generate-list styles z-ivls))]
           [alphas  (generate-list alphas z-ivls)])
      (for ([za     (in-list zs)]
            [zb     (in-list (rest zs))]
            [color : (List Real Real Real)  (in-cycle* colors)]
            [style : Plot-Brush-Style  (in-cycle* styles)]
            [alpha : Nonnegative-Real  (in-cycle* alphas)])
        (send area put-brush color style)
        (send area put-alpha alpha)
        (for-2d-sample
         (xa xb ya yb z1 z2 z3 z4) sample
         (for ([poly  (in-list (heights->polys xa xb ya yb za zb z1 z2 z3 z4))])
           (send area put-polygon (map (λ ([v : (Vectorof Real)]) (vector-take v 2)) poly)))))
      
      ((contours-render-proc g levels samples contour-colors contour-widths contour-styles alphas #f)
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
                     colors styles colors '(1) '(transparent)
                     contour-colors* contour-widths* contour-styles*
                     (rest contour-colors*) (rest contour-widths*) (rest contour-styles*))]
            [else   empty]))))

(:: contour-intervals
    (->* [(-> Real Real Real)]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:samples Positive-Integer
          #:levels Contour-Levels
          #:colors (Plot-Colors (Listof ivl))
          #:styles (Plot-Brush-Styles (Listof ivl))
          #:contour-colors (Plot-Colors (Listof Real))
          #:contour-widths (Pen-Widths (Listof Real))
          #:contour-styles (Plot-Pen-Styles (Listof Real))
          #:alphas (Alphas (Listof ivl))
          #:label (U String #f)]
         renderer2d))
(define (contour-intervals
         f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
         #:samples [samples (contour-samples)]
         #:levels [levels (contour-levels)]
         #:colors [colors (contour-interval-colors)]
         #:styles [styles (contour-interval-styles)]
         #:contour-colors [contour-colors (contour-colors)]
         #:contour-widths [contour-widths (contour-widths)]
         #:contour-styles [contour-styles (contour-styles)]
         #:alphas [alphas (contour-interval-alphas)]
         #:label [label #f])
  (define fail/pos (make-raise-argument-error 'contour-intervals f x-min x-max y-min y-max))
  (define fail/kw (make-raise-keyword-error 'contour-intervals))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 2)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 3)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 4)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 5)]
    [(< samples 2)  (fail/kw "Integer >= 2" '#:samples samples)]
    [else
     (define x-ivl (ivl x-min x-max))
     (define y-ivl (ivl y-min y-max))
     (define g (2d-function->sampler f (vector x-ivl y-ivl)))
     (renderer2d (vector x-ivl y-ivl) #f default-ticks-fun
                 (contour-intervals-render-proc g levels samples colors styles
                                                contour-colors contour-widths contour-styles
                                                alphas label))]))
