#lang typed/racket/base

(require typed/racket/class racket/list racket/match
         plot/utils
         "../common/type-doc.rkt"
         "../common/utils.rkt")

(require/typed
 "../common/untyped-utils.rkt"
 [fix-vector-field3d-fun  (-> Symbol
                              (U (-> Real Real Real (Sequenceof Real))
                                 (-> (Vector Real Real Real) (Sequenceof Real)))
                              (-> Real Real Real (Vectorof Real)))])

(provide (all-defined-out))

;; ===================================================================================================

(: points3d-render-proc (-> (Listof (Vectorof Real)) Point-Sym
                            Plot-Color Plot-Color
                            Nonnegative-Real Nonnegative-Real
                            Nonnegative-Real
                            (U String #f)
                            3D-Render-Proc))
(define ((points3d-render-proc vs sym color fill-color size line-width alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color line-width 'solid)
  (send area put-brush fill-color 'solid)
  (send area put-glyphs vs sym size)
  
  (cond [label  (point-legend-entry label sym color fill-color size line-width)]
        [else   empty]))

(:: points3d
    (->* [(Sequenceof (Sequenceof Real))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:sym Point-Sym
          #:color Plot-Color
          #:fill-color (U Plot-Color 'auto)
          #:x-jitter Nonnegative-Real
          #:y-jitter Nonnegative-Real
          #:z-jitter Nonnegative-Real
          #:size Nonnegative-Real
          #:line-width Nonnegative-Real
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (points3d vs
                  #:x-min [x-min #f] #:x-max [x-max #f]
                  #:y-min [y-min #f] #:y-max [y-max #f]
                  #:z-min [z-min #f] #:z-max [z-max #f]
                  #:sym [sym (point-sym)]
                  #:color [color (point-color)]
                  #:fill-color [fill-color 'auto]
                  #:x-jitter [x-jitter (point-x-jitter)]
                  #:y-jitter [y-jitter (point-y-jitter)]
                  #:z-jitter [z-jitter (point-z-jitter)]
                  #:size [size (point-size)]
                  #:line-width [line-width (point-line-width)]
                  #:alpha [alpha (point-alpha)]
                  #:label [label #f])
  (define fail/kw (make-raise-keyword-error 'points3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(not (rational? size))  (fail/kw "rational?" '#:size size)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let* ([vs  (sequence->listof-vector 'points3d vs 3)]
            [vs  (filter vrational? vs)])
       (cond [(empty? vs)  (renderer3d #f #f #f #f)]
             [else
              (unless (= 0 x-jitter y-jitter z-jitter)
                (points-apply-jitters vs (vector x-jitter y-jitter z-jitter)
                                      #:ivls (vector (ivl x-min x-max)
                                                     (ivl y-min y-max)
                                                     (ivl z-min z-max))))
              (match-define (list (vector #{xs : (Listof Real)}
                                          #{ys : (Listof Real)}
                                          #{zs : (Listof Real)})
                                  ...)
                vs)
              (let ([x-min  (if x-min x-min (apply min* xs))]
                    [x-max  (if x-max x-max (apply max* xs))]
                    [y-min  (if y-min y-min (apply min* ys))]
                    [y-max  (if y-max y-max (apply max* ys))]
                    [z-min  (if z-min z-min (apply min* zs))]
                    [z-max  (if z-max z-max (apply max* zs))]
                    [fill-color  (if (eq? fill-color 'auto) (->pen-color color) fill-color)])
                (renderer3d
                 (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f default-ticks-fun
                 (points3d-render-proc vs sym color fill-color
                                       size line-width alpha label)))]))]))

;; ===================================================================================================

(: vector-field3d-render-fun
   (-> (-> Real Real Real (Vectorof Real))
       Positive-Integer (U Real 'auto 'normalized)
       Plot-Color Nonnegative-Real Plot-Pen-Style
       Nonnegative-Real
       (U String #f)
       3D-Render-Proc))
(define ((vector-field3d-render-fun f samples scale color line-width line-style alpha label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
    (send area get-bounds-rect))
  
  (cond
    [(and x-min x-max y-min y-max z-min z-max)
     (define xs0 (linear-seq x-min x-max samples #:start? #t #:end? #t))
     (define ys0 (linear-seq y-min y-max samples #:start? #t #:end? #t))
     (define zs0 (linear-seq z-min z-max samples #:start? #t #:end? #t))
     
     (define-values (vs dxs dys dzs norms mags)
       (for*/lists ([vs : (Listof (Vectorof Real))]
                    [dxs : (Listof Real)]
                    [dys : (Listof Real)]
                    [dzs : (Listof Real)]
                    [norms : (Listof (Vectorof Real))]
                    [mags : (Listof Nonnegative-Real)]
                    ) ([x   (in-list xs0)]
                       [y   (in-list ys0)]
                       [z   (in-list zs0)]
                       [dv  (in-value (f x y z))] #:when (vrational? dv))
         (match-define (vector dx dy dz) dv)
         (values (vector x y z) dx dy dz (vnormalize dv) (vmag dv))))
     
     (cond [(empty? vs)  empty]
           [else (define box-x-size (/ (- x-max x-min) samples))
                 (define box-y-size (/ (- y-max y-min) samples))
                 (define box-z-size (/ (- z-max z-min) samples))
                 
                 (define new-mags
                   (match scale
                     [(? real?)  (map (λ ([mag : Real]) (* scale mag)) mags)]
                     ['normalized  (make-list (length dxs) (min box-x-size box-y-size box-z-size))]
                     ['auto  (define dx-max (apply max (map abs dxs)))
                             (define dy-max (apply max (map abs dys)))
                             (define dz-max (apply max (map abs dzs)))
                             (define scale (min (/ box-x-size dx-max)
                                                (/ box-y-size dy-max)
                                                (/ box-z-size dz-max)))
                             (map (λ ([mag : Real]) (* scale mag)) mags)]))
                 
                 (send area put-alpha alpha)
                 (send area put-pen color line-width line-style)
                 (for ([v     (in-list vs)]
                       [norm  (in-list norms)]
                       [mag   (in-list new-mags)])
                   (send area put-arrow v (v+ v (v* norm mag))))
                 
                 (cond [label  (arrow-legend-entry label color line-width line-style)]
                       [else   empty])])]
    [else  empty]))

(:: vector-field3d
    (->* [(U (-> Real Real Real (Sequenceof Real))
             (-> (Vector Real Real Real) (Sequenceof Real)))]
         [(U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          (U Real #f) (U Real #f)
          #:samples Positive-Integer
          #:scale (U Real 'auto 'normalized)
          #:color Plot-Color
          #:line-width Nonnegative-Real
          #:line-style Plot-Pen-Style
          #:alpha Nonnegative-Real
          #:label (U String #f)]
         renderer3d))
(define (vector-field3d f [x-min #f] [x-max #f] [y-min #f] [y-max #f] [z-min #f] [z-max #f]
                        #:samples [samples (vector-field3d-samples)]
                        #:scale [scale (vector-field-scale)]
                        #:color [color (vector-field-color)]
                        #:line-width [line-width (vector-field-line-width)]
                        #:line-style [line-style (vector-field-line-style)]
                        #:alpha [alpha (vector-field-alpha)]
                        #:label [label #f])
  (define fail/pos (make-raise-argument-error 'vector-field3d f x-min x-max y-min y-max z-min z-max))
  (define fail/kw (make-raise-keyword-error 'vector-field3d))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/pos "#f or rational" 1)]
    [(and x-max (not (rational? x-max)))  (fail/pos "#f or rational" 2)]
    [(and y-min (not (rational? y-min)))  (fail/pos "#f or rational" 3)]
    [(and y-max (not (rational? y-max)))  (fail/pos "#f or rational" 4)]
    [(and z-min (not (rational? z-min)))  (fail/pos "#f or rational" 5)]
    [(and z-max (not (rational? z-max)))  (fail/pos "#f or rational" 6)]
    [(and (real? scale) (not (rational? scale)))
     (fail/kw "'auto, 'normalized or rational" '#:scale scale)]
    [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
    [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
    [else
     (let ([f  (fix-vector-field3d-fun 'vector-field3d f)])
       (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f default-ticks-fun
                   (vector-field3d-render-fun
                    f samples scale color line-width line-style alpha label)))]))
