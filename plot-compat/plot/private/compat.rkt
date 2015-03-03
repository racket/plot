#lang racket/base

;; A compatibility module for the old 'plot'.

(require racket/contract racket/class racket/snip racket/draw racket/vector
         ;; Plotting
         plot/private/common/math
         plot/private/common/contract
         plot/private/common/ticks
         plot/private/plot2d/plot-area
         plot/private/plot2d/renderer
         plot/private/plot3d/plot-area
         plot/private/plot3d/renderer
         (prefix-in new. (only-in plot
                                  x-axis y-axis
                                  plot-x-ticks plot-y-ticks plot-z-ticks
                                  points error-bars vector-field
                                  plot-title plot-x-label plot-y-label plot-z-label
                                  plot-foreground plot-background
                                  plot3d-angle plot3d-altitude))
         plot/private/deprecated/renderers
         ;; Miscellaneous
         plot/private/deprecated/math)

(provide
 mix
 (contract-out
  [plot-color?  (-> any/c boolean?)]
  [plot
   (->* [((is-a?/c 2d-plot-area%) . -> . void?)]
        [#:width real?
         #:height real?
         #:x-min real?
         #:x-max real?
         #:y-min real?
         #:y-max real?
         #:x-label string?
         #:y-label string?
         #:title string?
         #:fgcolor (list/c byte? byte? byte?)
         #:bgcolor (list/c byte? byte? byte?)
         #:lncolor (list/c byte? byte? byte?)
         #:out-file (or/c path-string? output-port? #f)]
        (is-a?/c image-snip%))]
  [plot3d
   (->* [((is-a?/c 3d-plot-area%) . -> . void?)]
        [#:width real?
         #:height real?
         #:x-min real?
         #:x-max real?
         #:y-min real?
         #:y-max real?
         #:z-min real?
         #:z-max real?
         #:alt real?
         #:az real?
         #:x-label string?
         #:y-label string?
         #:z-label string?
         #:title string?
         #:fgcolor (list/c byte? byte? byte?)
         #:bgcolor (list/c byte? byte? byte?)
         #:lncolor (list/c byte? byte? byte?)
         #:out-file (or/c path-string? output-port? #f)]
        (is-a?/c image-snip%))]
  [points
   (->* [(listof (vectorof real?))]
        [#:sym (or/c char? string? exact-integer? symbol?)
         #:color plot-color?]
        ((is-a?/c 2d-plot-area%) . -> . void?))]
  [vector-field
   (->* [((vector/c real? real?) . -> . (vector/c real? real?))]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:width exact-positive-integer?
         #:color plot-color?
         #:style (one-of/c 'scaled 'normalized 'real)]
        ((is-a?/c 2d-plot-area%) . -> . void?))]
  [error-bars
   (->* [(listof (vector/c real? real? real?))]
        [#:color plot-color?]
        ((is-a?/c 2d-plot-area%) . -> . void?))]
  [line
   (->* [(real? . -> . (or/c real? (vector/c real? real?)))]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:width (>=/c 0)
         #:color plot-color/c
         #:mode (one-of/c 'standard 'parametric)
         #:mapping (one-of/c 'cartesian 'polar)
         #:t-min real?
         #:t-max real?]
        ((is-a?/c 2d-plot-area%) . -> . void?))]
  [contour
   (->* [(real? real? . -> . real?)]
        [#:samples exact-nonnegative-integer?
         #:width (>=/c 0)
         #:color plot-color/c
         #:levels (or/c (and/c exact-integer? (>=/c 2)) (listof real?))]
        ((is-a?/c 2d-plot-area%) . -> . void?))]
  [shade
   (->* [(real? real? . -> . real?)]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:levels (or/c (and/c exact-integer? (>=/c 2)) (listof real?))]
        ((is-a?/c 2d-plot-area%) . -> . void?))]
  [surface
   (->* [(real? real? . -> . real?)]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:width (>=/c 0)
         #:color plot-color/c]
        ((is-a?/c 3d-plot-area%) . -> . void?))]
  )
 ;; Miscellaneous
 make-vec
 derivative
 gradient)

(define (mix . data)
  (for/fold ([f  (λ (area) (void))]) ([d  (in-list data)])
    (λ (area)
      (f area)
      (d area)
      (void))))

(define (plot-color? v)
  (and (member v '(white black yellow green aqua pink wheat gray brown blue violet cyan
                         turquoise magenta salmon red))
       #t))

(define ((renderer2d->plot-data r) area)
  ((renderer2d-render-proc r) area)
  (void))

(define ((renderer3d->plot-data r) area)
  ((renderer3d-render-proc r) area)
  (void))

;; ===================================================================================================
;; Plotting

(define x-axis-data (renderer2d->plot-data (new.x-axis)))
(define y-axis-data (renderer2d->plot-data (new.y-axis)))

(define (plot data
              #:width [width 400]
              #:height [height 400]
              #:x-min [x-min -5]
              #:x-max [x-max 5]
              #:y-min [y-min -5]
              #:y-max [y-max 5]
              #:x-label [x-label "X axis"]
              #:y-label [y-label "Y axis"]
              #:title [title ""]
              #:fgcolor [fgcolor '(0 0 0)]
              #:bgcolor [bgcolor '(255 255 255)]
              #:lncolor [lncolor '(255 0 0)]
              #:out-file [out-file #f])
  (define x-ticks (ticks-generate (new.plot-x-ticks) x-min x-max))
  (define y-ticks (ticks-generate (new.plot-y-ticks) y-min y-max))
  (define bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max)))
  
  (parameterize ([new.plot-title       title]
                 [new.plot-x-label     x-label]
                 [new.plot-y-label     y-label]
                 [new.plot-foreground  fgcolor]
                 [new.plot-background  bgcolor])
    (define bm (make-bitmap (ceiling width) (ceiling height)))
    (define dc (make-object bitmap-dc% bm))
    (define area (make-object 2d-plot-area%
                   bounds-rect x-ticks x-ticks y-ticks y-ticks dc 0 0 width height))
    
    (define data+axes (mix x-axis-data y-axis-data data))
    
    (send area start-plot)
    (send area start-renderer bounds-rect)
    (data+axes area)
    (send area end-renderers)
    (send area end-plot)
    
    (when out-file (send bm save-file out-file 'png))
    
    (make-object image-snip% bm)))

(define (plot3d data
                #:width [width 400]
                #:height [height 400]
                #:x-min [x-min -5]
                #:x-max [x-max 5]
                #:y-min [y-min -5]
                #:y-max [y-max 5]
                #:z-min [z-min -5]
                #:z-max [z-max 5]
                #:alt [alt 30]
                #:az [az 45]
                #:x-label [x-label "X axis"]
                #:y-label [y-label "Y axis"]
                #:z-label [z-label "Z axis"]
                #:title [title ""]
                #:fgcolor [fgcolor '(0 0 0)]
                #:bgcolor [bgcolor '(255 255 255)]
                #:lncolor [lncolor '(255 0 0)]
                #:out-file [out-file #f])
  (define x-ticks (ticks-generate (new.plot-x-ticks) x-min x-max))
  (define y-ticks (ticks-generate (new.plot-y-ticks) y-min y-max))
  (define z-ticks (ticks-generate (new.plot-z-ticks) z-min z-max))
  (define bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)))
  
  (parameterize ([new.plot-title       title]
                 [new.plot-x-label     x-label]
                 [new.plot-y-label     y-label]
                 [new.plot-z-label     z-label]
                 [new.plot-foreground  fgcolor]
                 [new.plot-background  bgcolor]
                 [new.plot3d-angle     az]
                 [new.plot3d-altitude  alt])
    (define bm (make-bitmap (ceiling width) (ceiling height)))
    (define dc (make-object bitmap-dc% bm))
    (define area (make-object 3d-plot-area%
                   bounds-rect x-ticks x-ticks y-ticks y-ticks z-ticks z-ticks dc 0 0 width height))
    
    (send area start-plot)
    (send area start-renderer bounds-rect)
    (data area)
    (send area end-renderers)
    (send area end-plot)
    
    (when out-file (send bm save-file out-file 'png))
    
    (make-object image-snip% bm)))

;; ===================================================================================================
;; Functions that generate "plot data"

(define (points vecs #:sym [sym 'square] #:color [color 'black])
  (renderer2d->plot-data (new.points (map (λ (v) (vector-take v 2)) vecs)
                                     #:sym sym #:size 6 #:color color)))

(define (vector-field f
                      #:samples [samples 20]
                      #:width [width 1]
                      #:color [color 'red]
                      #:style [style 'scaled])
  (define scale (case style
                  [(scaled)      'auto]
                  [(normalized)  'normalized]
                  [(real)        1.0]))
  (renderer2d->plot-data
   (new.vector-field f #:samples samples #:line-width width #:color color #:scale scale)))

(define (error-bars vecs #:color [color 'black])
  (renderer2d->plot-data (new.error-bars vecs #:color color #:alpha 1 #:width 4)))

(define (line f
              #:samples [samples 150]
              #:width [width 1]
              #:color [color 'red]
              #:mode [mode 'standard]
              #:mapping [mapping 'cartesian]
              #:t-min [t-min -5]
              #:t-max [t-max 5])
  (renderer2d->plot-data (line-renderer f samples width color mode mapping t-min t-max)))

(define (contour f
                 #:samples [samples 50]
                 #:width [width 1]
                 #:color [color 'black]
                 #:levels [levels 10])
  (renderer2d->plot-data (contour-renderer f samples width color levels)))

(define (shade f #:samples [samples 50] #:levels [levels 10])
  (renderer2d->plot-data (shade-renderer f samples levels)))

(define (surface f #:samples [samples 50] #:width [width 1] #:color [color 'black])
  (renderer3d->plot-data (surface-renderer f samples width color)))
