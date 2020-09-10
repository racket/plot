#lang typed/racket/base

(require typed/racket/draw typed/racket/class racket/match
         (only-in typed/pict pict)
         "../common/type-doc.rkt"
         "../common/types.rkt"
         "../common/draw.rkt"
         "../common/parameters.rkt"
         "../common/parameter-groups.rkt"
         "../common/parameter-group.rkt"
         "../common/plot-element.rkt"
         "../common/nonrenderer.rkt"
         "../common/file-type.rkt"
         "../common/utils.rkt"
         "../common/math.rkt"
         "../common/plot-device.rkt"
         "../plot3d/plot-area.rkt"
         "../plot3d/renderer.rkt"
         "plot3d-utils.rkt"
         "evil.rkt"
         typed/racket/unsafe)



(unsafe-provide plot3d/dc
                plot3d-bitmap
                plot3d-pict legend3d-pict plot+legend3d-picts
                plot3d-file)

;; ===================================================================================================
;; Plot to a given device context

(:: plot3d/dc
    (->* [(Treeof (U renderer3d nonrenderer))
          (Instance DC<%>)
          Real Real Nonnegative-Real Nonnegative-Real]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:angle Real #:altitude Real
          #:title (U String pict #f)
          #:x-label (U String pict #f)
          #:y-label (U String pict #f)
          #:z-label (U String pict #f)
          #:legend-anchor Legend-Anchor]
         Void))
(define (plot3d/dc renderer-tree dc x y width height
                   #:x-min [x-min #f] #:x-max [x-max #f]
                   #:y-min [y-min #f] #:y-max [y-max #f]
                   #:z-min [z-min #f] #:z-max [z-max #f]
                   #:angle [angle (plot3d-angle)]
                   #:altitude [altitude (plot3d-altitude)]
                   #:title [title (plot-title)]
                   #:x-label [x-label (plot-x-label)]
                   #:y-label [y-label (plot-y-label)]
                   #:z-label [z-label (plot-z-label)]
                   #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define fail/pos (make-raise-argument-error 'plot3d/dc renderer-tree dc x y width height))
  (define fail/kw (make-raise-keyword-error 'plot3d/dc))
  (cond
    [(not (rational? x))  (fail/pos "rational?" 2)]
    [(not (rational? y))  (fail/pos "rational?" 3)]
    [(not (rational? width))   (fail/pos "rational?" 4)]
    [(not (rational? height))  (fail/pos "rational?" 5)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(not (rational? angle))     (fail/kw "rational?" '#:angle angle)]
    [(not (rational? altitude))  (fail/kw "rational?" '#:altitude altitude)]
    [else
     (define renderer-list (get-renderer-list renderer-tree))
     (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max z-min z-max))
     (define-values (x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks)
       (get-ticks renderer-list bounds-rect))
     (define legend-list (get-legend-entry-list renderer-list bounds-rect))
     
     (parameterize ([plot3d-angle        angle]
                    [plot3d-altitude     altitude]
                    [plot-title          title]
                    [plot-x-label        x-label]
                    [plot-y-label        y-label]
                    [plot-z-label        z-label]
                    [plot-legend-anchor  legend-anchor])
       (define area (make-object 3d-plot-area%
                      bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
                      legend-list
                      dc x y width height))
       (plot-area area renderer-list))]))

(require (for-syntax racket/base
                     "plot3d-evil-box.rkt"))

(begin-for-syntax
  (set-box! plot3d/dc-box #'plot3d/dc))

;; ===================================================================================================
;; Plot to a bitmap

(:: plot3d-bitmap
    (->* [(Treeof (U renderer3d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:angle Real #:altitude Real
          #:title (U String pict #f)
          #:x-label (U String pict #f)
          #:y-label (U String pict #f)
          #:z-label (U String pict #f)
          #:legend-anchor Legend-Anchor]
         (Instance Bitmap%)))
(define (plot3d-bitmap renderer-tree 
                       #:x-min [x-min #f] #:x-max [x-max #f]
                       #:y-min [y-min #f] #:y-max [y-max #f]
                       #:z-min [z-min #f] #:z-max [z-max #f]
                       #:width [width (plot-width)]
                       #:height [height (plot-height)]
                       #:angle [angle (plot3d-angle)]
                       #:altitude [altitude (plot3d-altitude)]
                       #:title [title (plot-title)]
                       #:x-label [x-label (plot-x-label)]
                       #:y-label [y-label (plot-y-label)]
                       #:z-label [z-label (plot-z-label)]
                       #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (plot3d/dc renderer-tree dc 0 0 width height
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
             #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
             #:z-label z-label #:legend-anchor legend-anchor)
  bm)

;; ===================================================================================================
;; Plot to a pict

(:: plot3d-pict
    (->* [(Treeof (U renderer3d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:angle Real #:altitude Real
          #:title (U String pict #f)
          #:x-label (U String pict #f)
          #:y-label (U String pict #f)
          #:z-label (U String pict #f)
          #:legend-anchor Legend-Anchor]
         Pict))
(define (plot3d-pict renderer-tree 
                     #:x-min [x-min #f] #:x-max [x-max #f]
                     #:y-min [y-min #f] #:y-max [y-max #f]
                     #:z-min [z-min #f] #:z-max [z-max #f]
                     #:width [width (plot-width)]
                     #:height [height (plot-height)]
                     #:angle [angle (plot3d-angle)]
                     #:altitude [altitude (plot3d-altitude)]
                     #:title [title (plot-title)]
                     #:x-label [x-label (plot-x-label)]
                     #:y-label [y-label (plot-y-label)]
                     #:z-label [z-label (plot-z-label)]
                     #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define saved-plot-parameters (plot-parameters))
  (dc (λ (dc x y)
        (parameterize/group ([plot-parameters  saved-plot-parameters])
          (plot3d/dc renderer-tree dc x y width height
                     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min
                     #:z-max z-max #:angle angle #:altitude altitude #:title title #:x-label x-label
                     #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor)))
      width height))

(:: legend3d-pict
    (->* [(Treeof (U renderer3d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)]
         Pict))
(define (legend3d-pict renderer-tree
                     #:x-min [x-min #f] #:x-max [x-max #f]
                     #:y-min [y-min #f] #:y-max [y-max #f]
                     #:z-min [z-min #f] #:z-max [z-max #f])
  (define fail/kw (make-raise-keyword-error 'plot/dc))
  (cond
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [else
     (define saved-values (plot-parameters))
  
     (define renderer-list (get-renderer-list renderer-tree))
     (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max z-min z-max))
     (define legend (get-legend-entry-list renderer-list bounds-rect))
     
     (define-values (width height)
       (parameterize/group ([plot-parameters saved-values])
        (define width : Nonnegative-Real 0)
        (define height : Nonnegative-Real 0)
        (dc (λ (dc x y)
              (define dummy (make-object plot-device% dc x y 1 1))
              (send dummy reset-drawing-params)
              (match-define (vector (ivl x- x+) (ivl y- y+))
                (send dummy calculate-legend-rect legend (vector (ivl x 1) (ivl y 1)) 'top-left))
              (send dummy restore-drawing-params)
              (when (and x- x+) (let ([w (- x+ x-)]) (when (< 0 w) (set! width w))))
              (when (and y- y+) (let ([h (- y+ y-)]) (when (< 0 h) (set! height h)))))
            1 1)
        (values width height)))
     
     (dc (λ (dc x y)
           (parameterize/group ([plot-parameters  saved-values])
             (define pd (make-object plot-device% dc x y width height))
             (send pd reset-drawing-params)
             (send pd clear)
             (send pd draw-legend legend (vector (ivl x (+ x width)) (ivl y (+ y height))))
             (send pd restore-drawing-params)))
         width height)]))

(:: plot+legend3d-picts
    (->* [(Treeof (U renderer3d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:title (U String pict #f)
          #:x-label (U String pict #f)
          #:y-label (U String pict #f)]
         (Values Pict Pict)))
(define (plot+legend3d-picts renderer-tree
                           #:x-min [x-min #f] #:x-max [x-max #f]
                           #:y-min [y-min #f] #:y-max [y-max #f]
                           #:width [width (plot-width)]
                           #:height [height (plot-height)]
                           #:title [title (plot-title)]
                           #:x-label [x-label (plot-x-label)]
                           #:y-label [y-label (plot-y-label)])
  (define saved-values (plot-parameters))
  (values
   (dc (λ (dc x y)
         (parameterize/group ([plot-parameters  saved-values])
           (plot3d/dc renderer-tree dc x y width height
                      #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                      #:title title #:x-label x-label #:y-label y-label #:legend-anchor #f)))
       width height)
   (legend3d-pict renderer-tree #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max)))

;; ===================================================================================================
;; Plot to any supported kind of file

(:: plot3d-file
    (->* [(Treeof (U renderer3d nonrenderer))
          (U Path-String Output-Port)]
         [(U 'auto Image-File-Format)
          #:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:angle Real #:altitude Real
          #:title (U String pict #f)
          #:x-label (U String pict #f)
          #:y-label (U String pict #f)
          #:z-label (U String pict #f)
          #:legend-anchor Legend-Anchor]
         Void))
(define (plot3d-file renderer-tree output [kind 'auto]
                     #:x-min [x-min #f] #:x-max [x-max #f]
                     #:y-min [y-min #f] #:y-max [y-max #f]
                     #:z-min [z-min #f] #:z-max [z-max #f]
                     #:width [width (plot-width)]
                     #:height [height (plot-height)]
                     #:angle [angle (plot3d-angle)]
                     #:altitude [altitude (plot3d-altitude)]
                     #:title [title (plot-title)]
                     #:x-label [x-label (plot-x-label)]
                     #:y-label [y-label (plot-y-label)]
                     #:z-label [z-label (plot-z-label)]
                     #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define real-kind
    (cond [(eq? kind 'auto)
           (cond [(port? output)  (error 'plot3d-file "can't detect file type from output stream")]
                 [else  (detect-image-file-type output)])]
          [else  kind]))
  (case real-kind
    [(png jpeg xbm xpm bmp)
     (define bm
       (plot3d-bitmap
        renderer-tree
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
        #:width width #:height height #:angle angle #:altitude altitude #:title title
        #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
     (send bm save-file output real-kind (plot-jpeg-quality))]
    [(ps pdf svg)
     (define dc
       (case real-kind
         [(ps)   (new-post-script-dc% width height output)]
         [(pdf)  (new-pdf-dc% width height output)]
         [(svg)  (new-svg-dc% width height output)]))
     (define-values (x-scale y-scale) (send dc get-device-scale))
     (send dc start-doc "Rendering plot")
     (send dc start-page)
     (plot3d/dc renderer-tree dc 0 0
                (/ width (inexact->exact x-scale)) (/ height (inexact->exact y-scale))
                #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
                #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
                #:z-label z-label #:legend-anchor legend-anchor)
     (send dc end-page)
     (send dc end-doc)])
  (void))
