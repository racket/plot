#lang typed/racket/base

(require typed/racket/draw typed/racket/class
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
         "../plot2d/plot-area.rkt"
         "../plot2d/renderer.rkt"
         "plot2d-utils.rkt"
         "evil.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Plot to a given device context

(:: plot/dc
    (->* [(Treeof (U renderer2d nonrenderer))
          (Instance DC<%>)
          Real Real Nonnegative-Real Nonnegative-Real]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:title (U String #f)
          #:x-label (U String #f)
          #:y-label (U String #f)
          #:legend-anchor Anchor]
         Void))
(define (plot/dc renderer-tree dc x y width height
                 #:x-min [x-min #f] #:x-max [x-max #f]
                 #:y-min [y-min #f] #:y-max [y-max #f]
                 #:title [title (plot-title)]
                 #:x-label [x-label (plot-x-label)]
                 #:y-label [y-label (plot-y-label)]
                 #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define fail/pos (make-raise-argument-error 'plot/dc renderer-tree dc x y width height))
  (define fail/kw (make-raise-keyword-error 'plot/dc))
  (cond
    [(not (rational? x))  (fail/pos "rational?" 2)]
    [(not (rational? y))  (fail/pos "rational?" 3)]
    [(not (rational? width))   (fail/pos "rational?" 4)]
    [(not (rational? height))  (fail/pos "rational?" 5)]
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [else
     (define renderer-list (get-renderer-list renderer-tree))
     (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max))
     (define-values (x-ticks x-far-ticks y-ticks y-far-ticks)
       (get-ticks renderer-list bounds-rect))
     
     (parameterize ([plot-title          title]
                    [plot-x-label        x-label]
                    [plot-y-label        y-label]
                    [plot-legend-anchor  legend-anchor])
       (define area (make-object 2d-plot-area%
                      bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks dc x y width height))
       (plot-area area renderer-list))]))

(require (for-syntax racket/base
                     "plot2d-evil-box.rkt"))

(begin-for-syntax
  (set-box! plot/dc-box #'plot/dc))

;; ===================================================================================================
;; Plot to a bitmap

(:: plot-bitmap
    (->* [(Treeof (U renderer2d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:title (U String #f)
          #:x-label (U String #f)
          #:y-label (U String #f)
          #:legend-anchor Anchor]
         (Instance Bitmap%)))
(define (plot-bitmap renderer-tree
                     #:x-min [x-min #f] #:x-max [x-max #f]
                     #:y-min [y-min #f] #:y-max [y-max #f]
                     #:width [width (plot-width)]
                     #:height [height (plot-height)]
                     #:title [title (plot-title)]
                     #:x-label [x-label (plot-x-label)]
                     #:y-label [y-label (plot-y-label)]
                     #:legend-anchor [legend-anchor (plot-legend-anchor)])
  ((if (plot-animating?) draw-bitmap draw-bitmap/supersampling)
   (λ (dc) 
     (plot/dc renderer-tree dc 0 0 width height
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
   width height))

;; ===================================================================================================
;; Plot to a pict

(:: plot-pict
    (->* [(Treeof (U renderer2d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:title (U String #f)
          #:x-label (U String #f)
          #:y-label (U String #f)
          #:legend-anchor Anchor]
         Pict))
(define (plot-pict renderer-tree
                   #:x-min [x-min #f] #:x-max [x-max #f]
                   #:y-min [y-min #f] #:y-max [y-max #f]
                   #:width [width (plot-width)]
                   #:height [height (plot-height)]
                   #:title [title (plot-title)]
                   #:x-label [x-label (plot-x-label)]
                   #:y-label [y-label (plot-y-label)]
                   #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define saved-values (plot-parameters))
  (dc (λ (dc x y)
        (parameterize/group ([plot-parameters  saved-values])
          (plot/dc renderer-tree dc x y width height
                   #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                   #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)))
      width height))

;; ===================================================================================================
;; Plot to a file

(:: plot-file
    (->* [(Treeof (U renderer2d nonrenderer))
          (U Path-String Output-Port)]
         [(U 'auto Image-File-Format)
          #:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:title (U String #f)
          #:x-label (U String #f)
          #:y-label (U String #f)
          #:legend-anchor Anchor]
         Void))
(define (plot-file renderer-tree output [kind 'auto]
                   #:x-min [x-min #f] #:x-max [x-max #f]
                   #:y-min [y-min #f] #:y-max [y-max #f]
                   #:width [width (plot-width)]
                   #:height [height (plot-height)]
                   #:title [title (plot-title)]
                   #:x-label [x-label (plot-x-label)]
                   #:y-label [y-label (plot-y-label)]
                   #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define real-kind
    (cond [(eq? kind 'auto)
           (cond [(port? output)  (error 'plot-file "can't detect file type from output stream")]
                 [else  (detect-image-file-type output)])]
          [else kind]))
  (case real-kind
    [(png jpeg xbm xpm bmp)
     (define bm
       (plot-bitmap
        renderer-tree
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
        #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
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
     (plot/dc renderer-tree dc 0 0
              (/ width (inexact->exact x-scale)) (/ height (inexact->exact y-scale))
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)
     (send dc end-page)
     (send dc end-doc)])
  (void))
