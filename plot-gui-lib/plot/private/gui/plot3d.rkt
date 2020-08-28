#lang typed/racket/base #:no-optimize

;; See note at the top of plot2d.rkt for the use of `#:no-optimize` and
;; `unsafe-provide`

(require (only-in typed/mred/mred Snip% Frame%)
         (only-in racket/gui/base get-display-backing-scale)
         typed/racket/draw typed/racket/class racket/match racket/list
         (only-in typed/pict pict pict?)
         plot/utils
         plot/private/common/parameter-group
         plot/private/common/draw
         plot/private/common/deprecation-warning
         plot/private/common/type-doc
         plot/private/common/utils
         plot/private/plot3d/plot-area
         plot/private/no-gui/plot3d
         plot/private/no-gui/plot3d-utils
         plot/private/no-gui/utils
         "lazy-snip-typed.rkt"
         typed/racket/unsafe)

(unsafe-provide plot3d-snip
                plot3d-frame
                plot3d)

(require/typed plot/utils
  (anchor/c (-> Any Boolean))
  (plot-color/c (-> Any Boolean))
  (plot-file-format/c (-> Any Boolean)))

;; ===================================================================================================
;; Plot to a snip

(:: plot3d-snip
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
          #:legend-anchor Anchor]
         (Instance Snip%)))
(define (plot3d-snip renderer-tree
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
  (define fail/kw (make-raise-keyword-error 'plot3d-snip))
  (cond
    ;; check arguments, see note at the top of this file
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(not (real? angle)) (fail/kw "real" '#:angle angle)]
    [(not (real? altitude)) (fail/kw "real" '#:altitude altitude)]
    [(not (and (integer? width) (positive? width))) (fail/kw "positive integer" '#:width width)]
    [(not (and (integer? height) (positive? height))) (fail/kw "positive integer" '#:height height)]
    [(and title (not (or (string? title) (pict? title)))) (fail/kw "#f, string or pict" '#:title title)]
    [(and x-label (not (or (string? x-label) (pict? x-label)))) (fail/kw "#f, string or pict" '#:x-label x-label)]
    [(and y-label (not (or (string? y-label) (pict? y-label)))) (fail/kw "#f, string or pict" '#:y-label y-label)]
    [(and z-label (not (or (string? z-label) (pict? z-label)))) (fail/kw "#f, string or pict" '#:y-label z-label)]
    [(not (anchor/c legend-anchor)) (fail/kw "anchor/c" '#:legend-anchor legend-anchor)])

  (parameterize ([plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-z-label        z-label]
                 [plot-legend-anchor  legend-anchor])
    (define saved-plot-parameters (plot-parameters))
    (define renderer-list (get-renderer-list renderer-tree))
    (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max z-min z-max))
    (define-values (x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks)
      (get-ticks renderer-list bounds-rect))

    (define render-tasks-hash ((inst make-hash Boolean render-tasks)))
    (define legend-entries-hash ((inst make-hash Boolean (Listof legend-entry))))

    (: make-bm (-> Boolean Real Real Positive-Integer Positive-Integer (Instance Bitmap%)))
    (define (make-bm anim? angle altitude width height)
      (parameterize/group ([plot-parameters  saved-plot-parameters]
                           [plot-animating?  (if anim? #t (plot-animating?))]
                           [plot3d-angle     angle]
                           [plot3d-altitude  altitude])
        (define bm (make-bitmap
                     width height #t
                     #:backing-scale (or (get-display-backing-scale) 1.0)))
        (define dc (make-object bitmap-dc% bm))
        (define area (make-object 3d-plot-area%
                                  bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
                                  dc 0 0 width height))
        (send area start-plot)

        (cond [(not (hash-ref render-tasks-hash (plot-animating?) #f))
               (hash-set!
                legend-entries-hash (plot-animating?)
                (flatten-legend-entries
                 (for/list : (Listof (Treeof legend-entry)) ([rend  (in-list renderer-list)])
                   (match-define (renderer3d rend-bounds-rect _bf _tf render-proc) rend)
                   (send area start-renderer (if rend-bounds-rect
                                                 (rect-inexact->exact rend-bounds-rect)
                                                 (unknown-rect 3)))
                   (if render-proc (render-proc area) empty))))

               (hash-set! render-tasks-hash (plot-animating?) (send area get-render-tasks))]
              [else
               (send area set-render-tasks (hash-ref render-tasks-hash (plot-animating?)))])

        (send area end-renderers)

        (define legend-entries (hash-ref legend-entries-hash (plot-animating?) #f))
        (when (and legend-entries (not (empty? legend-entries)))
          (send area draw-legend legend-entries))

        (send area end-plot)
        bm))

    (make-3d-plot-snip
     (make-bm #f angle altitude width height) saved-plot-parameters
     make-bm angle altitude width height)))

;; ===================================================================================================
;; Plot to a frame

(:: plot3d-frame
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
          #:legend-anchor Anchor]
         (Instance Frame%)))
(define (plot3d-frame renderer-tree
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
  (define fail/kw (make-raise-keyword-error 'plot3d-frame))
  (cond
    ;; check arguments, see note at the top of this file
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(not (real? angle)) (fail/kw "real" '#:angle angle)]
    [(not (real? altitude)) (fail/kw "real" '#:altitude altitude)]
    [(not (and (integer? width) (positive? width))) (fail/kw "positive integer" '#:width width)]
    [(not (and (integer? height) (positive? height))) (fail/kw "positive integer" '#:height height)]
    [(and title (not (or (string? title) (pict? title)))) (fail/kw "#f, string or pict" '#:title title)]
    [(and x-label (not (or (string? x-label) (pict? x-label)))) (fail/kw "#f, string or pict" '#:x-label x-label)]
    [(and y-label (not (or (string? y-label) (pict? y-label)))) (fail/kw "#f, string or pict" '#:y-label y-label)]
    [(and z-label (not (or (string? z-label) (pict? z-label)))) (fail/kw "#f, string or pict" '#:y-label z-label)]
    [(not (anchor/c legend-anchor)) (fail/kw "anchor/c" '#:legend-anchor legend-anchor)])

  ;; make-snip will be called in a separate thread, make sure the
  ;; parameters have the correct values in that thread as well.
  (define saved-plot-parameters (plot-parameters))
  (: make-snip (-> Positive-Integer Positive-Integer (Instance Snip%)))
  (define (make-snip width height)
    (parameterize/group ([plot-parameters  saved-plot-parameters])
      (plot3d-snip
       renderer-tree
       #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
       #:width width #:height height #:angle angle #:altitude altitude #:title title
       #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor)))
  (make-snip-frame make-snip width height (if title (format "Plot: ~a" title) "Plot")))

;; ===================================================================================================
;; Plot to a frame or a snip, depending on the value of plot-new-window?

(:: plot3d
    (->* [(Treeof (U renderer3d nonrenderer))]
         [#:x-min (U Real #f) #:x-max (U Real #f)
          #:y-min (U Real #f) #:y-max (U Real #f)
          #:z-min (U Real #f) #:z-max (U Real #f)
          #:width Positive-Integer
          #:height Positive-Integer
          #:angle Real #:altitude Real
          #:az Real #:alt Real
          #:title (U String pict #f)
          #:x-label (U String pict #f)
          #:y-label (U String pict #f)
          #:z-label (U String pict #f)
          #:legend-anchor Anchor
          #:out-file (U Path-String Output-Port #f)
          #:out-kind (U 'auto Image-File-Format)
          #:fgcolor Plot-Color
          #:bgcolor Plot-Color
          #:lncolor Plot-Color]
         (U (Instance Snip%) Void)))
(define (plot3d renderer-tree
                #:x-min [x-min #f] #:x-max [x-max #f]
                #:y-min [y-min #f] #:y-max [y-max #f]
                #:z-min [z-min #f] #:z-max [z-max #f]
                #:width [width (plot-width)]
                #:height [height (plot-height)]
                #:angle [angle #f]
                #:altitude [altitude #f]
                #:az [az #f] #:alt [alt #f]  ; backward-compatible aliases
                #:title [title (plot-title)]
                #:x-label [x-label (plot-x-label)]
                #:y-label [y-label (plot-y-label)]
                #:z-label [z-label (plot-z-label)]
                #:legend-anchor [legend-anchor (plot-legend-anchor)]
                #:out-file [out-file #f]
                #:out-kind [out-kind 'auto]
                #:fgcolor [fgcolor #f]
                #:bgcolor [bgcolor #f]
                #:lncolor [lncolor #f])  ; unused
  (when fgcolor
    (deprecation-warning "the plot3d #:fgcolor keyword argument" "plot-foreground"))
  (when bgcolor
    (deprecation-warning "the plot3d #:bgcolor keyword argument" "plot-background"))
  (when lncolor
    (deprecation-warning "the plot3d #:lncolor keyword argument"))
  (when az
    (deprecation-warning "the plot3d #:az keyword argument" "#:angle"))
  (when alt
    (deprecation-warning "the plot3d #:alt keyword argument" "#:altitude"))

  (define fail/kw (make-raise-keyword-error 'plot3d))
  (cond
    ;; check arguments, see note at the top of this file
    [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
    [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
    [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
    [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
    [(and z-min (not (rational? z-min)))  (fail/kw "#f or rational" '#:z-min z-min)]
    [(and z-max (not (rational? z-max)))  (fail/kw "#f or rational" '#:z-max z-max)]
    [(and angle (not (real? angle))) (fail/kw "#f or real" '#:angle angle)]
    [(and altitude (not (real? altitude))) (fail/kw "#f or real" '#:altitude altitude)]
    [(and az (not (real? az))) (fail/kw "#f or real" '#:az az)]
    [(and alt (not (real? alt))) (fail/kw "#f or real" '#:alt alt)]
    [(not (and (integer? width) (positive? width))) (fail/kw "positive integer" '#:width width)]
    [(not (and (integer? height) (positive? height))) (fail/kw "positive integer" '#:height height)]
    [(and title (not (or (string? title) (pict? title)))) (fail/kw "#f, string or pict" '#:title title)]
    [(and x-label (not (or (string? x-label) (pict? x-label)))) (fail/kw "#f, string or pict" '#:x-label x-label)]
    [(and y-label (not (or (string? y-label) (pict? y-label)))) (fail/kw "#f, string or pict" '#:y-label y-label)]
    [(and z-label (not (or (string? z-label) (pict? z-label)))) (fail/kw "#f, string or pict" '#:y-label z-label)]
    [(not (anchor/c legend-anchor)) (fail/kw "anchor/c" '#:legend-anchor legend-anchor)]
    [(and out-kind (not (plot-file-format/c out-kind))) (fail/kw "plot-file-format/c" '#:out-kind out-kind)]
    [(not (plot-file-format/c out-kind)) (fail/kw "plot-file-format/c" '#:out-kind out-kind)]
    [(and fgcolor (not (plot-color/c fgcolor))) (fail/kw "plot-color/c" '#:fgcolor fgcolor)]
    [(and bgcolor (not (plot-color/c bgcolor))) (fail/kw "plot-color/c" '#:bgcolor bgcolor)]
    ;; NOTE: don't check this one, as it is not used anyway
    ;; [(and lncolor (not (plot-color/c lncolor))) (fail/kw "plot-color/c" '#:lncolor lncolor)]
    [(and out-file (not (or (path-string? out-file) (output-port? out-file))))
     (fail/kw "#f, path-string or output port" '#:out-file out-file)])

  (cond ;; because this function is exported via `unsafe-provide`
    [(and out-kind (not (plot-file-format/c out-kind)))
     (fail/kw "plot-file-format/c" '#:out-kind out-kind)]
    [(and fgcolor (not (plot-color/c fgcolor)))
     (fail/kw "plot-color/c" '#:fgcolor fgcolor)]
    [(and bgcolor (not (plot-color/c bgcolor)))
     (fail/kw "plot-color/c" '#:bgcolor bgcolor)])

  (parameterize ([plot-foreground  (if fgcolor fgcolor (plot-foreground))]
                 [plot-background  (if bgcolor bgcolor (plot-background))])
    (when out-file
      (plot3d-file
       renderer-tree out-file out-kind
       #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
       #:width width #:height height #:title title
       #:angle (or angle az (plot3d-angle)) #:altitude (or altitude alt (plot3d-altitude))
       #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))

    (cond [(plot-new-window?)
           (define frame
             (plot3d-frame
              renderer-tree
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
              #:width width #:height height #:title title
              #:angle (or angle az (plot3d-angle)) #:altitude (or altitude alt (plot3d-altitude))
              #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
           (send frame show #t)
           (void)]
          [else
           (plot3d-snip
            renderer-tree
            #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
            #:width width #:height height #:title title
            #:angle (or angle az (plot3d-angle)) #:altitude (or altitude alt (plot3d-altitude))
            #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor)])))
