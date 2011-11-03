#lang racket/base

;; Parameters that control the look and behavior of plots.

(require racket/contract unstable/parameter-group unstable/latent-contract
         "contract.rkt"
         "contract-doc.rkt"
         "draw.rkt"
         "axis-transform.rkt"
         "ticks.rkt")

(provide (all-defined-out))

(defparam plot-deprecation-warnings? boolean? #f)

;; ===================================================================================================
;; General plot parameters

;; General appearance

(defparam plot-width exact-positive-integer? 400)
(defparam plot-height exact-positive-integer? 400)
(defparam plot-foreground color plot-color/c 0)
(defparam plot-background color plot-color/c 0)
(defparam plot-foreground-alpha alpha (real-in 0 1) 1)
(defparam plot-background-alpha alpha (real-in 0 1) 1)
(defparam plot-line-width width (>=/c 0) 1)
(defparam plot-tick-size (>=/c 0) 10)
(defparam plot-font-size size (>=/c 0) 11)
(defparam plot-font-family family font-family/c 'roman)
(defparam plot-legend-anchor anchor anchor/c 'top-left)
(defparam plot-legend-box-alpha alpha (real-in 0 1) 2/3)
(defparam plot-animating? boolean? #f)

(defparam plot-x-axis? boolean? #t)
(defparam plot-y-axis? boolean? #t)
(defparam plot-z-axis? boolean? #t)

(defparam plot-x-far-axis? boolean? #t)
(defparam plot-y-far-axis? boolean? #t)
(defparam plot-z-far-axis? boolean? #t)

(defparam plot-x-max-ticks exact-positive-integer? 5)
(defparam plot-y-max-ticks exact-positive-integer? 5)
(defparam plot-z-max-ticks exact-positive-integer? 8)
(defparam plot-d-max-ticks exact-positive-integer? 6)
(defparam plot-r-max-ticks exact-positive-integer? 8)

(defparam plot-x-far-max-ticks exact-positive-integer? 5)
(defparam plot-y-far-max-ticks exact-positive-integer? 5)
(defparam plot-z-far-max-ticks exact-positive-integer? 8)

(defparam plot-decorations? boolean? #t)

(define-parameter-group plot-axes?
  (plot-x-axis? plot-x-far-axis?
   plot-y-axis? plot-y-far-axis?
   plot-z-axis? plot-z-far-axis?)
  #:struct list)

(define-parameter-group plot-max-ticks
  (plot-x-max-ticks plot-x-far-max-ticks
   plot-y-max-ticks plot-y-far-max-ticks
   plot-z-max-ticks plot-z-far-max-ticks
   plot-d-max-ticks
   plot-r-max-ticks)
  #:struct list)

(define-parameter-group plot-appearance
   (plot-width
    plot-height
    plot-foreground plot-foreground-alpha
    plot-background plot-background-alpha
    plot-line-width plot-tick-size
    plot-font-size plot-font-family
    plot-legend-anchor plot-legend-box-alpha
    plot-axes? plot-max-ticks plot-decorations?
    plot-animating?))

(defproc (pen-gap) real? #:document-body
  (* 2 (plot-line-width)))

(defproc (animated-samples [samples (and/c exact-integer? (>=/c 2))]
                           ) (and/c exact-integer? (>=/c 2)) #:document-body
  (cond [(plot-animating?)  (max 2 (ceiling (* 1/4 samples)))]
        [else  samples]))

;; 3D-specific appearance

(defparam plot3d-samples (and/c exact-integer? (>=/c 2)) 41)
(defparam plot3d-angle real? 30)
(defparam plot3d-altitude real? 60)
(defparam plot3d-ambient-light (real-in 0 1) 2/3)
(defparam plot3d-diffuse-light? boolean? #t)
(defparam plot3d-specular-light? boolean? #t)

(define-parameter-group plot3d-appearance
  (plot3d-samples
   plot3d-angle
   plot3d-altitude
   plot3d-ambient-light
   plot3d-diffuse-light?
   plot3d-specular-light?))

;; Output

(defparam plot-new-window? boolean? #f)
(defparam plot-jpeg-quality (integer-in 0 100) 100)
(defparam plot-ps/pdf-interactive? boolean? #f)

(define-parameter-group plot-output (plot-new-window? plot-jpeg-quality plot-ps/pdf-interactive?))

;; Labels

(defparam plot-title (or/c string? #f) #f)
(defparam plot-x-label (or/c string? #f) "x axis")
(defparam plot-y-label (or/c string? #f) "y axis")
(defparam plot-z-label (or/c string? #f) #f)

(defparam plot-x-far-label (or/c string? #f) #f)
(defparam plot-y-far-label (or/c string? #f) #f)
(defparam plot-z-far-label (or/c string? #f) #f)

(define-parameter-group plot-labels
  (plot-title
   plot-x-label plot-y-label plot-z-label
   plot-x-far-label plot-y-far-label plot-z-far-label))

;; Axes: transform, ticks

(defparam plot-x-transform axis-transform/c id-transform)
(defparam plot-y-transform axis-transform/c id-transform)
(defparam plot-z-transform axis-transform/c id-transform)

(defparam plot-x-ticks ticks? (linear-ticks))
(defparam plot-y-ticks ticks? (linear-ticks))
(defparam plot-z-ticks ticks? (linear-ticks))
(defparam plot-d-ticks ticks? (linear-ticks #:divisors '(1 2 4 5)))
(defparam plot-r-ticks ticks? (linear-ticks))

(defparam plot-x-far-ticks ticks? (ticks-mimic plot-x-ticks))
(defparam plot-y-far-ticks ticks? (ticks-mimic plot-y-ticks))
(defparam plot-z-far-ticks ticks? (ticks-mimic plot-z-ticks))

(struct axis (transform ticks far-ticks) #:transparent)
(define-parameter-group plot-x-axis (plot-x-transform plot-x-ticks plot-x-far-ticks) #:struct axis)
(define-parameter-group plot-y-axis (plot-y-transform plot-y-ticks plot-y-far-ticks) #:struct axis)
(define-parameter-group plot-z-axis (plot-z-transform plot-z-ticks plot-z-far-ticks) #:struct axis)

(define-parameter-group plot-axes (plot-x-axis plot-y-axis plot-z-axis plot-d-ticks plot-r-ticks)
  #:struct list)

(defproc (default-x-ticks [x-min real?] [x-max real?]) (listof tick?) #:document-body
  ((plot-x-ticks) x-min x-max (plot-x-max-ticks)))

(defproc (default-y-ticks [y-min real?] [y-max real?]) (listof tick?) #:document-body
  ((plot-y-ticks) y-min y-max (plot-y-max-ticks)))

(defproc (default-z-ticks [z-min real?] [z-max real?]) (listof tick?) #:document-body
  ((plot-z-ticks) z-min z-max (plot-z-max-ticks)))

(defproc (default-d-ticks [d-min real?] [d-max real?]) (listof tick?) #:document-body
  ((plot-d-ticks) d-min d-max (plot-d-max-ticks)))

(defproc (default-r-ticks [r-min real?] [r-max real?]) (listof tick?) #:document-body
  ((plot-r-ticks) r-min r-max (plot-r-max-ticks)))

(defproc (default-x-far-ticks [x-min real?] [x-max real?]) (listof tick?) #:document-body
  ((plot-x-far-ticks) x-min x-max (plot-x-far-max-ticks)))

(defproc (default-y-far-ticks [y-min real?] [y-max real?]) (listof tick?) #:document-body
  ((plot-y-far-ticks) y-min y-max (plot-y-far-max-ticks)))

(defproc (default-z-far-ticks [z-min real?] [z-max real?]) (listof tick?) #:document-body
  ((plot-z-far-ticks) z-min z-max (plot-z-far-max-ticks)))

;; ===================================================================================================

(define-parameter-group plot-parameters
  (plot-appearance
   plot3d-appearance
   plot-labels
   plot-output
   plot-axes))

;; ===================================================================================================
;; Renderer-specific parameters

;; Lines

(defparam line-samples (and/c exact-integer? (>=/c 2)) 500)
(defparam line-color plot-color/c 1)
(defparam line-width (>=/c 0) 1)
(defparam line-style plot-pen-style/c 'solid)
(defparam line-alpha (real-in 0 1) 1)

;; Intervals

(defparam interval-color plot-color/c 3)
(defparam interval-style plot-brush-style/c 'solid)
(defparam interval-line1-color plot-color/c 3)
(defparam interval-line1-width (>=/c 0) 1)
(defparam interval-line1-style plot-pen-style/c 'solid)
(defparam interval-line2-color plot-color/c 3)
(defparam interval-line2-width (>=/c 0) 1)
(defparam interval-line2-style plot-pen-style/c 'solid)
(defparam interval-alpha (real-in 0 1) 3/4)

;; Points

(defparam point-sym point-sym/c 'circle)
(defparam point-color plot-color/c 0)
(defparam point-size (>=/c 0) 6)
(defparam point-line-width (>=/c 0) 1)
(defparam point-alpha (real-in 0 1) 1)

;; Vector fields

(defparam vector-field-samples exact-positive-integer? 20)
(defparam vector-field-color plot-color/c 1)
(defparam vector-field-line-width (>=/c 0) 2/3)
(defparam vector-field-line-style plot-pen-style/c 'solid)
(defparam vector-field-scale (or/c real? (one-of/c 'auto 'normalized)) 'auto)
(defparam vector-field-alpha (real-in 0 1) 1)

;; Error bars

(defparam error-bar-width (>=/c 0) 6)
(defparam error-bar-color plot-color/c 0)
(defparam error-bar-line-width (>=/c 0) 1)
(defparam error-bar-line-style plot-pen-style/c 'solid)
(defparam error-bar-alpha (real-in 0 1) 2/3)

;; Contours

(defproc (default-contour-colors [zs (listof real?)]) (listof plot-color/c) #:document-body
  (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
              (length zs)))

(defproc (default-contour-fill-colors [zs (listof real?)]) (listof plot-color/c) #:document-body
  (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
              (sub1 (length zs))))

(defparam contour-samples (and/c exact-integer? (>=/c 2)) 51)
(defparam contour-levels (or/c 'auto exact-positive-integer? (listof real?)) 'auto)
(defparam contour-colors plot-colors/c default-contour-colors)
(defparam contour-widths pen-widths/c '(1))
(defparam contour-styles plot-pen-styles/c '(solid long-dash))
(defparam contour-alphas alphas/c '(1))

(defparam contour-interval-colors plot-colors/c default-contour-fill-colors)
(defparam contour-interval-styles plot-brush-styles/c '(solid))
(defparam contour-interval-alphas alphas/c '(1))

;; Histograms

(defparam rectangle-color plot-color/c 3)
(defparam rectangle-style plot-brush-style/c 'solid)
(defparam rectangle-line-color plot-color/c 3)
(defparam rectangle-line-width (>=/c 0) 1)
(defparam rectangle-line-style plot-pen-style/c 'solid)
(defparam rectangle-alpha (real-in 0 1) 1)
(defparam discrete-histogram-gap (real-in 0 1) 1/8)

;; Decorations

(defparam x-axis-ticks? boolean? #t)
(defparam y-axis-ticks? boolean? #t)
(defparam z-axis-ticks? boolean? #t)

(defparam x-axis-labels? boolean? #f)
(defparam y-axis-labels? boolean? #f)
(defparam z-axis-labels? boolean? #f)

(defparam x-axis-far? boolean? #f)
(defparam y-axis-far? boolean? #f)
(defparam z-axis-far? boolean? #f)

(defparam x-axis-alpha (real-in 0 1) 1)
(defparam y-axis-alpha (real-in 0 1) 1)
(defparam z-axis-alpha (real-in 0 1) 1)

(defparam polar-axes-number exact-nonnegative-integer? 12)
(defparam polar-axes-ticks? boolean? #t)
(defparam polar-axes-labels? boolean? #t)
(defparam polar-axes-alpha (real-in 0 1) 1/2)

(defparam label-anchor anchor/c 'left)
(defparam label-angle real? 0)
(defparam label-alpha (real-in 0 1) 1)
(defparam label-point-size (>=/c 0) 4)

;; Surfaces

(defparam surface-color plot-color/c 0)
(defparam surface-style plot-brush-style/c 'solid)
(defparam surface-line-color plot-color/c 0)
(defparam surface-line-width (>=/c 0) 1/3)
(defparam surface-line-style plot-pen-style/c 'solid)
(defparam surface-alpha (real-in 0 1) 1)

;; Contour surfaces

(defparam contour-interval-line-colors plot-colors/c '(0))
(defparam contour-interval-line-widths pen-widths/c '(1/3))
(defparam contour-interval-line-styles plot-pen-styles/c '(solid))

;; Isosurfaces

(defproc (default-isosurface-colors [zs (listof real?)]) (listof plot-color/c) #:document-body
  (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
              (length zs)))

(defproc (default-isosurface-line-colors [zs (listof real?)]) (listof plot-color/c) #:document-body
  (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
              (length zs)))

(defparam isosurface-levels (or/c 'auto exact-positive-integer? (listof real?)) 'auto)
(defparam isosurface-colors plot-colors/c default-isosurface-colors)
(defparam isosurface-line-colors plot-colors/c default-isosurface-line-colors)
(defparam isosurface-line-widths pen-widths/c '(1/3))
(defparam isosurface-line-styles plot-pen-styles/c '(solid))
(defparam isosurface-alphas alphas/c '(1/2))

;; Histograms

(defparam rectangle3d-line-width (>=/c 0) 1/3)