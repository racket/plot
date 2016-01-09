#lang typed/racket/base

;; Parameters that control the look and behavior of plots.

(require typed/racket/class
         typed/racket/draw
         "type-doc.rkt"
         "types.rkt"
         "draw-attribs.rkt"
         "axis-transform.rkt"
         "ticks.rkt"
         "math.rkt")

(provide (except-out (all-defined-out)
                     unit-ivl
                     nonnegative-rational
                     integer>=1
                     integer>=2
                     integer-in
                     rational))

(defparam plot-deprecation-warnings? Boolean #f)

;; ===================================================================================================
;; General plot parameters

;; General appearance

(: unit-ivl (-> Symbol (-> Real Nonnegative-Real)))
(define ((unit-ivl name) x)
  (if (and (<= 0 x) (<= x 1))
      x
      (raise-argument-error name "Real in [0,1]" x)))

(: nonnegative-rational (-> Symbol (-> Real Nonnegative-Real)))
(define ((nonnegative-rational name) x)
  (if (or (negative? x) (not (rational? x)))
      (raise-argument-error name "rational Nonnegative-Real" x)
      x))

(: natural (-> Symbol (-> Integer Natural)))
(define ((natural name) n)
  (if (negative? n) (raise-argument-error name "Natural" n) n))

(: integer>=1 (-> Symbol (-> Integer Positive-Integer)))
(define ((integer>=1 name) n)
  (if (>= n 1) n (raise-argument-error name "Integer >= 1" n)))

(: integer>=2 (-> Symbol (-> Integer Positive-Integer)))
(define ((integer>=2 name) n)
  (if (>= n 2) n (raise-argument-error name "Integer >= 2" n)))

(define-syntax-rule (integer-in name mn mx)
  (λ ([x : Integer])
    (if (and (<= mn x) (<= x mx))
        x
        (raise-argument-error name (format "Integer in [~a,~a]" mn mx) x))))

(: rational (-> Symbol (-> Real Real)))
(define ((rational name) x)
  (if (rational? x) x (raise-argument-error name "rational Real" x)))

(defparam2 plot-width Integer Positive-Integer 400 (integer>=1 'plot-width))
(defparam2 plot-height Integer Positive-Integer 400 (integer>=1 'plot-height))
(defparam plot-foreground color Plot-Color 0)
(defparam plot-background color Plot-Color 0)
(defparam2 plot-foreground-alpha alpha Real Nonnegative-Real 1 (unit-ivl 'plot-foreground-alpha))
(defparam2 plot-background-alpha alpha Real Nonnegative-Real 1 (unit-ivl 'plot-background-alpha))
(defparam2 plot-line-width width Real Nonnegative-Real 1 (nonnegative-rational 'plot-line-width))
(defparam2 plot-tick-size size Real Nonnegative-Real 10 (nonnegative-rational 'plot-tick-size))
(defparam2 plot-font-size size Real Nonnegative-Real 11 (nonnegative-rational 'plot-font-size))
(defparam plot-font-face face (U String #f) #f)
(defparam plot-font-family family Font-Family 'roman)
(defparam plot-legend-anchor anchor Anchor 'top-left)
(defparam2 plot-legend-box-alpha alpha Real Nonnegative-Real 2/3 (unit-ivl 'plot-legend-box-alpha))
(defparam plot-animating? Boolean #f)

(defparam plot-x-axis? Boolean #t)
(defparam plot-y-axis? Boolean #t)
(defparam plot-z-axis? Boolean #t)

(defparam plot-x-far-axis? Boolean #t)
(defparam plot-y-far-axis? Boolean #t)
(defparam plot-z-far-axis? Boolean #t)

(defparam2 plot-x-tick-label-angle angle Real Real 0 (rational 'plot-x-tick-label-angle))
(defparam2 plot-y-tick-label-angle angle Real Real 0 (rational 'plot-y-tick-label-angle))
(defparam2 plot-x-far-tick-label-angle angle Real Real 0 (rational 'plot-x-far-tick-label-angle))
(defparam2 plot-y-far-tick-label-angle angle Real Real 0 (rational 'plot-y-far-tick-label-angle))

(defparam plot-x-tick-label-anchor anchor Anchor 'top)
(defparam plot-y-tick-label-anchor anchor Anchor 'right)
(defparam plot-x-far-tick-label-anchor anchor Anchor 'bottom)
(defparam plot-y-far-tick-label-anchor anchor Anchor 'left)

(defparam plot-decorations? Boolean #t)

(:: pen-gap (-> Real))
(define (pen-gap)
  (max 1 (* 2 (plot-line-width))))

(:: animated-samples (-> Positive-Integer Positive-Integer))
(define (animated-samples samples)
  (cond [(< samples 2)  (raise-argument-error 'animated-samples "Integer >= 2" samples)]
        [(plot-animating?)  (max 2 (ceiling (* 1/4 samples)))]
        [else  samples]))

;; 3D-specific appearance

(defparam2 plot3d-samples Integer Positive-Integer 41 (integer>=2 'plot3d-samples))
(defparam2 plot3d-angle Real Real 30 (rational 'plot3d-angle))
(defparam2 plot3d-altitude Real Real 60 (rational 'plot3d-altitude))
(defparam2 plot3d-ambient-light Real Nonnegative-Real 2/3 (unit-ivl 'plot3d-ambient))
(defparam plot3d-diffuse-light? Boolean #t)
(defparam plot3d-specular-light? Boolean #t)

;; Output

(define default-plot-ps-setup (new ps-setup%))
(send default-plot-ps-setup set-margin 0 0)
(send default-plot-ps-setup set-scaling 1 1)

(defparam plot-new-window? Boolean #f)
(defparam2 plot-jpeg-quality Integer Natural 100 (integer-in 'plot-jpeg-quality 0 100))
(defparam plot-ps/pdf-interactive? Boolean #f)
(defparam plot-ps-setup (Instance PS-Setup%) default-plot-ps-setup)

;; Labels

(defparam plot-title (U String #f) #f)
(defparam plot-x-label (U String #f) "x axis")
(defparam plot-y-label (U String #f) "y axis")
(defparam plot-z-label (U String #f) #f)

(defparam plot-x-far-label (U String #f) #f)
(defparam plot-y-far-label (U String #f) #f)
(defparam plot-z-far-label (U String #f) #f)

;; Axes: transform, ticks

(defparam plot-x-transform Axis-Transform id-transform)
(defparam plot-y-transform Axis-Transform id-transform)
(defparam plot-z-transform Axis-Transform id-transform)

(defparam plot-x-ticks ticks (linear-ticks))
(defparam plot-y-ticks ticks (linear-ticks))
(defparam plot-z-ticks ticks (linear-ticks))
(defparam plot-d-ticks ticks (linear-ticks))
(defparam plot-r-ticks ticks (linear-ticks))

(defparam plot-x-far-ticks ticks (ticks-mimic plot-x-ticks))
(defparam plot-y-far-ticks ticks (ticks-mimic plot-y-ticks))
(defparam plot-z-far-ticks ticks (ticks-mimic plot-z-ticks))

;; ===================================================================================================
;; Renderer-specific parameters

;; Lines

(defparam2 line-samples Integer Positive-Integer 500 (integer>=2 'line-samples))
(defparam line-color Plot-Color 1)
(defparam2 line-width Real Nonnegative-Real 1 (nonnegative-rational 'line-width))
(defparam line-style Plot-Pen-Style 'solid)
(defparam2 line-alpha Real Nonnegative-Real 1 (unit-ivl 'line-alpha))

;; Intervals

(defparam interval-color Plot-Color 3)
(defparam interval-style Plot-Brush-Style 'solid)
(defparam interval-line1-color Plot-Color 3)
(defparam2 interval-line1-width Real Nonnegative-Real 1 (nonnegative-rational 'interval-line1-width))
(defparam interval-line1-style Plot-Pen-Style 'solid)
(defparam interval-line2-color Plot-Color 3)
(defparam2 interval-line2-width Real Nonnegative-Real 1 (nonnegative-rational 'interval-line2-width))
(defparam interval-line2-style Plot-Pen-Style 'solid)
(defparam2 interval-alpha Real Nonnegative-Real 3/4 (unit-ivl 'interval-alpha))

;; Points

(defparam point-sym Point-Sym 'circle)
(defparam point-color Plot-Color 0)
(defparam point-x-jitter Real 0)
(defparam point-y-jitter Real 0)
(defparam point-z-jitter Real 0)
(defparam2 point-size Real Nonnegative-Real 6 (nonnegative-rational 'point-size))
(defparam2 point-line-width Real Nonnegative-Real 1 (nonnegative-rational 'point-line-width))
(defparam2 point-alpha Real Nonnegative-Real 1 (unit-ivl 'point-alpha))

;; Vector fields

(defparam vector-field-samples Positive-Integer 20)
(defparam vector-field-color Plot-Color 1)
(defparam2 vector-field-line-width Real Nonnegative-Real 2/3
  (nonnegative-rational 'vector-field-line-width))
(defparam vector-field-line-style Plot-Pen-Style 'solid)
(defparam vector-field-scale (U Real 'auto 'normalized) 'auto)
(defparam2 vector-field-alpha Real Nonnegative-Real 1 (unit-ivl 'vector-field-alpha))
(defparam vector-field3d-samples Positive-Integer 9)

;; Error bars

(defparam2 error-bar-width Real Nonnegative-Real 6 (nonnegative-rational 'error-bar-width))
(defparam error-bar-color Plot-Color 0)
(defparam2 error-bar-line-width Real Nonnegative-Real 1 (nonnegative-rational 'error-bar-line-width))
(defparam error-bar-line-style Plot-Pen-Style 'solid)
(defparam2 error-bar-alpha Real Nonnegative-Real 2/3 (unit-ivl 'error-bar-alpha))

;; Contours

(:: default-contour-colors (-> (Listof Real) (Listof Plot-Color)))
(define (default-contour-colors zs)
  (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
              (length zs)))

(:: default-contour-fill-colors (-> (Listof ivl) (Listof Plot-Color)))
(define (default-contour-fill-colors z-ivls)
  (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
              (length z-ivls)))

(defparam2 contour-samples Integer Positive-Integer 51 (integer>=2 'contour-samples))
(defparam contour-levels Contour-Levels 'auto)
(defparam contour-colors (Plot-Colors (Listof Real)) default-contour-colors)
(defparam contour-widths (Pen-Widths (Listof Real)) '(1))
(defparam contour-styles (Plot-Pen-Styles (Listof Real)) '(solid long-dash))
(defparam contour-alphas (Alphas (Listof Real)) '(1))

(defparam contour-interval-colors (Plot-Colors (Listof ivl)) default-contour-fill-colors)
(defparam contour-interval-styles (Plot-Brush-Styles (Listof ivl)) '(solid))
(defparam contour-interval-alphas (Alphas (Listof ivl)) '(1))

;; Rectangles

(defparam rectangle-color Plot-Color 3)
(defparam rectangle-style Plot-Brush-Style 'solid)
(defparam rectangle-line-color Plot-Color 3)
(defparam2 rectangle-line-width Real Nonnegative-Real 1 (nonnegative-rational 'rectangle-line-width))
(defparam rectangle-line-style Plot-Pen-Style 'solid)
(defparam2 rectangle-alpha Real Nonnegative-Real 1 (unit-ivl 'rectangle-alpha))
(defparam2 rectangle3d-line-width Real Nonnegative-Real 1/3
  (nonnegative-rational 'rectangle3d-line-width))

(defparam2 discrete-histogram-gap Real Nonnegative-Real 1/8 (unit-ivl 'discrete-histogram-gap))
(defparam2 discrete-histogram-skip Real Nonnegative-Real 1
  (nonnegative-rational 'discrete-histogram-skip))
(defparam discrete-histogram-invert? Boolean #f)

(defparam stacked-histogram-colors (Plot-Colors Natural) (λ ([n : Natural]) (build-list n add1)))
(defparam stacked-histogram-styles (Plot-Brush-Styles Natural) '(solid))
(defparam stacked-histogram-line-colors (Plot-Colors Natural) (stacked-histogram-colors))
(defparam stacked-histogram-line-widths (Pen-Widths Natural) '(1))
(defparam stacked-histogram-line-styles (Plot-Pen-Styles Natural) '(solid))
(defparam stacked-histogram-alphas (Alphas Natural) '(1))

;; Decorations

(defparam x-axis-ticks? Boolean #t)
(defparam y-axis-ticks? Boolean #t)
(defparam z-axis-ticks? Boolean #t)

(defparam x-axis-labels? Boolean #f)
(defparam y-axis-labels? Boolean #f)
(defparam z-axis-labels? Boolean #f)

(defparam x-axis-far? Boolean #f)
(defparam y-axis-far? Boolean #f)
(defparam z-axis-far? Boolean #f)

(defparam2 x-axis-alpha Real Nonnegative-Real 1 (unit-ivl 'x-axis-alpha))
(defparam2 y-axis-alpha Real Nonnegative-Real 1 (unit-ivl 'y-axis-alpha))
(defparam2 z-axis-alpha Real Nonnegative-Real 1 (unit-ivl 'z-axis-alpha))

(defparam2 polar-axes-number Integer Natural 12 (natural 'polar-axes-number))
(defparam polar-axes-ticks? Boolean #t)
(defparam polar-axes-labels? Boolean #t)
(defparam2 polar-axes-alpha Real Nonnegative-Real 1/2 (unit-ivl 'polar-axes-alpha))

(defparam label-anchor Anchor 'left)
(defparam2 label-angle Real Real 0 (rational 'label-angle))
(defparam2 label-alpha Real Nonnegative-Real 1 (unit-ivl 'label-alpha))
(defparam2 label-point-size Real Nonnegative-Real 4 (nonnegative-rational 'label-point-size))

;; Surfaces

(defparam surface-color Plot-Color 0)
(defparam surface-style Plot-Brush-Style 'solid)
(defparam surface-line-color Plot-Color 0)
(defparam2 surface-line-width Real Nonnegative-Real 1/3 (nonnegative-rational 'surface-line-width))
(defparam surface-line-style Plot-Pen-Style 'solid)
(defparam2 surface-alpha Real Nonnegative-Real 1 (unit-ivl 'surface-alpha))

;; Contour surfaces

(defparam contour-interval-line-colors (Plot-Colors (Listof ivl)) '(0))
(defparam contour-interval-line-widths (Pen-Widths (Listof ivl)) '(1/3))
(defparam contour-interval-line-styles (Plot-Pen-Styles (Listof ivl)) '(solid))

;; Isosurfaces

(:: default-isosurface-colors (-> (Listof Real) (Listof Plot-Color)))
(define (default-isosurface-colors zs)
  (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
              (length zs)))

(:: default-isosurface-line-colors (-> (Listof Real) (Listof Plot-Color)))
(define (default-isosurface-line-colors zs)
  (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
              (length zs)))

(defparam isosurface-levels Contour-Levels 'auto)
(defparam isosurface-colors (Plot-Colors (Listof Real)) default-isosurface-colors)
(defparam isosurface-styles (Plot-Brush-Styles (Listof Real)) '(solid))
(defparam isosurface-line-colors (Plot-Colors (Listof Real)) default-isosurface-line-colors)
(defparam isosurface-line-widths (Pen-Widths (Listof Real)) '(1/3))
(defparam isosurface-line-styles (Plot-Pen-Styles (Listof Real)) '(solid))
(defparam isosurface-alphas (Alphas (Listof Real)) '(1/2))
