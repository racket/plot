#lang racket/base

(require typed/untyped-utils)

;; ===================================================================================================
;; General exports

(require "private/utils-and-no-gui.rkt")
(provide (all-from-out "private/utils-and-no-gui.rkt"))

;; ===================================================================================================
;; Nonrenderers

(require "private/common/nonrenderer.rkt")
(provide
 x-ticks
 y-ticks
 z-ticks
 invisible-rect
 invisible-rect3d)

;; ===================================================================================================
;; 2D exports

(require (rename-in "private/no-gui/plot2d.rkt"
                    [plot/dc     typed-plot/dc]
                    [plot-bitmap typed-plot-bitmap]
                    [plot-pict   typed-plot-pict])
         "private/no-gui/plot2d-untyped.rkt")

(define-typed/untyped-identifier plot/dc
  typed-plot/dc
  untyped-plot/dc)
(define-typed/untyped-identifier plot-bitmap
  typed-plot-bitmap
  untyped-plot-bitmap)
(define-typed/untyped-identifier plot-pict
  typed-plot-pict
  untyped-plot-pict)

(provide
 plot/dc
 plot-bitmap
 plot-pict
 plot-file)

(require "private/plot2d/point.rkt")
(provide
 points
 vector-field
 error-bars
 candlesticks)

(require "private/plot2d/line.rkt")
(provide
 lines
 parametric
 polar
 hrule
 vrule
 function
 inverse
 density)

(require "private/plot2d/interval.rkt")
(provide
 lines-interval
 parametric-interval
 polar-interval
 function-interval
 inverse-interval)

(require "private/plot2d/contour.rkt")
(provide
 isoline
 contours
 contour-intervals)

(require "private/plot2d/rectangle.rkt")
(provide
 rectangles
 area-histogram
 discrete-histogram
 stacked-histogram)

(require "private/plot2d/decoration.rkt")
(provide
 x-axis
 y-axis
 axes
 polar-axes
 x-tick-lines
 y-tick-lines
 tick-grid
 point-label
 point-pict
 parametric-label
 parametric-pict
 polar-label
 polar-pict
 function-label
 function-pict
 inverse-label
 inverse-pict)

;; ===================================================================================================
;; 3D exports

(require (rename-in "private/no-gui/plot3d.rkt"
                    [plot3d/dc  typed-plot3d/dc])
         "private/no-gui/plot3d-untyped.rkt")

(define-typed/untyped-identifier plot3d/dc
  typed-plot3d/dc
  untyped-plot3d/dc)

(provide
 plot3d/dc
 plot3d-bitmap
 plot3d-pict
 plot3d-file)

(require "private/plot3d/surface.rkt")
(provide
 surface3d)

(require "private/plot3d/contour.rkt")
(provide
 isoline3d
 contours3d
 contour-intervals3d)

(require "private/plot3d/line.rkt")
(provide
 lines3d
 parametric3d)

(require "private/plot3d/point.rkt")
(provide
 points3d
 vector-field3d)

(require "private/plot3d/isosurface.rkt")
(provide
 isosurface3d
 isosurfaces3d
 polar3d)

(require "private/plot3d/rectangle.rkt")
(provide
 rectangles3d
 discrete-histogram3d
 stacked-histogram3d)

(require "private/plot3d/decoration.rkt")
(provide
 point-label3d)

;; ===================================================================================================
;; Deprecated functions

(require "private/deprecated/deprecated.rkt")
(provide
 mix
 line
 contour
 shade
 surface)
