#lang typed/racket/base

(require "private/utils-and-no-gui.rkt")
(provide (all-from-out "private/utils-and-no-gui.rkt"))

(require "private/common/contract.rkt"
         "private/common/leftover-contracts.rkt"
         "private/common/marching-squares.rkt"
         "private/common/marching-cubes.rkt")

(provide (all-from-out 
          "private/common/contract.rkt"
          "private/common/leftover-contracts.rkt"
          "private/common/marching-squares.rkt"
          "private/common/marching-cubes.rkt"))

(require "private/common/types.rkt")

(provide
 Color
 Plot-Pen-Style-Sym
 Plot-Brush-Style-Sym
 generate-list
 Legend-Draw-Proc
 (struct-out legend-entry)
 Plot-Device%)

(require "private/common/math.rkt")

(provide
 equal?*
 ;; Flonums
 flblend
 flsumof
 fldistance
 ;; Reals
 maybe-inexact->exact
 min*
 max*
 blend
 atan2
 sumof
 real-modulo
 distance
 floor-log/base
 ceiling-log/base
 polar->cartesian
 3d-polar->3d-cartesian
 ;; Vectors
 vcross
 vcross2
 v+
 v-
 vneg
 v*
 v/
 vmag^2
 vmag
 vnormalize
 vdot
 vcos-angle
 vrational?
 v=
 vcenter
 ;; Intervals
 ivl-meet
 ivl-join
 empty-ivl
 unknown-ivl
 rational-ivl?
 ivl-empty?
 ivl-known?
 ivl-rational?
 ivl-singular?
 ivl-length
 ivl-center
 ivl-zero-length?
 ivl-inexact->exact
 ivl-contains?
 ivl-translate
 bounds->intervals
 clamp-real
 points-apply-jitters
 ;; Rectangles
 Rect
 rect-meet
 rect-join
 empty-rect
 unknown-rect
 bounding-rect
 ;rational-rect?
 rect-empty?
 rect-known?
 rect-rational?
 rect-area
 rect-center
 rect-zero-area?
 rect-singular?
 rect-inexact->exact
 rect-translate
 rect-contains?)

(require "private/common/legend.rkt")

(provide
 (struct-out legend-entry)
 line-legend-entry
 line-legend-entries
 rectangle-legend-entry
 rectangle-legend-entries
 interval-legend-entry
 interval-legend-entries
 point-legend-entry
 arrow-legend-entry)

(require "private/common/parameter-groups.rkt")

(provide
 Plot-Parameters
 plot-parameters
 plot-axes?
 plot-tick-labels
 plot-appearance
 plot3d-appearance
 plot-output
 plot-labels
 plot-axes
 plot-x-axis
 plot-y-axis
 plot-z-axis)

(require "private/common/format.rkt")

(provide
 integer->superscript
 digits-for-range
 real->decimal-string*
 real->string/trunc
 real->plot-label
 ivl->plot-label
 ->plot-label
 parse-format-string
 apply-formatter)

(require "private/common/draw-attribs.rkt")

(provide
 ->color
 ->pen-color
 ->brush-color
 ->pen-style
 ->brush-style
 color-seq
 color-seq*)

(require "private/common/sample.rkt")

(provide
 (struct-out sample)
 (struct-out 2d-sample)
 (struct-out 3d-sample)
 Sampler
 2D-Sampler
 3D-Sampler
 build-linear-seq
 linear-seq
 linear-seq*
 nonlinear-seq
 make-function->sampler
 make-2d-function->sampler
 make-3d-function->sampler
 for-2d-sample
 for-3d-sample)

(require "private/common/samplers.rkt")

(provide
 function->sampler
 inverse->sampler
 2d-function->sampler
 3d-function->sampler)

(require "private/common/plot-element.rkt"
         "private/common/nonrenderer.rkt"
         "private/plot2d/plot-area.rkt"
         "private/plot3d/plot-area.rkt"
         "private/plot2d/renderer.rkt"
         "private/plot3d/renderer.rkt")

(provide
 2D-Plot-Area%
 3D-Plot-Area%
 Bounds-Fun
 Ticks-Fun
 2D-Render-Proc
 3D-Render-Proc
 default-ticks-fun
 function-bounds-fun
 function-interval-bounds-fun
 inverse-bounds-fun
 inverse-interval-bounds-fun
 surface3d-bounds-fun
 plot3d-back-layer
 plot3d-area-layer
 plot3d-front-layer)

(require "private/common/kde.rkt")

(provide
 kde)
