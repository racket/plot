#lang racket/base

(module untyped-defs racket/base
  (require "parameters.rkt"
           "parameter-group.rkt")
  
  (provide (all-defined-out))
  
  (define-parameter-group plot-axes?
    (plot-x-axis? plot-x-far-axis?
                  plot-y-axis? plot-y-far-axis?
                  plot-z-axis? plot-z-far-axis?))
  
  (define-parameter-group plot-tick-labels
    (plot-x-tick-label-anchor
     plot-x-tick-label-angle
     plot-x-far-tick-label-anchor
     plot-x-far-tick-label-angle
     plot-y-tick-label-anchor
     plot-y-tick-label-angle
     plot-y-far-tick-label-anchor
     plot-y-far-tick-label-angle))
  
  (define-parameter-group plot-appearance
    (plot-width
     plot-height
     plot-foreground plot-foreground-alpha
     plot-background plot-background-alpha
     plot-line-width plot-tick-size
     plot-font-size plot-font-face plot-font-family
     plot-legend-anchor plot-legend-box-alpha
     plot-axes? plot-tick-labels
     plot-decorations?
     plot-animating?))
  
  (define-parameter-group plot3d-appearance
    (plot3d-samples
     plot3d-angle
     plot3d-altitude
     plot3d-ambient-light
     plot3d-diffuse-light?
     plot3d-specular-light?))
  
  (define-parameter-group plot-output
    (plot-new-window? plot-jpeg-quality plot-ps/pdf-interactive? plot-ps-setup))
  
  (define-parameter-group plot-labels
    (plot-title
     plot-x-label plot-y-label plot-z-label
     plot-x-far-label plot-y-far-label plot-z-far-label))
  
  (define-parameter-group plot-x-axis (plot-x-transform plot-x-ticks plot-x-far-ticks))
  (define-parameter-group plot-y-axis (plot-y-transform plot-y-ticks plot-y-far-ticks))
  (define-parameter-group plot-z-axis (plot-z-transform plot-z-ticks plot-z-far-ticks))
  (define-parameter-group plot-axes (plot-x-axis plot-y-axis plot-z-axis plot-d-ticks plot-r-ticks))
  
  (define-parameter-group plot-parameters
    (plot-appearance
     plot3d-appearance
     plot-labels
     plot-output
     plot-axes))
  )

(module typed-defs typed/racket/base
  (require typed/racket/draw
           (submod ".." untyped-defs)
           "type-doc.rkt"
           "types.rkt"
           "axis-transform.rkt"
           "ticks.rkt")
  
  (provide Plot-Parameters)
  
  (deftype Plot-Parameters
    (List
     (List
      Positive-Integer
      Positive-Integer
      Plot-Color
      Nonnegative-Real
      Plot-Color
      Nonnegative-Real
      Nonnegative-Real
      Nonnegative-Real
      Nonnegative-Real
      (U False String)
      Font-Family
      Anchor
      Nonnegative-Real
      (List Boolean Boolean Boolean Boolean Boolean Boolean)
      (List Anchor Real Anchor Real Anchor Real Anchor Real)
      Boolean
      Boolean)
     (List Positive-Integer Real Real Nonnegative-Real Boolean Boolean)
     (List
      (U False String)
      (U False String)
      (U False String)
      (U False String)
      (U False String)
      (U False String)
      (U False String))
     (List Boolean Nonnegative-Integer Boolean (Instance PS-Setup%))
     (List
      (List Axis-Transform ticks ticks)
      (List Axis-Transform ticks ticks)
      (List Axis-Transform ticks ticks)
      ticks
      ticks)))
  
  (define (test) (ann (plot-parameters) Plot-Parameters)))

(require 'untyped-defs
         'typed-defs)

(provide (all-from-out
          'untyped-defs
          'typed-defs))
