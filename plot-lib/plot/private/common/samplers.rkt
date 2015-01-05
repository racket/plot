#lang typed/racket/base

(require racket/match racket/flonum racket/math racket/list
         "type-doc.rkt"
         "parameters.rkt"
         "sample.rkt"
         "math.rkt")

(provide (all-defined-out))

(defthing function->sampler (-> (-> Real Real) ivl Sampler)
  (make-function->sampler plot-x-transform))

(defthing inverse->sampler (-> (-> Real Real) ivl Sampler)
  (make-function->sampler plot-y-transform))

(defthing 2d-function->sampler (-> (-> Real Real Real) (Vector ivl ivl) 2D-Sampler)
  (make-2d-function->sampler plot-x-transform plot-y-transform))

(defthing 3d-function->sampler (-> (-> Real Real Real Real) (Vector ivl ivl ivl) 3D-Sampler)
  (make-3d-function->sampler plot-x-transform plot-y-transform plot-z-transform))
