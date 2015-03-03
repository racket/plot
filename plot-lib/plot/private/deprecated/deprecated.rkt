#lang racket/base

(require racket/contract
         racket/class
         plot/utils
         "../common/deprecation-warning.rkt"
         "renderers.rkt")

(provide 
 mix
 (contract-out
  [line
   (->* [(real? . -> . (or/c real? (vector/c real? real?)))]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:width (>=/c 0)
         #:color plot-color/c
         #:mode (one-of/c 'standard 'parametric)
         #:mapping (one-of/c 'cartesian 'polar)
         #:t-min real?
         #:t-max real?]
        renderer2d?)]
  [contour
   (->* [(real? real? . -> . real?)]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:width (>=/c 0)
         #:color plot-color/c
         #:levels (or/c (and/c exact-integer? (>=/c 2)) (listof real?))]
        renderer2d?)]
  [shade
   (->* [(real? real? . -> . real?)]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:levels (or/c (and/c exact-integer? (>=/c 2)) (listof real?))]
        renderer2d?)]
  [surface
   (->* [(real? real? . -> . real?)]
        [#:samples (and/c exact-integer? (>=/c 2))
         #:width (>=/c 0)
         #:color plot-color/c]
        renderer3d?)]))

(define (mix . renderers)
  (deprecation-warning "mix" "list")
  (apply list renderers))

(define (line f
              #:samples [samples 150]
              #:width [width 1]
              #:color [color 'red]
              #:mode [mode 'standard]
              #:mapping [mapping 'cartesian]
              #:t-min [t-min -5]
              #:t-max [t-max 5])
  (deprecation-warning "line" "function, parametric or polar")
  (line-renderer f samples width color mode mapping t-min t-max))

(define (contour f
                 #:samples [samples 50]
                 #:width [width 1]
                 #:color [color 'black]
                 #:levels [levels 10])
  (deprecation-warning "contour" "contours")
  (contour-renderer f samples width color levels))

(define (shade f #:samples [samples 50] #:levels [levels 10])
  (deprecation-warning "shade" "contour-intervals")
  (shade-renderer f samples levels))

(define (surface f #:samples [samples 50] #:width [width 1] #:color [color 'black])
  (deprecation-warning "surface" "surface3d")
  (surface-renderer f samples width color))
