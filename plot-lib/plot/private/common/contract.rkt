#lang racket/base

(require racket/contract
         racket/draw
         racket/class
         unstable/contract)

(provide
 (except-out
  (all-defined-out)
  maybe-function/c
  maybe-apply
  plot-colors/c
  pen-widths/c
  plot-pen-styles/c
  plot-brush-styles/c
  alphas/c
  labels/c)
 (contract-out
  [maybe-function/c  (-> contract? contract? contract?)]
  [maybe-apply       (-> (maybe-function/c any/c any/c) any/c any/c)]
  [plot-colors/c        (-> contract? contract?)]
  [pen-widths/c         (-> contract? contract?)]
  [plot-pen-styles/c    (-> contract? contract?)]
  [plot-brush-styles/c  (-> contract? contract?)]
  [alphas/c             (-> contract? contract?)]
  [labels/c             (-> contract? contract?)])
 (rename-out [natural-number/c nat/c])
 font-family/c
 truth/c)

;; ===================================================================================================
;; Plot-specific contracts

(define anchor/c (one-of/c 'top-left    'top    'top-right
                           'left        'center 'right
                           'bottom-left 'bottom 'bottom-right))

(define color/c (or/c (list/c real? real? real?)
                      string? symbol?
                      (is-a?/c color%)))

(define plot-color/c (or/c exact-integer? color/c))

(define plot-pen-style/c (or/c exact-integer?
                               (one-of/c 'transparent 'solid    'dot 'long-dash
                                         'short-dash  'dot-dash)))

(define plot-brush-style/c (or/c exact-integer?
                                 (one-of/c 'transparent      'solid
                                           'bdiagonal-hatch  'fdiagonal-hatch 'crossdiag-hatch
                                           'horizontal-hatch 'vertical-hatch  'cross-hatch)))

(module typed-defs typed/racket/base
  (require "type-doc.rkt")
  
  (provide (all-defined-out))
  
  (defthing known-point-symbols (Listof Symbol) #:document-value
    (list 'dot               'point            'pixel
          'plus              'times            'asterisk
          '5asterisk         'odot             'oplus
          'otimes            'oasterisk        'o5asterisk
          'circle            'square           'diamond
          'triangle          'fullcircle       'fullsquare
          'fulldiamond       'fulltriangle     'triangleup
          'triangledown      'triangleleft     'triangleright
          'fulltriangleup    'fulltriangledown 'fulltriangleleft
          'fulltriangleright 'rightarrow       'leftarrow
          'uparrow           'downarrow        '4star
          '5star             '6star            '7star
          '8star             'full4star        'full5star
          'full6star         'full7star        'full8star
          'circle1           'circle2          'circle3
          'circle4           'circle5          'circle6
          'circle7           'circle8          'bullet
          'fullcircle1       'fullcircle2      'fullcircle3
          'fullcircle4       'fullcircle5      'fullcircle6
          'fullcircle7       'fullcircle8)))

(require (submod "." typed-defs))
(provide (all-from-out 'typed-defs))

(define point-sym/c (or/c char? string? integer? (apply one-of/c known-point-symbols)))

(define (maybe-function/c in-contract out-contract)
  (or/c out-contract (in-contract . -> . out-contract)))

(define (maybe-apply f arg)
  (cond [(procedure? f)  (f arg)]
        [else            f]))

(define (plot-colors/c in-contract)
  (maybe-function/c in-contract (listof plot-color/c)))

(define (pen-widths/c in-contract)
  (maybe-function/c in-contract (listof (>=/c 0))))

(define (plot-pen-styles/c in-contract)
  (maybe-function/c in-contract (listof plot-pen-style/c)))

(define (plot-brush-styles/c in-contract)
  (maybe-function/c in-contract (listof plot-brush-style/c)))

(define (alphas/c in-contract)
  (maybe-function/c in-contract (listof (real-in 0 1))))

(define (labels/c in-contract)
  (maybe-function/c in-contract (listof (or/c string? #f))))
