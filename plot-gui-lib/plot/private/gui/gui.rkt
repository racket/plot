#lang racket/base

;; GUI helpers

(require racket/gui/base racket/class mrlib/snip-canvas)

(provide (all-defined-out))

(define snip-frame%
  (class frame%
    (define/override (on-traverse-char event)
      (define key-code (send event get-key-code))
      (case key-code
        [(escape)  (send this show #f)]
        [else  (super on-traverse-char event)]))
    
    (super-new)))

(define (make-snip-frame make-snip width height label)

  (define frame
    (new snip-frame% [label label] [width (+ 20 width)] [height (+ 20 height)]))
  
  (new snip-canvas%
       [parent frame]
       [make-snip make-snip]
       [horiz-margin 5] [vert-margin 5]
       [horizontal-inset 5] [vertical-inset 5])
  
  frame)
