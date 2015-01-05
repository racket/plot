#lang racket/base

(require racket/draw
         racket/class
         (prefix-in pict. pict)
         "../common/parameters.rkt")

(provide (all-defined-out))

(define (new-post-script-dc% width height output)
  (parameterize ([current-ps-setup  (plot-ps-setup)])
    (new post-script-dc%
         [interactive (plot-ps/pdf-interactive?)] [parent #f] [use-paper-bbox #f]
         [as-eps #t] [width width] [height height] [output output])))

(define (new-pdf-dc% width height output)
  (parameterize ([current-ps-setup  (plot-ps-setup)])
    (new pdf-dc%
         [interactive (plot-ps/pdf-interactive?)] [parent #f] [use-paper-bbox #f]
         [width width] [height height] [output output])))

(define (new-svg-dc% width height output)
  (new svg-dc% [width width] [height height] [output output] [exists 'truncate/replace]))

(define (dc the-dc width height)
  (pict.dc the-dc width height))
