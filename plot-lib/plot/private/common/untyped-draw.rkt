#lang racket/base

(require racket/draw
         racket/class)

(provide (all-defined-out))

(define (set-dc-alignment-scale dc s)
  (send dc set-alignment-scale s))
