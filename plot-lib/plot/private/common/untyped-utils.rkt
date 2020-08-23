#lang racket/base

(require "utils.rkt")

(provide (all-defined-out))

(define (fix-vector-field-fun name f)
  (cond [(procedure-arity-includes? f 2 #t)
         (λ (x y) (sequence-head-vector name (f x y) 2))]
        [else
         (λ (x y) (sequence-head-vector name (f (vector x y)) 2))]))

(define (fix-a-field-fun name f)
  (cond [(procedure-arity-includes? f 2 #t) f]
        [else (λ (x y) (f (vector x y)))]))

(define (fix-vector-field3d-fun name f)
  (cond [(procedure-arity-includes? f 3 #t)
         (λ (x y z) (sequence-head-vector name (f x y z) 3))]
        [else
         (λ (x y z) (sequence-head-vector name (f (vector x y z)) 3))]))
