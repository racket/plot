#lang racket/base

(require racket/contract
         "axis-transform.rkt"
         "sample.rkt"
         "math.rkt"
         "ticks.rkt")

(provide (all-defined-out))

(define axis-transform/c (real? real? invertible-function? . -> . invertible-function?))

(define ticks-layout/c (real? real? . -> . (listof pre-tick?)))
(define ticks-format/c (real? real? (listof pre-tick?) . -> . (listof string?)))

(define sampler/c
  (-> rational-ivl? exact-nonnegative-integer? sample?))

(define 2d-sampler/c
  (-> (vector/c rational-ivl? rational-ivl?)
      (vector/c exact-nonnegative-integer? exact-nonnegative-integer?)
      2d-sample?))

(define 3d-sampler/c
  (-> (vector/c rational-ivl? rational-ivl? rational-ivl?)
      (vector/c exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
      3d-sample?))

(define bounds-fun/c ((vectorof ivl?) . -> . (vectorof ivl?)))
(define ticks-fun/c ((vectorof ivl?) . -> . any))
