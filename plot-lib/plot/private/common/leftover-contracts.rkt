#lang racket/base

(require racket/contract
         unstable/latent-contract/defthing
         "axis-transform.rkt"
         "sample.rkt"
         "math.rkt"
         "ticks.rkt")

(provide (all-defined-out))

(defcontract axis-transform/c (real? real? invertible-function? . -> . invertible-function?))

(defcontract ticks-layout/c (real? real? . -> . (listof pre-tick?)))
(defcontract ticks-format/c (real? real? (listof pre-tick?) . -> . (listof string?)))

(defcontract sampler/c
  (-> rational-ivl? exact-nonnegative-integer? sample?))

(defcontract 2d-sampler/c
  (-> (vector/c rational-ivl? rational-ivl?)
      (vector/c exact-nonnegative-integer? exact-nonnegative-integer?)
      2d-sample?))

(defcontract 3d-sampler/c
  (-> (vector/c rational-ivl? rational-ivl? rational-ivl?)
      (vector/c exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
      3d-sample?))

(defcontract bounds-fun/c ((vectorof ivl?) . -> . (vectorof ivl?)))
(defcontract ticks-fun/c ((vectorof ivl?) . -> . any))
