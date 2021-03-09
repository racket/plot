#lang typed/racket

(require typed/pict)

(provide Pict pict?)

(define-type Pict pict)
#;(require/typed
 pict
 [#:opaque Pict pict?])
