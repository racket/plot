#lang typed/racket/no-check

(provide Pict pict?)

(define Pict #f) ; no-check doesn't bind it below

(require/typed
 pict
 [#:opaque Pict pict?])
