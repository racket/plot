#lang typed/racket/base

(require typed/racket/unsafe
         typed/racket/draw
         typed/racket/class
         "evil-types.rkt")

(provide Pict pict? dc
         new-post-script-dc%
         new-pdf-dc%
         new-svg-dc%)

(unsafe-require/typed "untyped-utils.rkt"
 [dc
  (-> (-> (Instance DC<%>) Real Real Any) Real Real Pict)]
 [new-post-script-dc%
  (-> Positive-Integer Positive-Integer (U Path-String Output-Port) (Instance DC<%>))]
 [new-pdf-dc%
  (-> Positive-Integer Positive-Integer (U Path-String Output-Port) (Instance DC<%>))]
 [new-svg-dc%
  (-> Positive-Integer Positive-Integer (U Path-String Output-Port) (Instance DC<%>))]
 )
