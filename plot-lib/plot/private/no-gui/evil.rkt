#lang s-exp typed-racket/base-env/extra-env-lang

(require typed/racket/base
         typed/racket/draw
         typed/racket/class
         (only-in "untyped-utils.rkt"
                  new-post-script-dc%
                  new-pdf-dc%
                  new-svg-dc%
                  dc)  ; uncontracted wrapper for `dc` in `pict` module
         "evil-types.rkt"  ; for Pict and pict?
         (for-syntax (submod "evil-types.rkt" #%type-decl)))

(provide Pict pict? dc
         new-post-script-dc%
         new-pdf-dc%
         new-svg-dc%)

(type-environment
 [dc
  (parse-type #'(-> (-> (Instance DC<%>) Real Real Any) Real Real Pict))]
 [new-post-script-dc%
  (parse-type #'(-> Positive-Integer Positive-Integer (U Path-String Output-Port) (Instance DC<%>)))]
 [new-pdf-dc%
  (parse-type #'(-> Positive-Integer Positive-Integer (U Path-String Output-Port) (Instance DC<%>)))]
 [new-svg-dc%
  (parse-type #'(-> Positive-Integer Positive-Integer (U Path-String Output-Port) (Instance DC<%>)))]
 )
