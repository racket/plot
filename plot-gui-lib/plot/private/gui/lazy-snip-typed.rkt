#lang s-exp typed-racket/base-env/extra-env-lang

(require "lazy-snip-untyped.rkt"
         "lazy-snip-types.rkt"
         (for-syntax (submod "lazy-snip-types.rkt" #%type-decl)))

(provide make-2d-plot-snip
         make-3d-plot-snip
         make-snip-frame)

(type-environment
 [make-2d-plot-snip  (parse-type #'Make-2D-Plot-Snip)]
 [make-3d-plot-snip  (parse-type #'Make-3D-Plot-Snip)]
 [make-snip-frame  (parse-type #'Make-Snip-Frame)])
