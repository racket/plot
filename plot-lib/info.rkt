#lang info

(define collection 'multi)

(define deps '("base"
               "draw-lib"
               "pict-lib"
               "db-lib"
               "srfi-lite-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "compatibility-lib"
               "math-lib"
               "unstable-latent-contract-lib"))

(define build-deps '())

(define pkg-desc "Plot non-GUI interface")

(define pkg-authors '(ntoronto))
