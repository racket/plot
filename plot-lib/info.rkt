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
               "math-lib"))

(define build-deps '())

(define pkg-desc "Plot non-GUI interface")

(define pkg-authors '(ntoronto))

(define version "1.1")

(define test-responsibles '((all AlexHarsanyi@gmail.com)))

(define license
  '(Apache-2.0 OR MIT))
