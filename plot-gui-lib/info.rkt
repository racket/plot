#lang info

(define collection 'multi)

(define deps '("base"
               "plot-lib"
               "math-lib"
               ["gui-lib" #:version "1.18"]
               "snip-lib"
               "typed-racket-lib"
               "typed-racket-more"))

(define build-deps '())

(define pkg-desc "Plot GUI interface")

(define pkg-authors '(ntoronto))

(define version "1.1")

(define test-responsibles '((all AlexHarsanyi@gmail.com)))

(define license
  '(Apache-2.0 OR MIT))
