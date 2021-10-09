#lang info

(define collection 'multi)

(define deps '("base"
               "plot-gui-lib"
               "draw-lib"
               "plot-lib"
               "snip-lib"))

(define build-deps '())

(define pkg-desc "Compatibility library for Plot 5.1.3 and earlier")

(define pkg-authors '(ntoronto))

(define test-responsibles '((all AlexHarsanyi@gmail.com)))

(define license
  '(Apache-2.0 OR MIT))
