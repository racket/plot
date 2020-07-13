#lang info

(define collection 'multi)

(define deps '("base"
               "plot-compat"
               "plot-gui-lib"
               "plot-lib"
               "plot-doc"
               "draw-lib"
               "pict-lib"
               "rackunit-lib"
               "slideshow-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "contract-profile"))

(define build-deps '())
(define update-implies '("plot-lib"))

(define pkg-desc "Plot tests")

(define pkg-authors '(ntoronto))

(define test-responsibles '((all AlexHarsanyi@gmail.com)))
