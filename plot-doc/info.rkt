#lang info

(define collection 'multi)

(define deps '("base"
               "plot-lib"
               "plot-gui-lib"))

(define build-deps '("db-doc"
                     "db-lib"
                     "draw-doc"
                     "draw-lib"
                     "gui-doc"
                     "gui-lib"
                     "pict-doc"
                     "pict-lib"
                     "plot-compat"
                     "racket-doc"
                     "scribble-lib"
                     "slideshow-doc"
                     "slideshow-lib"
                     "srfi-doc"))

(define update-implies '("plot-lib"))

(define pkg-desc "Documentation for plot")

(define pkg-authors '(ntoronto))

(define test-responsibles '((all AlexHarsanyi@gmail.com)))

(define license
  '(Apache-2.0 OR MIT))
