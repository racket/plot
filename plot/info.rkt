#lang info

(define collection 'multi)

(define deps '("plot-lib" "plot-gui-lib" "plot-doc"))
(define implies '("plot-lib" "plot-gui-lib" "plot-doc"))

(define pkg-desc "Functions (and docs and tests) for plotting")

(define pkg-authors '(ntoronto))

(define test-responsibles '((all AlexHarsanyi@gmail.com)))

(define license
  '(Apache-2.0 OR MIT))
