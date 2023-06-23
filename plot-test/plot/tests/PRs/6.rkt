#lang racket

(require rackunit
         plot racket/runtime-path
         "../helpers.rkt")

;; See https://github.com/racket/plot/issues/6 and associated discution.

(define-runtime-path pr6-data-round "./test-data/pr6-round.dat")
(define-runtime-path pr6-data-butt "./test-data/pr6-butt.dat")
(define-runtime-path pr6-data-projecting "./test-data/pr6-projecting.dat")

(define (do-plot-pr6 output-fn)
  (parameterize
      ([plot-line-width 10]
       [line-width 10]
       [plot-pen-color-map 'set1])
    (output-fn
     (list
      (function sin -5 5 #:color 1 #:label "sin(x)")
      (function cos -5 5 #:color 2 #:label "cos(x)")))))

(define pr6-test-suite
  (test-suite
   "PR#6: Add plot-line-cap and line-cap parameters"
   (test-case "pr6-round"
     (parameterize ([plot-line-cap 'round]
                    [line-cap 'round])
       (check-draw-steps do-plot-pr6 pr6-data-round)))
   (test-case "pr6-butt"
     (parameterize ([plot-line-cap 'butt]
                    [line-cap 'butt])
       (check-draw-steps do-plot-pr6 pr6-data-butt)))
   (test-case "pr6-projecting"
     (parameterize ([plot-line-cap 'projecting]
                    [line-cap 'projecting])
       (check-draw-steps do-plot-pr6 pr6-data-projecting)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr6-test-suite))
