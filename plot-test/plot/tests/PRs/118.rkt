#lang racket

(require rackunit
         plot racket/runtime-path
         "../helpers.rkt")

;; See https://github.com/racket/plot/issues/118 and associated discution.

(define-runtime-path pr118-data "./test-data/pr118.dat")

(define (do-plot-pr118 output-fn)
  (define-values (xdata ydata)
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 42)                    ; ensure our test is deterministic
      (define (randsin x)(+ (sin x) (/ ( - (random) 0.5) 5) ) )
      (define xdata (range 0 (* 2 pi) (/ pi 9 ) ) )
      (define ydata (map (lambda (x) (randsin x) ) xdata ) )
      (values xdata ydata)))

  (parameterize ([point-size 10]
                 [line-width 2]
                 [point-line-width 1.5]
                 [plot-pen-color-map 'set1]
                 [plot-x-label  #f]
                 [plot-y-label  #f])
    (output-fn (list
                (function sin 0 (* 2 pi)
                          #:label "exact"
                          #:marker 'diamond
                          #:color 0
                          #:marker-count 10)
                (lines (map vector xdata ydata )
                       #:label "discrete"
                       #:marker 'square
                       #:color 1
                       #:marker-color "black")
                (inverse sqr -2 2 #:color 3
                         #:label "x = y²"
                         #:marker '5star))
               #:legend-anchor 'top-right)))

(define pr118-test-suite
  (test-suite
   "PR#118: Add markers to lines and function renderers"
   (test-case "pr118"
     (check-draw-steps do-plot-pr118 pr118-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr118-test-suite))
