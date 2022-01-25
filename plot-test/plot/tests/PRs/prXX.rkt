#lang racket
(require rackunit
         plot pict racket/runtime-path
         math/distributions
         "../helpers.rkt")

(random-seed 42)

(define (rnorm sample-count mean stddev)
  (sample (normal-dist mean stddev) sample-count))

(define a (rnorm 500 10 5))
(define b (append (rnorm 500 13 1) (rnorm 500 18 1)))
(define c (rnorm 20 25 4))
(define d (rnorm 100 12 1))

(plot-pen-color-map 'tab20)
(plot-brush-color-map 'tab20)

(define ((do-plot-box-and-whisker invert? show-label? width) output-fn)
  (parameterize ([plot-x-label #f]
                 [plot-y-label #f])
    (output-fn
     (for/list ([data-set (list a b c d)]
                [label (list "a" "b" "c" "d")]
                [index (in-naturals)])
       (box-and-whisker data-set
                        #:label (and show-label? label)
                        #:invert? invert?
                        #:x index
                        #:width width
                        #:box-color (+ (* index 2) 1)
                        #:box-alpha 0.8
                        #:box-line-color (* index 2)
                        #:whisker-color (* index 2)
                        #:median-color "red"
                        ))
     #:legend-anchor 'no-legend)))

(define ((do-plot-violin invert? show-label? width) output-fn)
  (parameterize ([plot-x-label #f]
                 [plot-y-label #f])
    (output-fn
     (for/list ([data-set (list a b c d)]
                [label (list "a" "b" "c" "d")]
                [index (in-naturals)])
       (violin data-set
                        #:label (and show-label? label)
                        #:invert? invert?
                        #:x index
                        #:width width
                        #:color (+ (* index 2) 1)
                        #:alpha 0.8
                        #:line-color (* index 2)
                        ))
     #:legend-anchor 'no-legend)))

(define-runtime-path prXX-a "./test-data/prXX-a.dat")
(define-runtime-path prXX-b "./test-data/prXX-b.dat")
(define-runtime-path prXX-c "./test-data/prXX-c.dat")
(define-runtime-path prXX-d "./test-data/prXX-d.dat")
(define-runtime-path prXX-e "./test-data/prXX-e.dat")
(define-runtime-path prXX-f "./test-data/prXX-f.dat")

(define prXX-test-suite
  (test-suite
   "box-and-whisker"
   (test-case "PR#XX: box-and-whisker case A"
     (check-draw-steps (do-plot-box-and-whisker #f #t 7/8) prXX-a))
   (test-case "PR#XX: box-and-whisker case B"
     (check-draw-steps (do-plot-box-and-whisker #f #f 1.5) prXX-b))
   (test-case "PR#XX: box-and-whisker case C"
     (check-draw-steps (do-plot-box-and-whisker #t #t 1.5) prXX-c))
   (test-case "PR#XX: violin case D"
     (check-draw-steps (do-plot-violin #f #t 7/8) prXX-d))
   (test-case "PR#XX: violin case E"
     (check-draw-steps (do-plot-violin #f #f 1.5) prXX-e))
   (test-case "PR#XX: violin case F"
     (check-draw-steps (do-plot-violin #t #t 1.5) prXX-f))))

(module+ test
  (require rackunit/text-ui)
  (run-tests prXX-test-suite))
