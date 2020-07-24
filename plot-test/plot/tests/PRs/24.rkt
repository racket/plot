#lang racket
(require rackunit
         plot
         plot/utils
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/24

;; Plot all `known-point-symbols`.
(define (do-plot output-fn)
  (define NUM-ROWS 3)
  (define YOFFSET 4)
  (parameterize ([plot-x-axis? #f]
                 [plot-y-axis? #f]
                 [plot-x-far-axis? #f]
                 [plot-y-far-axis? #f]
                 [plot-decorations? #f])
    (output-fn
     (for/list ([s (in-list known-point-symbols)]
                [i (in-naturals)])
       (points (list (list (modulo i NUM-ROWS) (- (* YOFFSET (quotient i NUM-ROWS)))))
               #:sym s
               #:size 10))
     #:x-min -1
     #:x-max NUM-ROWS
     #:y-min (- (* YOFFSET (add1 (quotient (length known-point-symbols) NUM-ROWS))))
     #:y-max 1)))

(define-runtime-path pr24-data "./test-data/pr24.dat")

(define pr24-test-suite
  (test-suite
   "PR#24: Fix typo, 'cirlce2' => 'circle2'"
   (test-case "pr24" (check-draw-steps do-plot pr24-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr24-test-suite))
