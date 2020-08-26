#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/66 : color-field
(define (do-plot-color-field output-fn)
  (output-fn
   (color-field
    (λ (x y)
      (define z (make-rectangular x y))
      (if (< (magnitude z) 1)
          (cond
            [(< (magnitude z) 0.5) 'red]
            [(< (angle z) 0) 'blue]
            [else 'green])
          'black))
    -2 2 -2 2)))

(define-runtime-path pr66-color-field-data "./test-data/pr66-1.dat")

(define pr66-test-suite
  (test-suite
   "PR#66: color-field"
   (test-case "pr66-color-field"
     (check-draw-steps-3d do-plot-color-field pr66-color-field-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr66-test-suite))

;;



