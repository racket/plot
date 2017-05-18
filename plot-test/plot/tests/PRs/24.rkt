#lang racket/base
(require plot plot/utils)

;; Plot all `known-point-symbols`.
;; This test succeeds when it terminates.

(define NUM-ROWS 3)
(define YOFFSET 4)

(parameterize ([plot-x-axis? #f]
               [plot-y-axis? #f]
               [plot-x-far-axis? #f]
               [plot-y-far-axis? #f]
               [plot-decorations? #f])
  (plot
    (for/list ([s (in-list known-point-symbols)]
               [i (in-naturals)])
      (points (list (list (modulo i NUM-ROWS) (- (* YOFFSET (quotient i NUM-ROWS)))))
        #:sym s
        #:size 10))
    #:x-min -1
    #:x-max NUM-ROWS
    #:y-min (- (+ 1 (* YOFFSET (quotient (length known-point-symbols) NUM-ROWS))))
    #:y-max 1))
