#lang racket
(require plot racket/draw rackunit)

(plot-width 300)
(plot-height 300)

(define (generate-draw-steps renderer-tree)
  (define dc (new record-dc% [width (plot-width)] [height (plot-height)]))
  (plot/dc renderer-tree dc 0 0 (plot-width) (plot-height))
  (send dc get-recorded-datum))

(define (check-same-draw-commands set1 set2)
  (cond ((and (pair? set1) (pair? set2))
         (check-same-draw-commands (car set1) (car set2))
         (check-same-draw-commands (cdr set1) (cdr set2)))
        ((and (number? set1) (number? set2))
         (check-= set1 set2 1e-4))
        (#t
         (check-equal? set1 set2))))

(provide generate-draw-steps check-same-draw-commands)
