#lang racket/base

(require plot/private/no-gui/plot2d-utils
         rackunit)

(module+ test
  (check-exn (regexp (regexp-quote "expected: (is-a? 2d-plot-area%)"))
             (λ () (plot-area #f #f))))
