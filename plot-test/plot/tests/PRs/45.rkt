#lang racket/base
(require plot)

(plot-new-window? #t) ;; to regression-test #45

;; These plots should NOT have decorations. If they do, that means the parameter
;; is somehow ignored.
(parameterize ((plot-decorations? #f))
  (plot3d (surface3d (λ (x y) (* (cos x) (sin y)))
                     -3.0 3.0 -3.0 3.0)))
(parameterize ((plot-decorations? #f))
  (plot (function sin) #:x-min -3 #:x-max 3))
