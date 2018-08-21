#lang racket/base
(require plot racket/class racket/gui/base)

(define show-frame/subspace
  (let ([other-eventspace (make-eventspace)])
    (lambda (make-frame [sleep-seconds 5])
      (define f
        (parameterize ([current-eventspace other-eventspace])
          (make-frame)))
      (send f show #t)
      (sleep sleep-seconds)
      (send f show #f))))

;; These plots should NOT have decorations. If they do, that means the parameter
;; is somehow ignored.
(show-frame/subspace
  (lambda ()
    (parameterize ((plot-decorations? #f))
      (plot3d-frame (surface3d (λ (x y) (* (cos x) (sin y)))
                               -3.0 3.0 -3.0 3.0)))))
(show-frame/subspace
  (lambda ()
    (parameterize ((plot-decorations? #f))
      (plot-frame (function sin) #:x-min -3 #:x-max 3))))
