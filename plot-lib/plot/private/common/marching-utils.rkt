#lang typed/racket/base

(provide (all-defined-out))

;; Returns the interpolated distance of z from za toward zb
;; Examples: if z = za, this returns 0.0
;;           if z = zb, this returns 1.0
;;           if z = (za + zb) / 2, this returns 0.5
;; Intuitively, regard a use (solve-t z za zb) as "the point between za and zb".
(define-syntax-rule (solve-t z za zb)
  (/ (- z za) (- zb za)))

(define-syntax-rule (unsolve-t za zb t)
  (cond [(eq? t 0)  za]
        [(eq? t 1)  zb]
        [else  (+ (* t zb) (* (- 1 t) za))]))

