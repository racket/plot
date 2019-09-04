#lang racket
(require plot)

(time
  (plot3d
    (polygons3d (list (list (list 1 0 0) (list 0 0 1) (list 0 1 0))
                      (list (list 0 0 0) (list 0 0 1) (list 0 1 0))
                      (list (list 1 0 0) (list 0 0 1) (list 0 0 0))))))

(time
  (plot3d
   (parametric-surface3d (λ (ϕ φ)
                           (list (* (cos ϕ) (cos φ))
                                 (* (sin ϕ) (cos φ))
                                 (sin φ)))
                         0 (* 2 (angle -1)) #:s-samples 80
                         (- (/ (angle -1) 2)) (/ (angle -1) 2) #:t-samples 40)))

(time
  (plot3d (polar3d (λ (ϕ φ) 1))))
