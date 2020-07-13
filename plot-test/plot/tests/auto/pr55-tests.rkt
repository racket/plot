#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/55

(define (do-plot-polygons3d output-fn)
  (output-fn
   (polygons3d (list (list (list 1 0 0) (list 0 0 1) (list 0 1 0))
                     (list (list 0 0 0) (list 0 0 1) (list 0 1 0))
                     (list (list 1 0 0) (list 0 0 1) (list 0 0 0))))))

(define (do-plot-parametric-surface3d output-fn)
  (output-fn
   (parametric-surface3d (λ (ϕ φ)
                           (list (* (cos ϕ) (cos φ))
                                 (* (sin ϕ) (cos φ))
                                 (sin φ)))
                         0 (* 2 (angle -1)) #:s-samples 80
                         (- (/ (angle -1) 2)) (/ (angle -1) 2) #:t-samples 40)))

(define (do-plot-polar3d output-fn)
  (output-fn (polar3d (λ (ϕ φ) 1))))

(define-runtime-path pr55-polygons3d-data "./data/pr55-polygons3d-data.rktd")
(define-runtime-path pr55-parametric-surface3d-data "./data/pr55-parametric-suface3d-data.rktd")
(define-runtime-path pr55-polar3d-data "./data/pr55-parametric-polar3d-data.rktd")

(define pr55-test-suite
  (test-suite
   "PR#55: Parametric surface3d"
   (test-case "pr55-polygons3d"
     (check-draw-steps-3d do-plot-polygons3d pr55-polygons3d-data))
   (test-case "pr55-parametric-surface3d"
     (check-draw-steps-3d do-plot-parametric-surface3d pr55-parametric-surface3d-data))
   (test-case "pr55-polar3d"
     (check-draw-steps-3d do-plot-polar3d pr55-polar3d-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr55-test-suite))
