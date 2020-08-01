#lang racket
(require rackunit
         plot
         plot/utils
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for https://github.com/racket/plot/issues/46

(define r3d (surface3d (lambda (x y) (- (sqr x) (sqr y))) -1 1 -1 1))
(define r2d (function sin (- pi) pi))

;; Case 1: far ticks are the same as near ticks, labels are
;; drawn on the near axis only.
;;
;;     plot-{x,y,z}-ticks            - (linear-ticks)
;;     plot-{x,y,z}-far-ticks        - (linear-ticks)
;;     plot-{x,y,z}-tick-labels?     - NOT SET
;;     plot-{x,y,z}-far-tick-labels? - NOT SET

(define (do-plot-case1 output-fn)
  (parameterize ([plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r2d)))

(define (do-plot-case1-3d output-fn)
  (parameterize ([plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r3d)))

;; Case 2: far ticks are the same as near ticks, but labels are
;; forced to be drawn on the far axis as well.
;;
;;     plot-{x,y,z}-ticks            - (linear-ticks)
;;     plot-{x,y,z}-far-ticks        - (linear-ticks)
;;     plot-{x,y,z}-tick-labels?     - NOT SET
;;     plot-{x,y,z}-far-tick-labels? - #t

(define (do-plot-case2 output-fn)
  (parameterize ([plot-x-far-tick-labels? #t]
                 [plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-far-tick-labels? #t]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-far-tick-labels? #t]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r2d)))

(define (do-plot-case2-3d output-fn)
  (parameterize ([plot-x-far-tick-labels? #t]
                 [plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-far-tick-labels? #t]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-far-tick-labels? #t]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r3d)))

;; Case 3: far ticks are the same as near ticks, labels are
;; disabled on the near axis, but will not be drawn on the far axis.
;;
;;     plot-{x,y,z}-ticks            - (linear-ticks)
;;     plot-{x,y,z}-far-ticks        - (linear-ticks)
;;     plot-{x,y,z}-tick-labels?     - #f
;;     plot-{x,y,z}-far-tick-labels? - NOT SET

(define (do-plot-case3 output-fn)
  (parameterize ([plot-x-tick-labels? #f]
                 [plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-tick-labels? #f]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-tick-labels? #f]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r2d)))

(define (do-plot-case3-3d output-fn)
  (parameterize ([plot-x-tick-labels? #f]
                 [plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-tick-labels? #f]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-tick-labels? #f]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r3d)))

;; Case 4: no near ticks, only far ticks.  labels are drawn on the far ticks
;;
;;     plot-{x,y,z}-ticks            - no-ticks
;;     plot-{x,y,z}-far-ticks        - (linear-ticks)
;;     plot-{x,y,z}-tick-labels?     - NOT SET
;;     plot-{x,y,z}-far-tick-labels? - NOT SET

(define (do-plot-case4 output-fn)
  (parameterize ([plot-x-ticks no-ticks]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-ticks no-ticks]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-ticks no-ticks]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r2d)))

(define (do-plot-case4-3d output-fn)
  (parameterize ([plot-x-ticks no-ticks]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-ticks no-ticks]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-ticks no-ticks]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r3d)))

;; Case 5: no near ticks, only far ticks, but disable drawing labels on the far ticks
;;
;; plot-{x,y,z}-ticks            - no-ticks
;; plot-{x,y,z}-far-ticks        - (linear-ticks)
;; plot-{x,y,z}-tick-labels?     - NOT SET
;; plot-{x,y,z}-far-tick-labels? - #f

(define (do-plot-case5 output-fn)
  (parameterize ([plot-x-ticks no-ticks]
                 [plot-x-far-tick-labels? #f]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-ticks no-ticks]
                 [plot-y-far-tick-labels? #f]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-ticks no-ticks]
                 [plot-z-far-tick-labels? #f]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r2d)))

(define (do-plot-case5-3d output-fn)
  (parameterize ([plot-x-ticks no-ticks]
                 [plot-x-far-tick-labels? #f]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-ticks no-ticks]
                 [plot-y-far-tick-labels? #f]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-ticks no-ticks]
                 [plot-z-far-tick-labels? #f]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r3d)))

;; Case 6: far ticks are the same as near ticks, switch to drawing labels on the
;; far axis
;;
;;     plot-{x,y,z}-ticks            - (linear-ticks)
;;     plot-{x,y,z}-far-ticks        - (linear-ticks)
;;     plot-{x,y,z}-tick-labels?     - #f
;;     plot-{x,y,z}-far-tick-labels? - #t
(define (do-plot-case6 output-fn)
  (parameterize ([plot-x-tick-labels? #f]
                 [plot-x-far-tick-labels? #t]
                 [plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-tick-labels? #f]
                 [plot-y-far-tick-labels? #t]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-tick-labels? #f]
                 [plot-z-far-tick-labels? #t]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r2d)))

(define (do-plot-case6-3d output-fn)
  (parameterize ([plot-x-tick-labels? #f]
                 [plot-x-far-tick-labels? #t]
                 [plot-x-ticks (linear-ticks)]
                 [plot-x-far-ticks (linear-ticks)]
                 [plot-y-tick-labels? #f]
                 [plot-y-far-tick-labels? #t]
                 [plot-y-ticks (linear-ticks)]
                 [plot-y-far-ticks (linear-ticks)]
                 [plot-z-tick-labels? #f]
                 [plot-z-far-tick-labels? #t]
                 [plot-z-ticks (linear-ticks)]
                 [plot-z-far-ticks (linear-ticks)])
    (output-fn r3d)))

(define-runtime-path pr46-case1-data "./test-data/pr46-1.dat")
(define-runtime-path pr46-case1-3d-data "./test-data/pr46-1-3d.dat")
(define-runtime-path pr46-case2-data "./test-data/pr46-2.dat")
(define-runtime-path pr46-case2-3d-data "./test-data/pr46-2-3d.dat")
(define-runtime-path pr46-case3-data "./test-data/pr46-3.dat")
(define-runtime-path pr46-case3-3d-data "./test-data/pr46-3-3d.dat")
(define-runtime-path pr46-case4-data "./test-data/pr46-4.dat")
(define-runtime-path pr46-case4-3d-data "./test-data/pr46-4-3d.dat")
(define-runtime-path pr46-case5-data "./test-data/pr46-5.dat")
(define-runtime-path pr46-case5-3d-data "./test-data/pr46-5-3d.dat")
(define-runtime-path pr46-case6-data "./test-data/pr46-6.dat")
(define-runtime-path pr46-case6-3d-data "./test-data/pr46-6-3d.dat")

(define pr46-test-suite
  (test-suite
   "PR#46: far tick labels drawn when parameter is #false"
   (test-case "pr46 case1"
     (check-draw-steps do-plot-case1 pr46-case1-data))
   (test-case "pr46 case1 3d"
     (check-draw-steps-3d do-plot-case1-3d pr46-case1-3d-data))
   (test-case "pr46 case2"
     (check-draw-steps do-plot-case2 pr46-case2-data))
   (test-case "pr46 case2 3d"
     (check-draw-steps-3d do-plot-case2-3d pr46-case2-3d-data))
   (test-case "pr46 case3"
     (check-draw-steps do-plot-case3 pr46-case3-data))
   (test-case "pr46 case3 3d"
     (check-draw-steps-3d do-plot-case3-3d pr46-case3-3d-data))
   (test-case "pr46 case4"
     (check-draw-steps do-plot-case4 pr46-case4-data))
   (test-case "pr46 case4 3d"
     (check-draw-steps-3d do-plot-case4-3d pr46-case4-3d-data))
   (test-case "pr46 case5"
     (check-draw-steps do-plot-case5 pr46-case5-data))
   (test-case "pr46 case5 3d"
     (check-draw-steps-3d do-plot-case5-3d pr46-case5-3d-data))
   (test-case "pr46 case6"
     (check-draw-steps do-plot-case6 pr46-case6-data))
   (test-case "pr46 case6 3d"
     (check-draw-steps-3d do-plot-case6-3d pr46-case6-3d-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr46-test-suite))
