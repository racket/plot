#lang racket
(require plot)

(define r3d (surface3d (lambda (x y) (- (sqr x) (sqr y))) -1 1 -1 1))

;; Case 1: far ticks are the same as near ticks, labels are
;; drawn on the near axis only.
(printf "\
plot-{x,y,z}-ticks            - (linear-ticks)
plot-{x,y,z}-far-ticks        - (linear-ticks)
plot-{x,y,z}-tick-labels?     - NOT SET
plot-{x,y,z}-far-tick-labels? - NOT SET~%")
(parameterize ([plot-width    300]
               [plot-height   300]
               [plot-x-ticks (linear-ticks)]
               [plot-x-far-ticks (linear-ticks)]
               [plot-y-ticks (linear-ticks)]
               [plot-y-far-ticks (linear-ticks)]
               [plot-z-ticks (linear-ticks)]
               [plot-z-far-ticks (linear-ticks)])
  (list 
   (plot (function sin (- pi) pi))
   (plot3d r3d)))


;; Case 2: far ticks are the same as near ticks, but labels are
;; forced to be drawn on the far axis as well.
(printf "\
plot-{x,y,z}-ticks            - (linear-ticks)
plot-{x,y,z}-far-ticks        - (linear-ticks)
plot-{x,y,z}-tick-labels?     - NOT SET
plot-{x,y,z}-far-tick-labels? - #t~%")
(parameterize ([plot-width    300]
               [plot-height   300]
               [plot-x-far-tick-labels? #t]
               [plot-x-ticks (linear-ticks)]
               [plot-x-far-ticks (linear-ticks)]
               [plot-y-far-tick-labels? #t]
               [plot-y-ticks (linear-ticks)]
               [plot-y-far-ticks (linear-ticks)]
               [plot-z-far-tick-labels? #t]
               [plot-z-ticks (linear-ticks)]
               [plot-z-far-ticks (linear-ticks)])
 (list 
   (plot (function sin (- pi) pi))
   (plot3d r3d)))

;; Case 3: far ticks are the same as near ticks, labels are
;; disabled on the near axis, but will not be drawn on the far axis.
(printf "\
plot-{x,y,z}-ticks            - (linear-ticks)
plot-{x,y,z}-far-ticks        - (linear-ticks)
plot-{x,y,z}-tick-labels?     - #f
plot-{x,y,z}-far-tick-labels? - NOT SET~%")
(parameterize ([plot-width    300]
               [plot-height   300]
               [plot-x-tick-labels? #f]
               [plot-x-ticks (linear-ticks)]
               [plot-x-far-ticks (linear-ticks)]
               [plot-y-tick-labels? #f]
               [plot-y-ticks (linear-ticks)]
               [plot-y-far-ticks (linear-ticks)]
               [plot-z-tick-labels? #f]
               [plot-z-ticks (linear-ticks)]
               [plot-z-far-ticks (linear-ticks)])
 (list 
   (plot (function sin (- pi) pi))
   (plot3d r3d)))

;; Case 4: no near ticks, only far ticks.  labels are drawn on the far ticks
(printf "\
plot-{x,y,z}-ticks            - no-ticks
plot-{x,y,z}-far-ticks        - (linear-ticks)
plot-{x,y,z}-tick-labels?     - NOT SET
plot-{x,y,z}-far-tick-labels? - NOT SET~%")
(parameterize ([plot-width    300]
               [plot-height   300]
               [plot-x-ticks no-ticks]
               [plot-x-far-ticks (linear-ticks)]
               [plot-y-ticks no-ticks]
               [plot-y-far-ticks (linear-ticks)]
               [plot-z-ticks no-ticks]
               [plot-z-far-ticks (linear-ticks)])
  (list 
   (plot (function sin (- pi) pi))
   (plot3d r3d)))

;; Case 5: no near ticks, only far ticks, but disable drawing labels on the far ticks
(printf "\
plot-{x,y,z}-ticks            - no-ticks
plot-{x,y,z}-far-ticks        - (linear-ticks)
plot-{x,y,z}-tick-labels?     - NOT SET
plot-{x,y,z}-far-tick-labels? - #f~%")
(parameterize ([plot-width    300]
               [plot-height   300]
               [plot-x-ticks no-ticks]
               [plot-x-far-tick-labels? #f]
               [plot-x-far-ticks (linear-ticks)]
               [plot-y-ticks no-ticks]
               [plot-y-far-tick-labels? #f]
               [plot-y-far-ticks (linear-ticks)]
               [plot-z-ticks no-ticks]
               [plot-z-far-tick-labels? #f]
               [plot-z-far-ticks (linear-ticks)])
  (list 
   (plot (function sin (- pi) pi))
   (plot3d r3d)))

;; Case 6: far ticks are the same as near ticks, switch to drawing labels on the
;; far axis
(printf "\
plot-{x,y,z}-ticks            - (linear-ticks)
plot-{x,y,z}-far-ticks        - (linear-ticks)
plot-{x,y,z}-tick-labels?     - #f
plot-{x,y,z}-far-tick-labels? - #t~%")
(parameterize ([plot-width    300]
               [plot-height   300]
               [plot-x-tick-labels? #f]
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
  (list 
   (plot (function sin (- pi) pi))
   (plot3d r3d)))


