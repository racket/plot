#lang racket
(require plot)

;; Case 1: far ticks are the same as near ticks, labels are
;; drawn on the near axis only.
(printf "Labels on near axis only~%")
(parameterize ([plot-width    150]
               [plot-height   150]
               [plot-y-ticks (linear-ticks)]
               [plot-y-far-ticks (linear-ticks)])
  (plot (function sin (- pi) pi)))


;; Case 2: far ticks are the same as near ticks, but labels are
;; forced to be drawn on the far axis as well.
(printf "Labels on both near and far axes~%")
(parameterize ([plot-width    150]
               [plot-height   150]
               [plot-y-far-tick-labels? #t]
               [plot-y-ticks (linear-ticks)]
               [plot-y-far-ticks (linear-ticks)])
  (plot (function sin (- pi) pi)))

;; Case 3: far ticks are the same as near ticks, labels are
;; disabled on the near axis, but will not be drawn on the far axis.
(printf "Ticks on near and far axies, no labels at all~%")
(parameterize ([plot-width    150]
               [plot-height   150]
               [plot-y-tick-labels? #f]
               [plot-y-ticks (linear-ticks)]
               [plot-y-far-ticks (linear-ticks)])
  (plot (function sin (- pi) pi)))

;; Case 4: no near ticks, only far ticks.  labels are drawn on the far ticks
(printf "Labels on the far axis only, no ticks on the near axis~%")
(parameterize ([plot-width    150]
               [plot-height   150]
               [plot-y-ticks no-ticks]
               [plot-y-far-ticks (linear-ticks)])
  (plot (function sin (- pi) pi)))

;; Case 5: no near ticks, only far ticks, but disable drawing labels on the far ticks
(printf "No ticks on near axis, ticks on far axis, no labels on the far axis~%")
(parameterize ([plot-width    150]
               [plot-height   150]
               [plot-y-ticks no-ticks]
               [plot-y-far-tick-labels? #f]
               [plot-y-far-ticks (linear-ticks)])
  (plot (function sin (- pi) pi)))

;; Case 6: far ticks are the same as near ticks, switch to drawing labels on the
;; far axis
(printf "Same ticks on near and far axis, labels on far axis only~%")
(parameterize ([plot-width    150]
               [plot-height   150]
               [plot-y-tick-labels? #f]
               [plot-y-far-tick-labels? #t]
               [plot-y-ticks (linear-ticks)]
               [plot-y-far-ticks (linear-ticks)])
  (plot (function sin (- pi) pi)))
