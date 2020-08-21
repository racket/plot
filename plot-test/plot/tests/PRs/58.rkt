#lang racket
(require rackunit
         plot
         pict
         racket/draw
         racket/runtime-path
         "../helpers.rkt")

;; Tests for https://github.com/racket/plot/issues/58

;; Setup a draw context used for font metrics by the picts used in these tests
;; -- this will ensure that tests produce identical data sets on all
;; platforms.  Note that this line will make the pict look unaligned -- remove
;; this line to restore "normal" behavior.

(dc-for-text-size (new mock-record-dc%))

(define (do-plot-2d output-fn)
  (output-fn
   (list
    (lines '([10 1] [20 1.5] [30 1.7])
           #:color "DarkOrange"
           #:width 3
           #:label (vc-append (standard-fish 35 25 #:color "DarkOrange") (text "One Fish")))
    (points '([10 1.2] [15 1.0] [20 0.9] [25 0.7] [30 0.4])
            #:color "forestgreen"
            #:fill-color "medium forest green"
            #:line-width 3
            #:sym 'fullcircle1
            #:size 20
            #:label (vc-append (hc-append
                                (standard-fish 25 15 #:color "forestgreen")
                                (standard-fish 25 15 #:color "medium forest green"))
                               (text "Two Fish")))
    (lines '([10 .5] [20 1.6] [30 1.0])
           #:color "firebrick"
           #:width 3
           #:label (vc-append (standard-fish 50 30 #:color "firebrick")
                              (text "Red Fish")))
    (function sin 10 30
              #:color "steel blue"
              #:width 3
              #:label (vc-append (standard-fish 30 50 #:color "steel blue")
                                 (text "Blue Fish")))
    
    )))

(define (do-plot-3d output-fn)
  (output-fn
   (surface3d (λ (x y) (+ (/ 1.0 x))
                 (/ 1.0 y))
              -2 2 -2 2
              #:color '(255 128 128)
              #:line-color '(255 128 128)
              #:line-width 1.5
              #:label (standard-fish 50 30))))


(define (do-plot-2d-title+axes output-fn)
  (parameterize ((plot-title (hc-append 15
                                        (standard-fish 50 30 #:color "DarkGoldenRod")
                                        (text "Fishy Plot" null 24)
                                        (standard-fish 50 30 #:color "DarkGoldenRod")))
                 (plot-x-label (standard-fish 75 30 #:color "RoyalBlue"))
                 (plot-x-far-label (standard-fish 75 30 #:color "Teal"))
                 (plot-y-label (standard-fish 75 30 #:color "Coral"))
                 (plot-y-far-label (standard-fish 75 30 #:color "DarkSalmon"))
                 (plot-z-label (standard-fish 75 30 #:color "DarkOrchid"))
                 (plot-z-far-label (standard-fish 75 30 #:color "Plum")))
    (output-fn (function sin -3 3))))

(define (do-plot-3d-title+axes output-fn)
  (parameterize ((plot-title (hc-append 15
                                        (standard-fish 50 30 #:color "DarkGoldenRod")
                                        (text "Fishy Plot" null 24)
                                        (standard-fish 50 30 #:color "DarkGoldenRod")))
                 (plot-x-label (standard-fish 75 30 #:color "RoyalBlue"))
                 (plot-x-far-label (standard-fish 75 30 #:color "Teal"))
                 (plot-y-label (standard-fish 75 30 #:color "Coral"))
                 (plot-y-far-label (standard-fish 75 30 #:color "DarkSalmon"))
                 (plot-z-label (standard-fish 75 30 #:color "DarkOrchid"))
                 (plot-z-far-label (standard-fish 75 30 #:color "Plum")))
    (output-fn (surface3d (λ (x y) (+ (/ 1.0 x))
                             (/ 1.0 y))
                          -2 2 -2 2
                          #:color '(255 128 128)
                          #:line-color '(255 128 128)
                          #:line-width 1.5))))

(define-runtime-path pr58-2d "./test-data/pr58-2d.dat")
(define-runtime-path pr58-3d "./test-data/pr58-3d.dat")
(define-runtime-path pr58-2d-t "./test-data/pr58-2d-t.dat")
(define-runtime-path pr58-3d-t "./test-data/pr58-3d-t.dat")

(define pr58-test-suite
  (test-suite
   "PR#58: add support for pict legend entries, title and axes"
   (test-case "pr58 2d plots"
     (check-draw-steps do-plot-2d pr58-2d))
   (test-case "pr58 3d plots"
     (check-draw-steps-3d do-plot-3d pr58-3d))
   (test-case "pr58 2d plots title+axes"
     (check-draw-steps do-plot-2d-title+axes pr58-2d-t))
   (test-case "pr58 3d plots title+axes"
     (check-draw-steps-3d do-plot-3d-title+axes pr58-3d-t))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr58-test-suite))

                    
