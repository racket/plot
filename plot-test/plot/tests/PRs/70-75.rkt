#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         pict
         "../helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/70, 71, 72, 73, 74

(define rendertree2d (list (function values 0 1 #:label "fct" #:color 'blue)
                         (contour-intervals (λ (x y) (* x y)) 1 2 #:label "cti")))

(define rendertree3d (list (surface3d (λ (x y) 1) 0 1 0 1 #:label "srf")
                           (contour-intervals3d (λ (x y) (* x y)) 1 2 1 2 #:label "cti")))


(define-runtime-path pr70/75-2d-nl "./test-data/pr70-2d-no-legend.dat")
(define (do-plot-2d-nl output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'no-legend
             #:title "no-legend"))

(define-runtime-path pr70/75-2d-otl "./test-data/pr70-2d-outside-top-left.dat")
(define (do-plot-2d-otl output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-top-left
             #:title "outside-top-left"))

(define-runtime-path pr70/75-2d-ot "./test-data/pr70-2d-outside-top.dat")
(define (do-plot-2d-ot output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-top
             #:title "outside-top"))

(define-runtime-path pr70/75-2d-otr "./test-data/pr70-2d-outside-top-right.dat")
(define (do-plot-2d-otr output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-top-right
             #:title "outside-top-right"))

(define-runtime-path pr70/75-2d-olt "./test-data/pr70-2d-outside-left-top.dat")
(define (do-plot-2d-olt output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-left-top
             #:title "outside-left-top"))

(define-runtime-path pr70/75-2d-ol "./test-data/pr70-2d-outside-left.dat")
(define (do-plot-2d-ol output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-left
             #:title "outside-left"))

(define-runtime-path pr70/75-2d-olb "./test-data/pr70-2d-outside-left-bottom.dat")
(define (do-plot-2d-olb output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-left-bottom
             #:title "outside-left-bottom"))

(define-runtime-path pr70/75-2d-ort "./test-data/pr70-2d-outside-right-top.dat")
(define (do-plot-2d-ort output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-right-top
             #:title "outside-right-top"))

(define-runtime-path pr70/75-2d-or "./test-data/pr70-2d-outside-right.dat")
(define (do-plot-2d-or output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-right
             #:title "outside-right"))

(define-runtime-path pr70/75-2d-orb "./test-data/pr70-2d-outside-right-bottom.dat")
(define (do-plot-2d-orb output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-right-bottom
             #:title "outside-right-bottom"))

(define-runtime-path pr70/75-2d-obl "./test-data/pr70-2d-outside-bottom-left.dat")
(define (do-plot-2d-obl output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-bottom-left
             #:title "outside-bottom-left"))

(define-runtime-path pr70/75-2d-ob "./test-data/pr70-2d-outside-bottom.dat")
(define (do-plot-2d-ob output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-bottom
             #:title "outside-bottom"))

(define-runtime-path pr70/75-2d-obr "./test-data/pr70-2d-outside-bottom-right.dat")
(define (do-plot-2d-obr output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-bottom-right
             #:title "outside-bottom-right"))

(define-runtime-path pr70/75-2d-ogt "./test-data/pr70-2d-outside-global-top.dat")
(define (do-plot-2d-ogt output-fn)
  (output-fn rendertree2d
             #:legend-anchor 'outside-global-top
             #:title "outside-global-top"))

(define-runtime-path pr70/75-3d-nl "./test-data/pr70-3d-no-legend.dat")
(define (do-plot-3d-nl output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'no-legend
             #:title "no-legend"))

(define-runtime-path pr70/75-3d-otl "./test-data/pr70-3d-outside-top-left.dat")
(define (do-plot-3d-otl output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-top-left
             #:title "outside-top-left"))

(define-runtime-path pr70/75-3d-ot "./test-data/pr70-3d-outside-top.dat")
(define (do-plot-3d-ot output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-top
             #:title "outside-top"))

(define-runtime-path pr70/75-3d-otr "./test-data/pr70-3d-outside-top-right.dat")
(define (do-plot-3d-otr output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-top-right
             #:title "outside-top-right"))

(define-runtime-path pr70/75-3d-olt "./test-data/pr70-3d-outside-left-top.dat")
(define (do-plot-3d-olt output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-left-top
             #:title "outside-left-top"))

(define-runtime-path pr70/75-3d-ol "./test-data/pr70-3d-outside-left.dat")
(define (do-plot-3d-ol output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-left
             #:title "outside-left"))

(define-runtime-path pr70/75-3d-olb "./test-data/pr70-3d-outside-left-bottom.dat")
(define (do-plot-3d-olb output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-left-bottom
             #:title "outside-left-bottom"))

(define-runtime-path pr70/75-3d-ort "./test-data/pr70-3d-outside-right-top.dat")
(define (do-plot-3d-ort output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-right-top
             #:title "outside-right-top"))

(define-runtime-path pr70/75-3d-or "./test-data/pr70-3d-outside-right.dat")
(define (do-plot-3d-or output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-right
             #:title "outside-right"))

(define-runtime-path pr70/75-3d-orb "./test-data/pr70-3d-outside-right-bottom.dat")
(define (do-plot-3d-orb output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-right-bottom
             #:title "outside-right-bottom"))

(define-runtime-path pr70/75-3d-obl "./test-data/pr70-3d-outside-bottom-left.dat")
(define (do-plot-3d-obl output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-bottom-left
             #:title "outside-bottom-left"))

(define-runtime-path pr70/75-3d-ob "./test-data/pr70-3d-outside-bottom.dat")
(define (do-plot-3d-ob output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-bottom
             #:title "outside-bottom"))

(define-runtime-path pr70/75-3d-obr "./test-data/pr70-3d-outside-bottom-right.dat")
(define (do-plot-3d-obr output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-bottom-right
             #:title "outside-bottom-right"))

(define-runtime-path pr70/75-3d-ogt "./test-data/pr70-3d-outside-global-top.dat")
(define (do-plot-3d-ogt output-fn)
  (output-fn (cons (points3d '((0 1 2)) #:label "an extra really really really long label")
                   rendertree3d)
             #:legend-anchor 'outside-global-top
             #:title "outside-global-top"))

(define pr70-test-suite
  (test-suite
   "PR#70-75: outside legend placements"
   (test-case "2d no-legend" (check-draw-steps do-plot-2d-nl pr70/75-2d-nl))

   (test-case "2d outside-top-left" (check-draw-steps do-plot-2d-otl pr70/75-2d-otl))
   (test-case "2d outside-top" (check-draw-steps do-plot-2d-ot pr70/75-2d-ot))
   (test-case "2d outside-top-right" (check-draw-steps do-plot-2d-otr pr70/75-2d-otr))

   (test-case "2d outside-left-top" (check-draw-steps do-plot-2d-olt pr70/75-2d-olt))
   (test-case "2d outside-left" (check-draw-steps do-plot-2d-ol pr70/75-2d-ol))
   (test-case "2d outside-left-bottom" (check-draw-steps do-plot-2d-olb pr70/75-2d-olb))

   (test-case "2d outside-right-top" (check-draw-steps do-plot-2d-ort pr70/75-2d-ort))
   (test-case "2d outside-right" (check-draw-steps do-plot-2d-or pr70/75-2d-or))
   (test-case "2d outside-right-bottom" (check-draw-steps do-plot-2d-orb pr70/75-2d-orb))

   (test-case "2d outside-bottom-left" (check-draw-steps do-plot-2d-obl pr70/75-2d-obl))
   (test-case "2d outside-bottom" (check-draw-steps do-plot-2d-ob pr70/75-2d-ob))
   (test-case "2d outside-bottom-right" (check-draw-steps do-plot-2d-obr pr70/75-2d-obr))

   (test-case "2d outside-global-top" (check-draw-steps do-plot-2d-ogt pr70/75-2d-ogt))

   (test-case "3d no-legend" (check-draw-steps-3d do-plot-3d-nl pr70/75-3d-nl))
   (test-case "3d outside-top-left" (check-draw-steps-3d do-plot-3d-otl pr70/75-3d-otl))
   (test-case "3d outside-top" (check-draw-steps-3d do-plot-3d-ot pr70/75-3d-ot))
   (test-case "3d outside-top-right" (check-draw-steps-3d do-plot-3d-otr pr70/75-3d-otr))

   (test-case "3d outside-left-top" (check-draw-steps-3d do-plot-3d-olt pr70/75-3d-olt))
   (test-case "3d outside-left" (check-draw-steps-3d do-plot-3d-ol pr70/75-3d-ol))
   (test-case "3d outside-left-bottom" (check-draw-steps-3d do-plot-3d-olb pr70/75-3d-olb))

   (test-case "3d outside-right-top" (check-draw-steps-3d do-plot-3d-ort pr70/75-3d-ort))
   (test-case "3d outside-right" (check-draw-steps-3d do-plot-3d-or pr70/75-3d-or))
   (test-case "3d outside-right-bottom" (check-draw-steps-3d do-plot-3d-orb pr70/75-3d-orb))

   (test-case "3d outside-bottom-left" (check-draw-steps-3d do-plot-3d-obl pr70/75-3d-obl))
   (test-case "3d outside-bottom" (check-draw-steps-3d do-plot-3d-ob pr70/75-3d-ob))
   (test-case "3d outside-bottom-right" (check-draw-steps-3d do-plot-3d-obr pr70/75-3d-obr))

   (test-case "3d outside-global-top" (check-draw-steps-3d do-plot-3d-ogt pr70/75-3d-ogt))

   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr70-test-suite))
