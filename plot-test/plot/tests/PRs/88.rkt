#lang racket

(require rackunit
         plot racket/runtime-path
         "../helpers.rkt")

;; Tests for https://github.com/racket/plot/pull/88, aspect ratio function
;; testing.

(plot-background "lightblue")
(plot-x-label #f)
(plot-y-label #f)
(plot-aspect-ratio #f)

(define (do-plot-no-ar output-fn)
  (output-fn (polar (lambda (t) 1))
             #:width 300 #:height 150
             #:title "No Aspect Ratio"))

(define-runtime-path pr88-no-ar-data "./test-data/pr88-no-ar.dat")

(define (do-plot-1610-ar output-fn)
  (output-fn (polar (lambda (t) 1))
             #:aspect-ratio 16/10
             #:width 300 #:height 150
             #:title "16:10 Aspect Ratio"))

(define-runtime-path pr88-1610-ar-data "./test-data/pr88-1610-ar.dat")

(define (do-plot-11-ar output-fn)
 (output-fn (polar (lambda (t) 1))
            #:aspect-ratio 1/1
            #:width 300 #:height 150
            #:title "1:1 Aspect Ratio"))

(define-runtime-path pr88-11-ar-data "./test-data/pr88-11-ar.dat")

(define (do-plot-11-ar-portrait output-fn)
  (output-fn (polar (lambda (t) 1))
             #:aspect-ratio 1/1
             #:width 150 #:height 300
             #:title "1:1 Aspect Ratio"))

(define-runtime-path pr88-11-ar-portrait-data "./test-data/pr88-11-ar-portrait.dat")

(define (do-plot3d-no-ar output-fn)
  (output-fn (polar3d (lambda (a b) 1))
             #:width 300 #:height 150
             #:title "No Aspect Ratio"))

(define-runtime-path pr88-3d-no-ar-data "./test-data/pr88-3d-no-ar.dat")

(define (do-plot3d-1610-ar output-fn)
  (output-fn (polar3d (lambda (a b) 1))
             #:aspect-ratio 16/10
             #:width 300 #:height 150
             #:title "16:10 Aspect Ratio"))

(define-runtime-path pr88-3d-1610-ar-data "./test-data/pr88-3d-1610-ar.dat")

(define (do-plot3d-11-ar output-fn)
  (output-fn (polar3d (lambda (a b) 1))
             #:aspect-ratio 1/1
             #:width 300 #:height 150
             #:title "1:1 Aspect Ratio"))

(define-runtime-path pr88-3d-11-ar-data "./test-data/pr88-3d-11-ar.dat")

(define (do-plot3d-11-ar-portrait output-fn)
  (output-fn (polar3d (lambda (a b) 1))
             #:aspect-ratio 1/1
             #:width 150 #:height 300
             #:title "1:1 Aspect Ratio"))

(define-runtime-path pr88-3d-11-ar-portrait-data "./test-data/pr88-3d-11-ar-portrait.dat")

(define pr88-test-suite
  (test-suite
   "PR#88: add support for aspect-ratio"
   (test-case "No Aspect Ratio"
     (check-draw-steps do-plot-no-ar pr88-no-ar-data))
   (test-case "16:10 Aspect Ratio"
     (check-draw-steps do-plot-1610-ar pr88-1610-ar-data))
   (test-case "1:1 Aspect Ratio"
     (check-draw-steps do-plot-11-ar pr88-11-ar-data))
   (test-case "1:1 Aspect Ratio Portrait"
     (check-draw-steps do-plot-11-ar-portrait pr88-11-ar-portrait-data))
   (test-case "No Aspect Ratio"
     (check-draw-steps-3d do-plot3d-no-ar pr88-3d-no-ar-data))
   (test-case "16:10 Aspect Ratio"
     (check-draw-steps-3d do-plot3d-1610-ar pr88-3d-1610-ar-data))
   (test-case "1:1 Aspect Ratio"
     (check-draw-steps-3d do-plot3d-11-ar pr88-3d-11-ar-data))
   (test-case "1:1 Aspect Ratio Portrait"
     (check-draw-steps-3d do-plot3d-11-ar-portrait pr88-3d-11-ar-portrait-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr88-test-suite))
