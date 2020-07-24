#lang racket
(require rackunit
         plot
         plot/utils
         racket/draw
         racket/runtime-path
         "helpers.rkt")

;; NOTE: these plots used to test some `plot2d-subdivisions` and
;; `plot3d-subdivisions` parameters, but, while these parameters are defined,
;; but are not used by the plot package -- their usage was commented out and
;; disappeared over time.  I suspect they were used during debugging to
;; determine how many subdivisions are done for the plot, as there is still
;; one commented out place where the plot3d-subdivisions is incremented (an
;; equivalent commented out increment also existed for `plot2d-subdivisions`
;;
;; Now, these plots just exercise the plot library in interesting ways.

(define (do-plot1 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-x-ticks      (log-ticks)])
    (output-fn (lines '(#(1 1) #(200 200))))))

(define (do-plot2 output-fn)
  (parameterize ([plot-x-transform  log-transform]
                 [plot-x-ticks      (log-ticks)])
    (output-fn (lines3d '(#(1 1 1) #(200 200 200))))))

(define (do-plot3 output-fn)
  (parameterize ([plot3d-samples 4]
                 [plot-x-transform  log-transform]
                 [plot-x-ticks      (log-ticks)]
                 [plot-y-transform  log-transform]
                 [plot-y-ticks      (log-ticks)])
    (output-fn (lines '(#(1 1) #(200 200))))))

(define (do-plot4 output-fn)
  (parameterize ([plot3d-samples 4]
                 [plot-x-transform  log-transform]
                 [plot-x-ticks      (log-ticks)]
                 [plot-y-transform  log-transform]
                 [plot-y-ticks      (log-ticks)])
    (output-fn (surface3d + 1 200 1 200))))

(define (do-plot5 output-fn) 
  (parameterize ([plot-x-transform  (collapse-transform -1 1)])
    (output-fn (lines '(#(-2 -2) #(2 2))))))

(define (do-plot6 output-fn) 
  (parameterize ([plot-x-transform  (collapse-transform -1 1)])
    (output-fn (surface3d + -2 2 -2 2))))

(define-runtime-path subdivision1-data "./test-data/sd-1.dat")
(define-runtime-path subdivision2-data "./test-data/sd-2.dat")
(define-runtime-path subdivision3-data "./test-data/sd-3.dat")
(define-runtime-path subdivision4-data "./test-data/sd-4.dat")
(define-runtime-path subdivision5-data "./test-data/sd-5.dat")
(define-runtime-path subdivision6-data "./test-data/sd-6.dat")

(define subdivision-tests
  (test-suite
   "subdivision-tests"
   (test-case "test case 1" (check-draw-steps do-plot1 subdivision1-data))
   (test-case "test case 2" (check-draw-steps-3d do-plot2 subdivision2-data))
   (test-case "test case 3" (check-draw-steps do-plot3 subdivision3-data))
   (test-case "test case 4" (check-draw-steps-3d do-plot4 subdivision4-data))
   (test-case "test case 5" (check-draw-steps do-plot5 subdivision5-data))
   (test-case "test case 6" (check-draw-steps-3d do-plot6 subdivision6-data))))
  
(module+ test
  (require rackunit/text-ui)
  (run-tests subdivision-tests))
