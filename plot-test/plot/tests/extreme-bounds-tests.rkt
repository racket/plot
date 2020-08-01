#lang racket

(require plot plot/utils math/flonum
         racket/runtime-path
         rackunit
         "helpers.rkt")

(module+ test
  (module config info
    (define timeout 150)))

(define-runtime-path eb-1-data "./test-data/eb-1.rktd")
(define (do-plot-1 output-fn)
  (output-fn (points '(#(0 0)))
             #:x-min +min.0 #:x-max (flstep +min.0 1000)
             #:y-min 0 #:y-max 1
             #:title "Point at 0,0"))

(define-runtime-path eb-2-data "./test-data/eb-2.rktd")
(define (do-plot-2 output-fn)
  (output-fn (points3d '(#(0 0 0)))
             #:x-min +min.0 #:x-max (flstep +min.0 1000)
             #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1
             #:title "Point at 0,0,0"))

(define-runtime-path eb-3-data "./test-data/eb-3.rktd")
(define (do-plot-3 output-fn)
  (output-fn (function fl (flstep 0.0 -1) (flstep 0.0 2))
             #:title "Steps should appear all the way from the bottom to the top"))

(define-runtime-path eb-4-data "./test-data/eb-4.rktd")
(define (do-plot-4 output-fn)
  (output-fn (surface3d (λ (x y) (fl x))
                        (flstep 0.0 -1) (flstep 0.0 2)
                        (flstep 0.0 -1) (flstep 0.0 2))
             #:title "Steps should appear all the way from the bottom to the top"))

(plot-x-label #f)
(plot-y-label #f)

(define stops (list (* 2 (inexact->exact -max.0))
                    -max.0 -min.0 0.0 +min.0 +max.0
                    (* 2 (inexact->exact +max.0))))

(define (extreme-real->string x)
  (real->plot-label x (digits-for-range 0 (abs x))))

(define-runtime-path data-dir "./test-data")

(define 2d-test-case-counter 0)
(define 2d-test-cases
  (test-suite
   "2d-test-cases"
   (for/list ([x-min  (in-list stops)]
              [x-max  (in-list (rest stops))]
              #:when #t
              [y-min  (in-list stops)]
              [y-max  (in-list (rest stops))])
     (set! 2d-test-case-counter (add1 2d-test-case-counter))
     (define name
       (format "[~a,~a] × [~a,~a]"
               (extreme-real->string x-min) (extreme-real->string x-max)
               (extreme-real->string y-min) (extreme-real->string y-max)))
     (define data-file (build-path data-dir (format "eb2d-~a.dat" 2d-test-case-counter)))
     (define (do-plot output-fn)
       (output-fn (lines (list (vector x-min y-min) (vector x-max y-max))) #:title name))
     (test-case name (check-draw-steps do-plot data-file)))))

(define 3d-test-case-counter 0)
(define 3d-test-cases
  (test-suite
   "3d-test-cases"
   (for/list ([x-min  (in-list stops)]
              [x-max  (in-list (rest stops))]
              #:when #t
              [y-min  (in-list stops)]
              [y-max  (in-list (rest stops))]
              #:when #t
              [z-min  (in-list stops)]
              [z-max  (in-list (rest stops))])
     (set! 3d-test-case-counter (add1 3d-test-case-counter))
     (define name
       (format "[~a,~a] × [~a,~a] × [~a,~a]"
               (extreme-real->string x-min) (extreme-real->string x-max)
               (extreme-real->string y-min) (extreme-real->string y-max)
               (extreme-real->string z-min) (extreme-real->string z-max)))
     (define data-file (build-path data-dir (format "eb3d-~a.dat" 3d-test-case-counter)))
     (define (do-plot output-fn)
       (output-fn (lines3d (list (vector x-min y-min z-min)
                                 (vector x-max y-max z-max)))
                  #:title name))
     (test-case name (check-draw-steps-3d do-plot data-file)))))

(define extreme-bounds-tests
  (test-suite
   "extreme-bounds-tests"
   (test-case "test case 1" (check-draw-steps do-plot-1 eb-1-data))
   (test-case "test case 2" (check-draw-steps-3d do-plot-2 eb-2-data))
   (test-case "test case 3" (check-draw-steps do-plot-3 eb-3-data))
   (test-case "test case 4" (check-draw-steps-3d do-plot-4 eb-4-data))
   2d-test-cases
   3d-test-cases))

(module+ test
  (require rackunit/text-ui)
  (run-tests extreme-bounds-tests))

