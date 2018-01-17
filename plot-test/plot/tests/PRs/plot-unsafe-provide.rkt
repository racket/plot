#lang racket/base

;; Check that bad arguments to plot functions do not cause segfaults.
;;  (since these functions are exported via `unsafe-provide`)
;;
;; To run these tests:
;; 1. `raco test <THIS-FILE>`
;; 2. uncomment the code at the bottom of this file,
;;    run `racket <THIS-FILE>`
;;    check that the tests output contract violations --- nothing worse
;;
;; It would be good to eliminate step 2, but rackunit and `with-handlers` don't
;; catch the exceptions.

(require plot rackunit)

(define BAD-ARG ;; none of the plot functions expect a box
  (box #f))

(define example-renderer2
  (points (for/list ((i (in-range 0 4))) (list 0 i)) #:label "some points"))

(define example-nonrenderer2
  (invisible-rect -5 5 -5 5))

(define example-renderer3
  (vector-field3d (λ (x y z) (vector x z y)) -2 2 -2 2 -2 2))

(define example-nonrenderer3
  (invisible-rect3d -5 5 -5 5 -5 5))

;; -----------------------------------------------------------------------------
;; These tests should all pass

(module+ test
  (test-case "plot-snip"
    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 BAD-ARG))))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:x-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:x-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:y-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:y-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:width BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:height BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:title BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:x-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:y-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-snip (list example-renderer2 example-nonrenderer2)
                   #:legend-anchor BAD-ARG))))

  (test-case "plot-frame"
    (check-exn exn:fail:contract?
      (lambda ()
        (plot-frame (list example-renderer2 example-nonrenderer2)
                    #:x-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-frame (list example-renderer2 example-nonrenderer2)
                    #:x-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-frame (list example-renderer2 example-nonrenderer2)
                    #:y-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-frame (list example-renderer2 example-nonrenderer2)
                    #:y-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-frame (list example-renderer2 example-nonrenderer2)
                    #:width BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-frame (list example-renderer2 example-nonrenderer2)
                    #:height BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot-frame (list example-renderer2 example-nonrenderer2)
                    #:legend-anchor BAD-ARG))))

  (test-case "plot"
    (check-exn exn:fail:contract?
      (lambda ()
        (plot BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:x-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:x-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:y-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:y-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:width BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:height BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:title BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:x-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:y-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:legend-anchor BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:out-file BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:out-kind BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:fgcolor BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:bgcolor BAD-ARG)))

    (check-not-exn ;; unused argument
      (lambda ()
        (plot (list example-renderer2 example-nonrenderer2)
              #:lncolor BAD-ARG))))

  (test-case "plot3d-snip"
    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 BAD-ARG))))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:x-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:x-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:y-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:y-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:z-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:z-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:width BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:height BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:angle BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:altitude BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:title BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:x-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:y-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:z-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-snip (list example-renderer3 example-nonrenderer3)
                     #:legend-anchor BAD-ARG))))

  (test-case "plot3d-frame"
    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:x-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:x-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:y-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:y-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:z-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:z-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:width BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d-frame (list example-renderer3 example-nonrenderer3)
                     #:height BAD-ARG))))

  (test-case "plot3d"
    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:x-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:x-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:y-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:y-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:z-min BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:z-max BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:width BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:height BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:angle BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:altitude BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:az BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:alt BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:title BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:x-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:y-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:z-label BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:legend-anchor BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:out-file BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:out-kind BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:fgcolor BAD-ARG)))

    (check-exn exn:fail:contract?
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:bgcolor BAD-ARG)))

    (check-not-exn ;; unused argument
      (lambda ()
        (plot3d (list example-renderer3 example-nonrenderer3)
                     #:lncolor BAD-ARG))))

)

;; -----------------------------------------------------------------------------
;; When uncommented, these expressions should raise contract error messages
;; that blame `BAD-ARG` (given: '#&#f)

#;(let ()
    (plot-frame BAD-ARG)
    (plot-frame (list example-renderer2 example-nonrenderer2)
                #:title BAD-ARG)
    (plot-frame (list example-renderer2 example-nonrenderer2)
                #:x-label BAD-ARG)
    (plot-frame (list example-renderer2 example-nonrenderer2)
                #:y-label BAD-ARG)
    (plot3d-frame BAD-ARG)
    (plot3d-frame (list example-renderer3 example-nonrenderer3)
                 #:angle BAD-ARG)
    (plot3d-frame (list example-renderer3 example-nonrenderer3)
                 #:altitude BAD-ARG)
    (plot3d-frame (list example-renderer3 example-nonrenderer3)
                 #:title BAD-ARG)
    (plot3d-frame (list example-renderer3 example-nonrenderer3)
                 #:x-label BAD-ARG)
    (plot3d-frame (list example-renderer3 example-nonrenderer3)
                 #:y-label BAD-ARG)
    (plot3d-frame (list example-renderer3 example-nonrenderer3)
                 #:z-label BAD-ARG)
    (plot3d-frame (list example-renderer3 example-nonrenderer3)
                 #:legend-anchor BAD-ARG)
    (void))
