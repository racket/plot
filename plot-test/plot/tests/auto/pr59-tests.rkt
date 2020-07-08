#lang racket
(require rackunit
         plot
         racket/draw
         racket/runtime-path
         "helpers.rkt")

;; Tests for: https://github.com/racket/plot/pull/59

(define (do-plot-not-inverted output-fn)
  (output-fn (list (function sqr 1 7)
                   (error-bars (list (vector 2 4 12)
                                     (vector 4 16 20)
                                     (vector 6 36 10))
                               #:invert? #f))))

;; NOTE: both the invert? parameter and the coordinates need to be inverted.
(define (do-plot-inverted output-fn)
  (output-fn (list (function sqr 1 7)
                   (error-bars (list (vector 4 2 12)
                                     (vector 16 4 20)
                                     (vector 36 6 10))
                               #:invert? #t))))

;; To generate a plot image (e.g. to compare it to the saves samples, run:
;;
;;   (do-plot-not-inverted (lambda (rt) (plot-file rt "./data/pr59-not-inverted.png")))
;;   (do-plot-inverted (lambda (rt) (plot-file rt "./data/pr59-inverted.png")))
;;
;; To generate the draw commands, to compare them, run:
;;
;;   (define data-not-inverted (do-plot-not-inverted generate-draw-steps))
;;   (call-with-output-file "./data/pr59-not-inverted.rktd" (lambda (out) (write data-not-inverted out)) #:exists 'replace)
;;
;;   (define data-inverted (do-plot-inverted generate-draw-steps))
;;   (call-with-output-file "./data/pr59-inverted.rktd" (lambda (out) (write data-inverted out)) #:exists 'replace)

(define-runtime-path pr59-not-inverted-data "./data/pr59-not-inverted-data.rktd")
(define-runtime-path pr59-inverted-data "./data/pr59-inverted-data.rktd")

(define pr59-test-suite
  (test-suite
   "PR#59: Add #:invert? option to error-bars"
   (test-case "pr59 not inverted"
     (define saved (call-with-input-file pr59-not-inverted-data read))
     (define current (do-plot-not-inverted generate-draw-steps))
     (if (same-draw-commands? saved current)
         (check-true #t)
         (begin
           (printf "draw commands not the same, writing new set")
           (call-with-output-file "./data/new-pr59-not-inverted-data.rktd" (lambda (out) (write current out)))
           (check-true #f)))
     (check-same-draw-commands saved current))
   (test-case "pr59 inverted"
     (define saved (call-with-input-file pr59-inverted-data read))
     (define current (do-plot-inverted generate-draw-steps))
     (if (same-draw-commands? saved current)
         (check-true #t)
         (begin
           (printf "draw commands not the same, writing new set")
           (call-with-output-file "./data/new-pr59-inverted-data.rktd" (lambda (out) (write current out)))
           (check-true #f))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr59-test-suite))

