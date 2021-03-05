#lang racket
(require rackunit
         plot pict racket/runtime-path
         "../helpers.rkt")

; tests for PR#86, https://github.com/racket/plot/pull/86
; "add scientific? argument to log-ticks and log-ticks-format"

(define ((do-plot scientific? [base 10]) output-fn)
  (parameterize ([plot-x-ticks (log-ticks #:scientific? scientific? #:base base)]
                 [plot-x-transform log-transform])
    (output-fn
     (function (λ (x) (log x base)) 1 1000))))

(define-runtime-path pr86-base10-sci-data "./test-data/pr86-1.dat")
(define-runtime-path pr86-base10-nosci-data "./test-data/pr86-2.dat")
(define-runtime-path pr86-base2-sci-data "./test-data/pr86-3.dat")
(define-runtime-path pr86-base2-nosci-data "./test-data/pr86-4.dat")

(define pr86-test-suite
  (test-suite
   "PR#86: add scientific? to log-ticks"
   (test-case "PR86: base 10, scientific? #t"
     (check-draw-steps (do-plot #t 10) pr86-base10-sci-data))
   (test-case "PR86: base 10, scientific? #f"
     (check-draw-steps (do-plot #f 10) pr86-base10-nosci-data))
   (test-case "PR86: base 2, scientific? #t"
     (check-draw-steps (do-plot #t 2) pr86-base2-sci-data))
   (test-case "PR86: base 2, scientific? #f"
     (check-draw-steps (do-plot #f 2) pr86-base2-nosci-data))))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr86-test-suite))
