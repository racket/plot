#lang racket/base

(require scribble/eval
         (for-label racket
                    racket/gui/base
                    pict
                    db
                    plot
                    plot/utils
                    unstable/contract))

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket
                                  racket/gui/base
                                  pict
                                  db
                                  plot
                                  plot/utils
                                  unstable/contract)))

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         (prefix-in s. scribble/manual)
         (only-in racket/contract any/c)
         (for-label (only-in racket/contract any/c)))

(define (author-email) "neil.toronto@gmail.com")

(define (plot-name) "Plot")

(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require racket/math racket/match racket/list racket/draw racket/class
                      plot/pict
                      plot/utils)))
    eval))

(define (close-plot-eval)
  (close-eval plot-eval))
