#lang racket/base

(require scribble/eval
         (for-label racket
                    racket/gui/base
                    pict
                    db
                    plot
                    plot/utils
                    unstable/contract)
         (rename-in unstable/latent-contract/defthing
                    [doc-apply  old-doc-apply]))

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket
                                  racket/gui/base
                                  pict
                                  db
                                  plot
                                  plot/utils
                                  unstable/contract))
         doc-apply)

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         (prefix-in s. scribble/manual)
         (only-in racket/contract any/c)
         (for-label (only-in racket/contract any/c)))

(define (author-email) "neil.toronto@gmail.com")

(define-syntax (doc-apply stx)
  (syntax-parse stx
    [(_ name:id . pre-flows)
     (with-syntax ([name:doc  (format-id #'name "~a:doc" #'name)])
       (syntax-protect
        (syntax/loc stx (s.defthing name any/c . pre-flows))
        #;
        (syntax/loc stx (name:doc . pre-flows))))]))

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
