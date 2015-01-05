#lang typed/racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide (all-defined-out))

(define-syntax-rule (deftype stuff ...)
  (define-type stuff ...))

(define-syntax-rule (:: name type)
  (: name type))

(define-syntax (defthing stx)
  (syntax-case stx ()
    [(_ name type val)
     (syntax/loc stx
       (begin
         (: name type)
         (define name val)))]
    [(_ name type #:document-value val)
     (syntax/loc stx
       (defthing name type val))]))

(define-for-syntax (parameter-name->arg-name name-stx)
  (define name-str (symbol->string (syntax->datum name-stx)))
  (define arg-name-str
    (cond [(regexp-match #rx".*-(.*)$" name-str)  => (λ (m) (last m))]
          [(regexp-match #rx"^$" name-str)        => (λ (m) "value")]
          [else  (substring name-str 0 1)]))
  (datum->syntax name-stx (string->symbol arg-name-str)))

(define-syntax (defparam stx)
  (syntax-parse stx
    [(_ name:id arg:id type:expr default:expr)
     (syntax/loc stx
       (begin (: name (Parameterof type))
              (define name (make-parameter default))))]
    [(_ name:id type:expr default:expr)
     (quasisyntax/loc stx
       (defparam name #,(parameter-name->arg-name #'name) type default))]))

(define-syntax (defparam2 stx)
  (syntax-parse stx
    [(_ name:id arg:id type1:expr type2:expr default:expr proc:expr)
     (syntax/loc stx
       (begin (: name (Parameterof type1 type2))
              (define name (make-parameter default proc))))]
    [(_ name:id type1:expr type2:expr default:expr proc:expr)
     (quasisyntax/loc stx
       (defparam2 name #,(parameter-name->arg-name #'name) type1 type2 default proc))]))
