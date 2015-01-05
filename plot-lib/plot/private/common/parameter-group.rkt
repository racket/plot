#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/list)
         racket/list)

(provide define-parameter-group parameterize/group parameterize*/group)

(begin-for-syntax
  (struct parameter-group (ids) #:transparent
    #:property prop:procedure
    (λ (g stx)
      (syntax-case stx ()
        [(_)
         (with-syntax ([(ids ...)  (parameter-group-ids g)])
           (syntax/loc stx
             (list (ids) ...)))]))))

(define-syntax (define-parameter-group stx)
  (syntax-parse stx
    [(_ name:id (param:id ...))
     (syntax/loc stx
       (define-syntax name (parameter-group (list #'param ...))))]))

(define-for-syntax (make-list-ref val i)
  (cond [(= i 0)  #`(car #,val)]
        [else  (make-list-ref #`(cdr #,val) (- i 1))]))

(define-for-syntax (expand-parameter-groups id val)
  (define group (syntax-local-value id (λ () #f)))
  (if (and group (parameter-group? group))
      (let ([ids  (parameter-group-ids group)])
        (append*
         (map (λ (id i) (expand-parameter-groups id (make-list-ref val i)))
              ids
              (build-list (length ids) values))))
      (list #`[#,id #,val])))

;; Corresponds to parameterize
(define-syntax (parameterize/group stx)
  (syntax-parse stx
    [(_ ([p:id v] ...) . body)
     (with-syntax* ([(v-name ...)  (generate-temporaries #'(v ...))]
                    [([p new-v] ...)  (append* (map expand-parameter-groups
                                                    (syntax->list #'(p ...))
                                                    (syntax->list #'(v-name ...))))])
       (syntax/loc stx
         (let ([v-name v] ...)
           (parameterize ([p new-v] ...) . body))))]))

;; Corresponds to parameterize*
(define-syntax parameterize*/group
  (syntax-rules ()
      [(_ () . body)
       (let () . body)]
      [(_ ([lhs1 rhs1] [lhs rhs] ...) . body)
       (parameterize/group ([lhs1 rhs1]) (parameterize*/group ([lhs rhs] ...) . body))]))
