#lang typed/racket/base

(require racket/list
         (only-in racket/unsafe/ops unsafe-vector-ref))

(provide exact-vector2d
         exact-vector2d-sublists
         exact-polygon2d)

(: exact-vector2d (-> (Vectorof Real) (U #f (Vectorof Real))))
(define (exact-vector2d v)
  (define n (vector-length v))
  (cond [(= n 2)
         (define v1 (unsafe-vector-ref v 0))
         (define v2 (unsafe-vector-ref v 1))
         (cond [(and (rational? v1) (rational? v2))
                (vector (inexact->exact v1) (inexact->exact v2))]
               [else  #f])]
        [else  #f]))

(: sublists (All (A) (-> (Listof (U #f A)) (Listof (Listof A)))))
(define (sublists vs)
  (define vss
    (for/fold ([vss : (Listof (Listof A))  (list null)]) ([v  (in-list vs)])
      (cond [v  (cons (cons v (car vss)) (cdr vss))]
            [(null? (car vss))  vss]
            [else  (cons null vss)])))
  (cond [(null? (car vss))  (cdr vss)]
        [else  vss]))

(: exact-vector2d-sublists (-> (Listof (Vectorof Real)) (Listof (Listof (Vectorof Real)))))
(define (exact-vector2d-sublists vs)
  (sublists (map exact-vector2d vs)))

(: exact-polygon2d (All (L) (-> (Listof (Vectorof Real)) (Listof L)
                                (Values (Listof (Vectorof Real))
                                        (Listof L)))))
(define (exact-polygon2d vs ls)
  (cond
    [(null? vs)  (values null null)]
    [else
     (define-values (new-vs new-ls _)
       (for/fold ([vs : (Listof (Vectorof Real))  null]
                  [ls : (Listof L)  null]
                  [v1 : (Vectorof Real)  (last vs)])
                 ([v2  (in-list vs)]
                  [l   (in-list ls)])
         (cond [(equal? v1 v2)  (values vs ls v2)]
               [else
                (define exact-v2 (exact-vector2d v2))
                (if exact-v2
                    (values (cons exact-v2 vs) (cons l ls) v2)
                    (values vs ls v2))])))
     (values (reverse new-vs) (reverse new-ls))]))
