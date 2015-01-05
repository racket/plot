#lang typed/racket/base

;; Extra functions that can't be easily categorized

(require racket/sequence racket/list racket/match)

(provide (all-defined-out))

(: in-cycle* (All (A) (-> (Sequenceof A) (Sequenceof A))))
(define (in-cycle* s)
  (define n (sequence-length s))
  (if (zero? n) empty-sequence (in-cycle s)))

(: sequence-take (All (A) (-> (Sequenceof A) Integer Integer (Listof A))))
(define (sequence-take seq start end)
  (for/list ([e  (sequence-tail seq start)] 
             [_  (in-range (- end start))])
    e))

(: sequence-head-vector (All (A) (-> Symbol (Sequenceof A) Integer (Vectorof A))))
(define (sequence-head-vector name xs n)
  (define vec (for/vector ([x  xs] [i  (in-range n)]) : A
                x))
  (unless (= n (vector-length vec))
    (raise-argument-error name (format "sequence of length >= ~a" n) xs))
  vec)

(: sequence->listof-vector (All (A) (-> Symbol (Sequenceof (Sequenceof A)) Integer
                                        (Listof (Vectorof A)))))
(define (sequence->listof-vector name vs n)
  (map (λ ([v : (Sequenceof A)])
         (sequence-head-vector name v n))
       (sequence->list vs)))

(: cumulative-sum (-> (Listof Real) (Listof Real)))
(define (cumulative-sum xs)
  (reverse (foldl (λ ([x : Real] [xs : (Listof Real)])
                    (cons (+ x (first xs)) xs))
                  '(0)
                  xs)))

(: pair (All (A B) (-> A B (Pair A B))))
(define pair cons)

(: assoc-cons (All (A B) (-> (Listof (Pair A (Pair B (Listof B)))) A B
                             (Listof (Pair A (Pair B (Listof B)))))))
(define (assoc-cons hash key new-value)
  (let loop ([hash  hash])
    (cond [(empty? hash)  (list (pair key (list new-value)))]
          [else
           (define entry (first hash))
           (cond [(equal? (car entry) key)  (cons (pair key (pair new-value (cdr entry)))
                                                  (rest hash))]
                 [else  (cons (first hash) (loop (rest hash)))])])))

(: vector-find-index (All (A) (->* [(-> A Any) (Vectorof A)] [Integer Integer] (U Integer #f))))
(define (vector-find-index pred? xs [start 0] [end (vector-length xs)])
  (let/ec return : (U Integer #f)
    (for ([i  (in-range start end)] #:when (pred? (vector-ref xs i)))
      (return i))
    #f))

(: sorted-apply (All (A B) (-> (-> (Listof A) (Listof A))
                               (-> (Listof A) (Listof B))
                               (-> (Listof A) (Listof B)))))
(define ((sorted-apply sort f) lst)
  (define h
    (let ([sorted-lst  (sort lst)])
      (make-hash (map (inst pair A B) sorted-lst (f sorted-lst)))))
  (map (λ ([e : A]) (hash-ref h e)) lst))

(: transpose (All (A) (-> (Listof (Listof A))
                          (Listof (Listof (U #f A))))))
(define (transpose xss)
  (cond [(andmap empty? xss)  empty]
        [else  (cons (map (λ ([xs : (Listof A)]) (if (empty? xs) #f (first xs))) xss)
                     (transpose (map (λ ([xs : (Listof A)]) (if (empty? xs) empty (rest xs)))
                                     xss)))]))

(: group-neighbors (All (A) (-> (Listof A) (-> A A Any) (Listof (Listof A)))))
(define (group-neighbors lst equiv?)
  (reverse
   (map (inst reverse A)
        (cond
          [(empty? lst)  empty]
          [else
           (for/fold ([res : (Listof (Listof A))  (list (list (first lst)))])
                     ([e  (in-list (rest lst))])
             (if (andmap (λ ([e2 : A]) (equiv? e e2)) (first res))
                 (cons (cons e (first res)) (rest res))
                 (list* (list e) res)))]))))

(: bin-samples (-> (Listof Real) (Listof Real) (Listof (Listof Real))))
(define (bin-samples bin-bounds xs)
  (let* ([bin-bounds  (filter (λ (x) (not (eqv? x +nan.0))) (remove-duplicates bin-bounds))]
         [bin-bounds  (sort bin-bounds <)]
         [x-min  (first bin-bounds)]
         [x-max  (last bin-bounds)]
         [xs  (filter (λ ([x : Real]) (<= x-min x x-max)) xs)]
         [xs  (sort xs <)])
    (define-values (res rest-xs)
      (for/fold ([res : (Listof (Listof Real))  empty]
                 [xs : (Listof Real)  xs])
                ([x1  (in-list bin-bounds)]
                 [x2  (in-list (rest bin-bounds))])
        (: lst (Listof Real))
        (: rest-xs (Listof Real))
        (define-values (lst rest-xs)
          (let loop ([lst : (Listof Real)  empty] [xs xs])
            (if (and (not (empty? xs)) (<= x1 (first xs) x2))
                (loop (cons (first xs) lst) (rest xs))
                (values lst xs))))
        (values (cons (reverse lst) res)
                rest-xs)))
    (reverse res)))

(: make-raise-argument-error (-> Symbol Any * (-> String Natural Nothing)))
(define ((make-raise-argument-error name . args) type n)
  (apply raise-argument-error name type n args))

(: raise-keyword-error (-> Symbol String Keyword Any Nothing))
(define (raise-keyword-error fun-name type name value)
  (raise-argument-error fun-name (format "~a for ~a" type name) value))

(: make-raise-keyword-error (-> Symbol (-> String Keyword Any Nothing)))
(define ((make-raise-keyword-error fun-name) type name value)
  (raise-keyword-error fun-name type name value))
