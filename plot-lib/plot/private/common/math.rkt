#lang typed/racket/base

(require racket/match racket/list racket/vector math/base math/flonum
         "type-doc.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Flonums

(:: flblend (-> Flonum Flonum Flonum Flonum))
(define (flblend x y α)
  (+ (* α x) (* (- 1.0 α) y)))

(:: flsumof (All (A) (-> (-> A Flonum) (Listof A) Flonum)))
(define (flsumof f xs)
  (flsum (map f xs)))

(:: fldistance (-> Flonum * Flonum))
(define fldistance
  (case-lambda
    [()   0.0]
    [(x)  (abs x)]
    [(x y)  (flsqrt (+ (* x x) (* y y)))]
    [(x y z)  (flsqrt (+ (* x x) (* y y) (* z z)))]
    [xs  (flsqrt (flsumof (λ ([x : Flonum]) (* x x)) xs))]))

;; ===================================================================================================
;; Reals

(: rational?* (-> Any Boolean : #:+ Real))
(define (rational?* x)
  (and (real? x) (rational? x)))

(:: maybe-inexact->exact (-> (U #f Real) (U #f Exact-Rational)))
(define (maybe-inexact->exact x)
  (cond [x  (unless (rational? x)
              (raise-argument-error 'maybe-inexact->exact "rational or #f" x))
            (inexact->exact x)]
        [else  #f]))

(:: equal?* (-> Any * Boolean))
(define equal?*
  (case-lambda
    [()   #t]
    [(x)  #t]
    [xs   (and (equal? (car xs) (cadr xs))
               (apply equal?* (cdr xs)))]))

(define-syntax-rule (min2* x y)
  (cond [(x . < . y)  x]
        [(y . < . x)  y]
        [(exact? x)   x]
        [else  y]))

(define-syntax-rule (max2* x y)
  (cond [(x . > . y)  x]
        [(y . > . x)  y]
        [(exact? x)   x]
        [else  y]))

(:: min* (-> Real * Real))
(define min*
  (case-lambda
    [()     +inf.0]
    [(x)    x]
    [(x y)  (min2* x y)]
    [xs  (for/fold ([m : Real  +inf.0]) ([y  (in-list xs)])
           (min2* m y))]))

(:: max* (-> Real * Real))
(define max*
  (case-lambda
    [()     -inf.0]
    [(x)    x]
    [(x y)  (max2* x y)]
    [xs  (for/fold ([m : Real -inf.0]) ([y  (in-list xs)])
           (max2* m y))]))

(:: blend (-> Real Real Real Real))
(define (blend x y α)
  (+ (* α x) (* (- 1 α) y)))

(:: atan2 (-> Real Real Real))
(define (atan2 y x) real?
  (if (and (zero? y) (zero? x)) 0 (atan y x)))

(:: sumof (All (A) (-> (-> A Real) (Listof A) Real)))
(define (sumof f xs)
  (sum (map f xs)))

(:: real-modulo (-> Real Real Real))
(define (real-modulo x y)
  (- x (* y (floor (/ x y)))))

(:: distance (-> Real * Real))
(define distance
  (case-lambda
    [()  0]
    [(x)  (abs x)]
    [(x y)  (real-part (sqrt (+ (* x x) (* y y))))]
    [(x y z)  (real-part (sqrt (+ (* x x) (* y y) (* z z))))]
    [xs  (real-part (sqrt (sumof sqr xs)))]))

(:: exact∘log (-> Positive-Real Exact-Rational))
(define (exact∘log x)
  (define y (log x))
  (cond [(infinite? y)  (- (inexact->exact (log (numerator x)))
                           (inexact->exact (log (denominator x))))]
        [else  (inexact->exact y)]))

(:: floor-log/base (-> Positive-Integer Positive-Real Integer))
(define (floor-log/base b x)
  (cond [(not (b . >= . 2))
         (raise-argument-error 'floor-log/base "exact integer >= 2" 0 b x)]
        [else
         (define q (inexact->exact x))
         (define m (floor (/ (exact∘log q) (inexact->exact (log b)))))
         (let loop ([m m] [p  (expt b m)])
           (cond [(q . < . p)  (loop (sub1 m) (/ p b))]
                 [else  (define u (* p b))
                        (cond [(q . >= . u)  (loop (add1 m) u)]
                              [else  m])]))]))

(:: ceiling-log/base (-> Positive-Integer Positive-Real Integer))
(define (ceiling-log/base b x) exact-integer?
  (- (floor-log/base b (/ (inexact->exact x)))))

(:: polar->cartesian (-> Real Real (Vector Real Real)))
(define (polar->cartesian θ r)
  (let ([θ  (fl θ)]
        [r  (fl r)])
    (vector (* r (flcos θ))
            (* r (flsin θ)))))

(:: 3d-polar->3d-cartesian (-> Real Real Real (Vector Real Real Real)))
(define (3d-polar->3d-cartesian θ ρ r)
  (let ([θ  (fl θ)]
        [ρ  (fl ρ)]
        [r  (fl r)])
    (define cos-ρ (flcos ρ))
    (vector (* r (* (flcos θ) cos-ρ))
            (* r (* (flsin θ) cos-ρ))
            (* r (flsin ρ)))))

;; ===================================================================================================
;; Vectors

(:: vector-andmap
   (All (A B C ...)
        (case-> (-> (-> A Boolean) (Vectorof A) Boolean)
                (-> (-> A B Boolean) (Vectorof A) (Vectorof B) Boolean)
                (-> (-> A B C ... C Boolean) (Vectorof A) (Vectorof B) (Vectorof C) ... C Boolean))))
(define vector-andmap
  (case-lambda
    [([f : (-> A Boolean)] [as : (Vectorof A)])
     (for/and ([a  (in-vector as)])
       (f a))]
    [([f : (-> A B Boolean)] [as : (Vectorof A)] [bs : (Vectorof B)])
     (define n (vector-length as))
     (unless (= (vector-length bs) n)
       (raise-argument-error 'vector-andmap (format "vector of length ~a" n) 2 f as bs))
     (for/and ([a  (in-vector as)]
               [b  (in-vector bs)])
       (f a b))]
    [(f as bs . vs)
     (define n (vector-length as))
     (for ([v  (in-list (cons bs vs))]
           [i  (in-naturals 2)])
       (unless (= (vector-length v) n)
         (apply raise-argument-error 'vector-andmap (format "vector of length ~a" n) i f as bs vs)))
     (let loop ([i : Nonnegative-Fixnum  0])
       (cond [(< i n)
              (and (apply f (vector-ref as n) (vector-ref bs n)
                          (map (plambda: (C) ([v : (Vectorof C)])
                                 (vector-ref v n))
                               vs))
                   (loop (+ i 1)))]
             [else  #t]))]))

(:: vector-ormap
   (All (A B C ...)
        (case-> (-> (-> A Boolean) (Vectorof A) Boolean)
                (-> (-> A B Boolean) (Vectorof A) (Vectorof B) Boolean)
                (-> (-> A B C ... C Boolean) (Vectorof A) (Vectorof B) (Vectorof C) ... C Boolean))))
(define vector-ormap
  (case-lambda
    [([f : (-> A Boolean)] [as : (Vectorof A)])
     (for/or ([a  (in-vector as)])
       (f a))]
    [([f : (-> A B Boolean)] [as : (Vectorof A)] [bs : (Vectorof B)])
     (define n (vector-length as))
     (unless (= (vector-length bs) n)
       (raise-argument-error 'vector-ormap (format "vector of length ~a" n) 2 f as bs))
     (for/or ([a  (in-vector as)]
              [b  (in-vector bs)])
       (f a b))]
    [(f as bs . vs)
     (define n (vector-length as))
     (for ([v  (in-list (cons bs vs))]
           [i  (in-naturals 2)])
       (unless (= (vector-length v) n)
         (apply raise-argument-error 'vector-ormap (format "vector of length ~a" n) i f as bs vs)))
     (let loop ([i : Nonnegative-Fixnum  0])
       (cond [(< i n)
              (or (apply f (vector-ref as n) (vector-ref bs n)
                         (map (plambda: (C) ([v : (Vectorof C)])
                                (vector-ref v n))
                              vs))
                  (loop (+ i 1)))]
             [else  #f]))]))

(:: vcross (-> (Vectorof Real) (Vectorof Real) (Vector Real Real Real)))
(define (vcross v1 v2)
  (match v1
    [(vector x1 y1 z1)
     (match v2
       [(vector x2 y2 z2)
        (vector (- (* y1 z2) (* z1 y2))
                (- (* z1 x2) (* x1 z2))
                (- (* x1 y2) (* y1 x2)))]
       [_  (raise-argument-error 'vcross "vector of length 3" 1 v1 v2)])]
    [_  (raise-argument-error 'vcross "vector of length 3" 0 v1 v2)]))

(:: vcross2 (-> (Vectorof Real) (Vectorof Real) Real))
(define (vcross2 v1 v2)
  (match v1
    [(vector x1 y1)
     (match v2
       [(vector x2 y2)  (- (* x1 y2) (* y1 x2))]
       [_  (raise-argument-error 'vcross2 "vector of length 2" 1 v1 v2)])]
    [_  (raise-argument-error 'vcross2 "vector of length 2" 0 v1 v2)]))

(define-syntax-rule (vmap name f v)
  (let ()
    (define n (vector-length v))
    (for/vector #:length n ([x  (in-vector v)]) : Real
      (f x))))

(define-syntax-rule (unrolled-vmap name f v)
  (let ()
    (match v
      [(vector x y)    (vector (f x) (f y))]
      [(vector x y z)  (vector (f x) (f y) (f z))]
      [_  (vmap name f v)])))

(define-syntax-rule (vmap2 name f v1 v2)
  (let ()
    (define n (vector-length v1))
    (unless (= n (vector-length v2))
      (raise-argument-error name (format "vector of length ~a" n) 1 v1 v2))
    (for/vector #:length n ([x  (in-vector v1)]
                            [y  (in-vector v2)]) : Real
      (f x y))))

(define-syntax-rule (unrolled-vmap2 name f v1 v2)
  (match v1
    [(vector x1 y1)
     (match v2
       [(vector x2 y2)  (vector (f x1 x2) (f y1 y2))]
       [_  (raise-argument-error name "vector of length 2" 1 v1 v2)])]
    [(vector x1 y1 z1)
     (match v2
       [(vector x2 y2 z2)  (vector (f x1 x2) (f y1 y2) (f z1 z2))]
       [_  (raise-argument-error name "vector of length 3" 1 v1 v2)])]
    [_  (vmap2 name f v1 v2)]))

(:: v+ (-> (Vectorof Real) (Vectorof Real) (Vectorof Real)))
(define (v+ v1 v2)
  (unrolled-vmap2 'v+ + v1 v2))

(:: v- (-> (Vectorof Real) (Vectorof Real) (Vectorof Real)))
(define (v- v1 v2)
  (unrolled-vmap2 'v- - v1 v2))

(:: vneg (-> (Vectorof Real) (Vectorof Real)))
(define (vneg v)
  (unrolled-vmap 'vneg - v))

(:: v* (-> (Vectorof Real) Real (Vectorof Real)))
(define (v* v c)
  (define-syntax-rule (f x) (* x c))
  (unrolled-vmap 'v* f v))

(:: v/ (-> (Vectorof Real) Real (Vectorof Real)))
(define (v/ v c)
  (define-syntax-rule (f x) (/ x c))
  (unrolled-vmap 'v/ f v))

(:: vmag^2 (-> (Vectorof Real) Nonnegative-Real))
(define (vmag^2 v)
  (match v
    [(vector x y)    (+ (sqr x) (sqr y))]
    [(vector x y z)  (+ (sqr x) (sqr y) (sqr z))]
    [_  (for/fold ([mag : Nonnegative-Real  0]) ([x  (in-vector v)])
          (+ mag (sqr x)))]))

(:: vmag (-> (Vectorof Real) Nonnegative-Real))
(define (vmag v)
  (sqrt (vmag^2 v)))

(:: vnormalize (-> (Vectorof Real) (Vectorof Real)))
(define (vnormalize v)
  (define m (vmag v))
  (if (= 0 m) v (v/ v m)))

(:: vdot (-> (Vectorof Real) (Vectorof Real) Real))
(define (vdot v1 v2)
  (match v1
    [(vector x1 y1)
     (match v2
       [(vector x2 y2)  (+ (* x1 x2) (* y1 y2))]
       [_  (raise-argument-error 'vdot "vector of length 2" 1 v1 v2)])]
    [(vector x1 y1 z1)
     (match v2
       [(vector x2 y2 z2)  (+ (* x1 x2) (* y1 y2) (* z1 z2))]
       [_  (raise-argument-error 'vdot "vector of length 3" 1 v1 v2)])]
    [_
     (unless (= (vector-length v1) (vector-length v2))
       (raise-argument-error 'vdot (format "vector of ~a real numbers" (vector-length v1)) 1 v1 v2))
     (for/fold ([dot : Real  0]) ([x1  (in-vector v1)] [x2  (in-vector v2)])
       (+ dot (* x1 x2)))]))

(:: vcos-angle (-> (Vectorof Real) (Vectorof Real) Real))
(define (vcos-angle v1 v2)
  (define d (vdot v1 v2))
  (if (= d 0) d (/ d (vmag v1) (vmag v2))))

(:: vrational? (-> (Vectorof Real) Boolean))
(define (vrational? v)
  (match v
    [(vector (? rational? x) (? rational? y))  #t]
    [(vector (? rational? x) (? rational? y) (? rational? z))  #t]
    [_  (vector-andmap rational? v)]))

(:: v= (-> (Vectorof Real) (Vectorof Real) Boolean))
(define (v= v1 v2)
  (match v1
    [(vector x1 y1)
     (match v2
       [(vector x2 y2)  (and (= x1 x2) (= y1 y2))]
       [_  (raise-argument-error 'v= "vector of length 2" 1 v1 v2)])]
    [(vector x1 y1 z1)
     (match v2
       [(vector x2 y2 z2)  (and (= x1 x2) (= y1 y2) (= z1 z2))]
       [_  (raise-argument-error 'v= "vector of length 3" 1 v1 v2)])]
    [_
     (define n (vector-length v1))
     (unless (= (vector-length v2) n)
       (raise-argument-error 'v= (format "vector of length ~a" n) 1 v1 v2))
     (for/and ([x1  (in-vector v1)] [x2  (in-vector v2)])
       (= x1 x2))]))

(:: vcenter (-> (Listof (Vectorof Real)) (Vectorof Real)))
(define (vcenter vs)
  (match vs
    [(list (vector #{xs : (Listof Real)} #{ys : (Listof Real)}) ...)
     (define x-min (apply min* xs))
     (define x-max (apply max* xs))
     (define y-min (apply min* ys))
     (define y-max (apply max* ys))
     (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max)))]
    [(list (vector #{xs : (Listof Real)} #{ys : (Listof Real)} #{zs : (Listof Real)}) ...)
     (define x-min (apply min* xs))
     (define x-max (apply max* xs))
     (define y-min (apply min* ys))
     (define y-max (apply max* ys))
     (define z-min (apply min* zs))
     (define z-max (apply max* zs))
     (vector (* 1/2 (+ x-min x-max)) (* 1/2 (+ y-min y-max)) (* 1/2 (+ z-min z-max)))]
    [_
     (define-values (mins maxs)
       (for/fold ([mins : (Vectorof Real)  (first vs)]
                  [maxs : (Vectorof Real)  (first vs)])
                 ([v  (in-list (rest vs))])
         (values (vector-map min mins v)
                 (vector-map max maxs v))))
     (unrolled-vmap2 'vcenter (λ (x1 x2) (* 1/2 (+ x1 x2))) mins maxs)]))

(:: vrational-sublists (-> (Listof (Vectorof Real)) (Listof (Listof (Vectorof Real)))))
(define (vrational-sublists vs)
  (: res (Listof (Listof (Vectorof Real))))
  (define res
    (let loop ([vs vs])
      (cond [(null? vs)  (list null)]
            [(vrational? (car vs))  (define rst (loop (cdr vs)))
                                    (cons (cons (car vs) (car rst)) (cdr rst))]
            [else  (cons null (loop (cdr vs)))])))
  (cond [(and (not (null? res)) (null? (car res)))  (cdr res)]
        [else  res]))

(:: remove-degenerate-edges (-> (Listof (Vectorof Real)) (Listof (Vectorof Real))))
(define (remove-degenerate-edges vs)
  (cond
    [(empty? vs)  empty]
    [else
     (let*-values ([(last vs)
                    (for/fold ([last : (Vectorof Real)  (first vs)]
                               [vs : (Listof (Vectorof Real))  (list (first vs))])
                      ([v  (in-list (rest vs))])
                      (cond [(v= last v)  (values v vs)]
                            [else         (values v (cons v vs))]))]
                   [(vs)  (reverse vs)])
       (cond [(v= last (first vs))  (rest vs)]
             [else  vs]))]))

(:: default-normal (Vectorof Real))
(define default-normal (vector 0.0 -1.0 0.0))

(:: vnormal (-> (Listof (Vectorof Real)) (Vectorof Real)))
(define (vnormal vs)
  (let ([vs  (remove-degenerate-edges vs)])
    (cond
      [((length vs) . < . 3)  default-normal]
      [else
       (let ([vs  (append vs (take vs 2))])
         (define norm
           (for/fold ([norm : (Vectorof Real)  (vector 0.0 0.0 0.0)])
                     ([v1  (in-list vs)]
                      [v2  (in-list (rest vs))]
                      [v3  (in-list (rest (rest vs)))])
             (v+ norm (vcross (v- v3 v2) (v- v1 v2)))))
         (define m (vmag norm))
         (if (m . > . 0) (v/ norm m) default-normal))])))

;; ===================================================================================================
;; Intervals

(define-syntax-rule (maybe-min x y)
  (if x (if y (min* x y) x)
      (if y y #f)))

(define-syntax-rule (maybe-max x y)
  (if x (if y (max* x y) x)
      (if y y #f)))

(:: ivl-guard (-> (U Real #f) (U Real #f) Symbol (Values (U Real #f) (U Real #f))))
(define (ivl-guard a b _)
  (cond [(or (and a (nan? a)) (and b (nan? b)))  (values +nan.0 +nan.0)]
        [(and a b)  (values (min* a b) (max* a b))]
        [else  (values a b)]))

(struct ivl ([min : (U Real #f)] [max : (U Real #f)]) #:transparent #:guard ivl-guard)

(defthing empty-ivl ivl (ivl +nan.0 +nan.0))
(defthing unknown-ivl ivl (ivl #f #f))

(:: ivl-empty? (-> ivl Boolean))
(define (ivl-empty? i)
  (define a (ivl-min i))
  (and a (nan? a)))

(:: ivl-known? (-> ivl Boolean))
(define (ivl-known? i)
  (match-define (ivl a b) i)
  (and a b #t))

(:: ivl-rational? (-> ivl Boolean))
(define (ivl-rational? i)
  (match-define (ivl a b) i)
  (and (rational? a) (rational? b)))

#|
(:: rational-ivl? (-> Any Boolean))
(define (rational-ivl? i)
  (and (ivl? i) (ivl-rational? i)))
|#

(define (rational-ivl? i)
  (and (ivl? i) (ivl-rational? i)))

(:: ivl-singular? (-> ivl Boolean))
(define (ivl-singular? i)
  (match-define (ivl a b) i)
  (and a b (= a b)))

(:: ivl-length (-> ivl (U Real #f)))
(define (ivl-length i)
  (match-define (ivl a b) i)
  (if (and a b) (- b a) #f))

(:: ivl-center (-> ivl (U Real #f)))
(define (ivl-center i)
  (match-define (ivl a b) i)
  (if (and a b) (* 1/2 (+ a b)) #f))

(:: ivl-zero-length? (-> ivl Boolean))
(define (ivl-zero-length? i)
  (or (ivl-empty? i) (ivl-singular? i)))

(:: ivl-inexact->exact (-> ivl ivl))
(define (ivl-inexact->exact i)
  (match-define (ivl a b) i)
  (ivl (and a (if (nan? a) a (inexact->exact a)))
       (and b (if (nan? b) b (inexact->exact b)))))

(:: ivl-contains? (-> ivl Real Boolean))
(define (ivl-contains? i x)
  (match-define (ivl a b) i)
  (and a b (x . >= . a) (x . <= . b)))

(: ivl-meet2 (-> ivl ivl ivl))
(define (ivl-meet2 i1 i2)
  (cond [(or (ivl-empty? i1) (ivl-empty? i2))  empty-ivl]
        [else
         (match-define (ivl a1 b1) i1)
         (match-define (ivl a2 b2) i2)
         (define a (maybe-max a1 a2))
         (define b (maybe-min b1 b2))
         (if (and a b (a . > . b)) empty-ivl (ivl a b))]))

(:: ivl-meet (-> ivl * ivl))
(define (ivl-meet . is)
  (for/fold ([res  unknown-ivl]) ([i  (in-list is)])
    (ivl-meet2 res i)))

(: ivl-join2 (-> ivl ivl ivl))
(define (ivl-join2 i1 i2)
  (cond [(ivl-empty? i1)  i2]
        [(ivl-empty? i2)  i1]
        [else
         (match-define (ivl a1 b1) i1)
         (match-define (ivl a2 b2) i2)
         (ivl (maybe-min a1 a2) (maybe-max b1 b2))]))

(:: ivl-join (-> ivl * ivl))
(define (ivl-join . is)
  (for/fold ([res  empty-ivl]) ([i  (in-list is)])
    (ivl-join2 res i)))

(:: ivl-translate (-> ivl Real ivl))
(define (ivl-translate i d)
  (match-define (ivl a b) i)
  (ivl (and a (+ a d)) (and b (+ b d))))

(:: bounds->intervals (-> (Listof Real) (Listof ivl)))
(define (bounds->intervals xs)
  (cond [((length xs) . < . 2)  (raise-argument-error 'bounds->intervals "list with length >= 2" xs)]
        [else
         (for/list ([x1  (in-list xs)]
                    [x2  (in-list (rest xs))])
           (ivl x1 x2))]))

(:: clamp-real (-> Real ivl Real))
(define (clamp-real x i)
  (match-define (ivl a b) i)
  (let* ([x  (if b (min x b) x)]
         [x  (if a (max x a) x)])
    x))

;; ===================================================================================================
;; Rectangles

(deftype Rect (Vectorof ivl))

(:: empty-rect (-> Natural Rect))
(define (empty-rect n)
  (make-vector n empty-ivl))

(:: unknown-rect (-> Natural Rect))
(define (unknown-rect n)
  (make-vector n unknown-ivl))

(:: bounding-rect (-> (Listof (Vectorof Real)) Rect))
(define (bounding-rect vs)
  (define-values (mins maxs)
    (for/fold ([mins : (Vectorof Real)  (first vs)]
               [maxs : (Vectorof Real)  (first vs)])
              ([v  (in-list (rest vs))])
      (values (vector-map min mins v)
              (vector-map max maxs v))))
  (vector-map ivl mins maxs))

(:: rect-empty? (-> Rect Boolean))
(define (rect-empty? r)
  (vector-ormap ivl-empty? r))

(:: rect-known? (-> Rect Boolean))
(define (rect-known? r)
  (vector-andmap ivl-known? r))

(:: rect-rational? (-> Rect Boolean))
(define (rect-rational? r)
  (vector-andmap ivl-rational? r))

#|
(:: rational-rect? (-> Any Boolean))
(define (rational-rect? r)
  (and (vector? r) (vector-andmap rational-ivl? r)))
|#

(:: rect-area (-> Rect (U Real #f)))
(define (rect-area r)
  (let/ec break : (U Real #f)
    (for/fold ([area : Real  1]) ([i  (in-vector r)])
      (define len (ivl-length i))
      (cond [(or (not len) (zero? len))  (break len)]
            [else  (* area len)]))))

(:: rect-center (-> Rect (U #f (Vectorof Real))))
(define (rect-center r)
  (define n (vector-length r))
  (let/ec return : (U #f (Vectorof Real))
    (for/vector #:length n ([i  (in-vector r)]) : Real
      (define x (ivl-center i))
      (if x x (return #f)))))

(:: rect-zero-area? (-> Rect Boolean))
(define (rect-zero-area? r)
  (vector-ormap ivl-zero-length? r))

(:: rect-singular? (-> Rect Boolean))
(define (rect-singular? r)
  (vector-andmap ivl-singular? r))

(:: rect-inexact->exact (-> Rect Rect))
(define (rect-inexact->exact r)
  (vector-map ivl-inexact->exact r))

(:: rect-contains? (-> Rect (Vectorof Real) Boolean))
(define (rect-contains? r v)
  (vector-andmap ivl-contains? r v))

(: rect-meet2 (-> Rect Rect Rect))
(define (rect-meet2 r1 r2)
  (vector-map ivl-meet2 r1 r2))

(:: rect-meet (-> Rect * Rect))
(define (rect-meet . rs)
  (define n (apply max (map vector-length rs)))
  (for/fold ([res  (unknown-rect n)]) ([r  (in-list rs)])
    (rect-meet2 res r)))

(: rect-join2 (-> Rect Rect Rect))
(define (rect-join2 r1 r2)
  (vector-map ivl-join2 r1 r2))

(:: rect-join (-> Rect * Rect))
(define (rect-join . rs)
  (define n (apply max (map vector-length rs)))
  (for/fold ([res  (empty-rect n)]) ([r  (in-list rs)])
    (rect-join2 res r)))

(:: rect-translate (-> Rect (Vectorof Real) Rect))
(define (rect-translate r v)
  (vector-map ivl-translate r v))
