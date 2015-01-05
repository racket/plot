#lang typed/racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/list racket/vector
         "type-doc.rkt"
         "math.rkt"
         "axis-transform.rkt")

(provide (all-defined-out))

(:: build-linear-seq (->* [Real Real Natural] [Real Real] (Listof Real)))
(define (build-linear-seq start step num [min-val start] [max-val (+ start (* (- num 1) step))])
  (define n-start (max 0 (inexact->exact (floor (/ (- min-val start) step)))))
  (define n-end (min num (+ (inexact->exact (ceiling (/ (- max-val start) step))) 1)))
  (for*/list : (Listof Real) ([n  (in-range n-start n-end)]
                              [x  (in-value (+ start (* n step)))]
                              #:when (<= min-val x max-val))
    x))

(:: linear-seq (->* [Real Real Natural] [#:start? Boolean #:end? Boolean] (Listof Real)))
(define (linear-seq start end num #:start? [start? #t] #:end? [end? #t])
  (cond
    [(zero? num)  empty]
    ; ambiguous request: arbitrarily return start
    [(and start? end? (= 1 num))  (list start)]
    [(end . < . start)  (reverse (linear-seq end start num #:start? end? #:end? start?))]
    [(end . = . start)  (build-list num (λ _ start))]
    [else
     (define size (- end start))
     (define step (/ size (cond [(and start? end?)  (- num 1)]
                                [(or start? end?)   (- num 1/2)]
                                [else               num])))
     (define real-start
       (cond [start?  start]
             [else    (+ start (* 1/2 step))]))
     
     (build-linear-seq real-start step num)]))

(:: linear-seq* (->* [(Listof Real) Natural] [#:start? Boolean #:end? Boolean] (Listof Real)))
(define (linear-seq* points num #:start? [start? #t] #:end? [end? #t])
  (let/ec return
    (when (empty? points) (raise-type-error 'linear-seq* "nonempty (listof real?)" points))
    
    (define pts (list->vector points))
    (define len (vector-length pts))
    
    (define indexes (linear-seq 0 (sub1 len) num #:start? start? #:end? end?))
    (define int-parts (map exact-floor indexes))
    (define frac-parts (map - indexes int-parts))
    (map (λ ([i : Integer] [f : Real])
           (if (= i (sub1 len))
               (vector-ref pts i)
               (blend (vector-ref pts (add1 i)) (vector-ref pts i) f)))
         int-parts frac-parts)))

(:: nonlinear-seq (->* [Real Real Natural Axis-Transform] [#:start? Boolean #:end? Boolean]
                       (Listof Real)))
(define (nonlinear-seq start end num transform #:start? [start? #t] #:end? [end? #t])
  (match-define (invertible-function _ finv) (apply-axis-transform transform start end))
  (map finv (linear-seq start end num #:start? start? #:end? end?)))

;; ===================================================================================================

(: ensure-endpoints (-> (Listof Real) Real Real (Listof Real)))
(define (ensure-endpoints xs i-min i-max)
  (cond [(empty? xs)  (cond [(i-min . = . i-max)  (list i-min)]
                            [else  (list i-min i-max)])]
        [else
         (define xs-min (first xs))
         (define xs-max (last xs))
         (let* ([xs  (if (xs-min . <= . i-min) xs (cons i-min xs))]
                [xs  (if (xs-max . >= . i-max) xs (append xs (list i-max)))])
           xs)]))

(:: sample-points (->* [ivl ivl Natural] [Axis-Transform] (Listof Real)))
(define (sample-points outer-ivl inner-ivl num [transform id-transform])
  (unless (ivl-rational? outer-ivl)
    (raise-argument-error 'sample-points "ivl-rational?" 0 outer-ivl inner-ivl num transform))
  (let* ([inner-ivl  (ivl-meet inner-ivl outer-ivl)]
         [inner-ivl  (ivl-inexact->exact inner-ivl)]
         [outer-ivl  (ivl-inexact->exact outer-ivl)])
    (match-define (ivl o-min o-max) outer-ivl)
    (match-define (ivl i-min i-max) inner-ivl)
    (let ([o-min  (assert o-min values)]  ; must be non-#f because outer-ivl is rational
          [o-max  (assert o-max values)]  ; ditto
          [i-min  (assert i-min values)]  ; should be non-#f because of meet with rational interval
          [i-max  (assert i-max values)]  ; ditto
          )
      (match-define (invertible-function f finv) (apply-axis-transform transform o-min o-max))
      (cond
        [(ivl-empty? inner-ivl)  empty]
        [(= num 0)  empty]
        [(or (= o-min o-max) (= num 1))
         (cond [(<= i-min o-min i-max)  (build-list num (λ _ o-min))]
               [else  empty])]
        [else
         (define step (/ (- o-max o-min) (- num 1)))
         (let* ([xs  (map finv (build-linear-seq o-min step num (f i-min) (f i-max)))]
                [xs  (remove-duplicates (map (λ ([x : Real]) (clamp-real x inner-ivl)) xs))])
           (ensure-endpoints xs i-min i-max))]))))

;; ===================================================================================================
;; Making memoized samplers

(struct sample ([xs : (Listof Real)]
                [ys : (Listof Real)]
                [y-min : (U Real #f)]
                [y-max : (U Real #f)])
  #:transparent)

(struct 2d-sample ([xs : (Listof Real)]
                   [ys : (Listof Real)]
                   [zss : (Vectorof (Vectorof Real))]
                   [z-min : (U Real #f)]
                   [z-max : (U Real #f)])
  #:transparent)

(struct 3d-sample ([xs : (Listof Real)]
                   [ys : (Listof Real)]
                   [zs : (Listof Real)]
                   [dsss : (Vectorof (Vectorof (Vectorof Real)))]
                   [d-min : (U Real #f)]
                   [d-max : (U Real #f)])
  #:transparent)

(deftype Sampler (-> ivl Natural sample))
(deftype 2D-Sampler (-> (Vector ivl ivl) (Vector Natural Natural) 2d-sample))
(deftype 3D-Sampler (-> (Vector ivl ivl ivl) (Vector Natural Natural Natural) 3d-sample))

(:: make-function->sampler (-> (-> Axis-Transform) (-> (-> Real Real) ivl Sampler)))
(define ((make-function->sampler transform-thnk) g inner-ivl)
  ;; f is like g, but silently throws away errors
  (define f (λ ([x : Real]) (with-handlers ([exn:fail?  (λ (_) +nan.0)]) (g x))))
  (define memo ((inst make-hash (Vector ivl Natural Axis-Transform) sample)))
  (λ (outer-ivl num)
    (define tx (transform-thnk))
    (: key (Vector ivl Natural Axis-Transform))
    (define key (vector outer-ivl num tx))
    (hash-ref! memo key
               (λ ()
                 (define xs (sample-points outer-ivl inner-ivl num tx))
                 (define: y-min : (U Real #f) #f)
                 (define: y-max : (U Real #f) #f)
                 (define ys
                   (for/list : (Listof Real) ([x  (in-list xs)])
                     (define y (f x))
                     (cond [(rational? y)
                            (let ([y-min-val  y-min])
                              (unless (and y-min-val (y . >= . y-min-val)) (set! y-min y)))
                            (let ([y-max-val  y-max])
                              (unless (and y-max-val (y . <= . y-max-val)) (set! y-max y)))
                            (inexact->exact y)]
                           [else  y])))
                 (sample xs ys
                         (maybe-inexact->exact y-min)
                         (maybe-inexact->exact y-max))))))

(:: make-2d-function->sampler (-> (-> Axis-Transform) (-> Axis-Transform)
                                  (-> (-> Real Real Real) (Vector ivl ivl) 2D-Sampler)))
(define ((make-2d-function->sampler transform-x-thnk transform-y-thnk) g inner-rect)
  ;; f is like g, but silently throws away errors
  (define f (λ ([x : Real] [y : Real]) (with-handlers ([exn:fail?  (λ (_) +nan.0)]) (g x y))))
  (define memo ((inst make-hash
                      (Vector (Vector ivl ivl)
                              (Vector Natural Natural)
                              Axis-Transform
                              Axis-Transform)
                      2d-sample)))
  (λ (outer-rect nums)
    (define tx (transform-x-thnk))
    (define ty (transform-y-thnk))
    (hash-ref! memo (vector outer-rect nums tx ty)
               (λ ()
                 (match-define (vector outer-x-ivl outer-y-ivl) outer-rect)
                 (match-define (vector inner-x-ivl inner-y-ivl) inner-rect)
                 (match-define (vector x-num y-num) nums)
                 (define xs (sample-points outer-x-ivl inner-x-ivl x-num tx))
                 (define ys (sample-points outer-y-ivl inner-y-ivl y-num ty))
                 (define: z-min : (U Real #f) #f)
                 (define: z-max : (U Real #f) #f)
                 (define zss
                   (for/vector #:length (length ys) ([y  (in-list ys)]) : (Vectorof Real)
                     (for/vector #:length (length xs) ([x  (in-list xs)]) : Real
                       (define z (f x y))
                       (cond [(rational? z)
                              (let ([z-min-val  z-min])
                                (unless (and z-min-val (z . >= . z-min-val)) (set! z-min z)))
                              (let ([z-max-val  z-max])
                                (unless (and z-max-val (z . <= . z-max-val)) (set! z-max z)))
                              (inexact->exact z)]
                             [else  z]))))
                 (2d-sample xs ys zss
                            (maybe-inexact->exact z-min)
                            (maybe-inexact->exact z-max))))))

(:: make-3d-function->sampler (-> (-> Axis-Transform) (-> Axis-Transform) (-> Axis-Transform)
                                  (-> (-> Real Real Real Real) (Vector ivl ivl ivl) 3D-Sampler)))
(define ((make-3d-function->sampler transform-x-thnk transform-y-thnk transform-z-thnk) g inner-rect)
  ;; f is like g, but silently throws away errors
  (define f (λ ([x : Real] [y : Real] [z : Real])
              (with-handlers ([exn:fail?  (λ (_) +nan.0)])
                (g x y z))))
  (define memo ((inst make-hash
                      (Vector (Vector ivl ivl ivl)
                              (Vector Natural Natural Natural)
                              Axis-Transform
                              Axis-Transform
                              Axis-Transform)
                      3d-sample)))
  (λ (outer-rect nums)
    (define tx (transform-x-thnk))
    (define ty (transform-y-thnk))
    (define tz (transform-z-thnk))
    (hash-ref! memo (vector outer-rect nums tx ty tz)
               (λ ()
                 (match-define (vector outer-x-ivl outer-y-ivl outer-z-ivl) outer-rect)
                 (match-define (vector inner-x-ivl inner-y-ivl inner-z-ivl) inner-rect)
                 (match-define (vector x-num y-num z-num) nums)
                 (define xs (sample-points outer-x-ivl inner-x-ivl x-num tx))
                 (define ys (sample-points outer-y-ivl inner-y-ivl y-num ty))
                 (define zs (sample-points outer-z-ivl inner-z-ivl z-num tz))
                 (define: d-min : (U Real #f) #f)
                 (define: d-max : (U Real #f) #f)
                 (define dsss
                   (for/vector #:length (length zs) ([z  (in-list zs)]) : (Vectorof (Vectorof Real))
                     (for/vector #:length (length ys) ([y  (in-list ys)]) : (Vectorof Real)
                       (for/vector #:length (length xs) ([x  (in-list xs)]) : Real
                         (define d (f x y z))
                         (cond [(rational? d)
                                (let ([d-min-val  d-min])
                                  (unless (and d-min-val (d . >= . d-min-val)) (set! d-min d)))
                                (let ([d-max-val  d-max])
                                  (unless (and d-max-val (d . <= . d-max-val)) (set! d-max d)))
                                (inexact->exact d)]
                               [else  d])))))
                 (3d-sample xs ys zs dsss
                            (maybe-inexact->exact d-min)
                            (maybe-inexact->exact d-max))))))

(define-syntax-rule (for-2d-sample (xa xb ya yb z1 z2 z3 z4) sample expr ...)
  (let ()
    (match-define (2d-sample xs ys zss fz-min fz-max) sample)
    (define ya (first ys))
    (define zs0 (vector-ref zss 0))
    (for/fold ([ya : Real  ya]
               [zs0 : (Vectorof Real)  zs0])
              ([yb  (in-list (rest ys))]
               [i  (in-range 1 (vector-length zss))])
      (define zs1 (vector-ref zss i))
      (define xa (first xs))
      (define z1 (vector-ref zs0 0))
      (define z4 (vector-ref zs1 0))
      (for/fold ([xa : Real  xa]
                 [z1 : Real  z1]
                 [z4 : Real  z4])
                ([xb  (in-list (rest xs))]
                 [j  (in-range 1 (min (vector-length zs0) (vector-length zs1)))])
        (define z2 (vector-ref zs0 j))
        (define z3 (vector-ref zs1 j))
        expr ...
        (values xb z2 z3))
      (values yb zs1))))

(define-syntax-rule (for-3d-sample (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample expr ...)
  (let ()
    (match-define (3d-sample xs ys zs dsss fd-min fd-max) sample)
    (define za (first zs))
    (define dss0 (vector-ref dsss 0))
    (for/fold ([za : Real  za]
               [dss0 : (Vectorof (Vectorof Real))  dss0])
              ([zb  (in-list (rest zs))]
               [i  (in-range 1 (vector-length dsss))])
      (define dss1 (vector-ref dsss i))
      (define ya (first ys))
      (define ds00 (vector-ref dss0 0))
      (define ds10 (vector-ref dss1 0))
      (for/fold ([ya : Real  ya]
                 [ds00 : (Vectorof Real)  ds00]
                 [ds10 : (Vectorof Real)  ds10])
                ([yb  (in-list (rest ys))]
                 [j  (in-range 1 (min (vector-length dss0) (vector-length dss1)))])
        (define ds01 (vector-ref dss0 j))
        (define ds11 (vector-ref dss1 j))
        (define xa (first xs))
        (define d1 (vector-ref ds00 0))
        (define d4 (vector-ref ds01 0))
        (define d5 (vector-ref ds10 0))
        (define d8 (vector-ref ds11 0))
        (for/fold ([xa : Real  xa]
                   [d1 : Real  d1]
                   [d4 : Real  d4]
                   [d5 : Real  d5]
                   [d8 : Real  d8])
                  ([xb  (in-list (rest xs))]
                   [k  (in-range 1 (min (vector-length ds00)
                                        (vector-length ds01)
                                        (vector-length ds10)
                                        (vector-length ds11)))])
          (define d2 (vector-ref ds00 k))
          (define d3 (vector-ref ds01 k))
          (define d6 (vector-ref ds10 k))
          (define d7 (vector-ref ds11 k))
          expr ...
          (values xb d2 d3 d6 d7))
        (values yb ds01 ds11))
      (values zb dss1))))
