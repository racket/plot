#lang typed/racket/base

(require racket/match
         math/flonum
         "type-doc.rkt"
         "math.rkt")

(provide (all-defined-out))

(struct invertible-function ([f : (-> Real Real)]
                             [g : (-> Real Real)])
  #:transparent)

(:: invertible-compose (-> invertible-function invertible-function invertible-function))
(define (invertible-compose f1 f2)
  (match-let ([(invertible-function f1 g1)  f1]
              [(invertible-function f2 g2)  f2])
    (invertible-function (compose f1 f2) (compose g2 g1))))

(:: invertible-inverse (-> invertible-function invertible-function))
(define (invertible-inverse h)
  (match-define (invertible-function f g) h)
  (invertible-function g f))

(deftype Axis-Transform (-> Real Real invertible-function invertible-function))

(defthing id-transform Axis-Transform
  (λ (x-min x-max old-function) old-function))

(defthing id-function invertible-function (invertible-function (λ (x) x) (λ (x) x)))

(:: apply-axis-transform (-> Axis-Transform Real Real invertible-function))
(define (apply-axis-transform t x-min x-max)
  (t x-min x-max id-function))

(:: make-axis-transform (-> invertible-function Axis-Transform))
;; Turns any total, surjective, monotone real function and its inverse into an axis transform
(define (make-axis-transform fun)
  (match-define (invertible-function f g) fun)
  (λ (x-min x-max old-function)
    (define fx-min (f x-min))
    (define fx-scale (/ (- x-max x-min) (- (f x-max) fx-min)))
    (: new-f (-> Real Real))
    (: new-g (-> Real Real))
    (define (new-f x) (+ x-min (* (- (f x) fx-min) fx-scale)))
    (define (new-g y) (g (+ fx-min (/ (- y x-min) fx-scale))))
    (invertible-compose (invertible-function new-f new-g) old-function)))

;; ===================================================================================================
;; Axis transform combinators

(:: axis-transform-compose (-> Axis-Transform Axis-Transform Axis-Transform))
(define (axis-transform-compose t1 t2)
  (λ (x-min x-max old-function)
    (t1 x-min x-max (t2 x-min x-max old-function))))

(:: axis-transform-append (-> Axis-Transform Axis-Transform Real Axis-Transform))
(define (axis-transform-append t1 t2 mid)
  (λ (x-min x-max old-function)
    (match-define (invertible-function old-f old-g) old-function)
    (let ([mid  (old-f mid)])
      (cond [(mid . >= . x-max)  (t1 x-min x-max old-function)]
            [(mid . <= . x-min)  (t2 x-min x-max old-function)]
            [else
             (match-define (invertible-function f1 g1) (t1 x-min mid old-function))
             (match-define (invertible-function f2 g2) (t2 mid x-max old-function))
             ((make-axis-transform
               (invertible-function
                (λ (x) (cond [((old-f x) . < . mid)  (f1 x)]
                             [else  (f2 x)]))
                (λ (x) (cond [(x . < . mid)  (g1 x)]
                             [else  (g2 x)]))))
              x-min x-max id-function)]))))

(:: axis-transform-bound (-> Axis-Transform Real Real Axis-Transform))
(define (axis-transform-bound t a b)
  (axis-transform-append
   (axis-transform-append id-transform t a) id-transform b))

;; ===================================================================================================
;; Specific axis transforms

(: flnewton-invert (-> (-> Flonum Flonum)
                       (-> Flonum Flonum)
                       (-> Flonum Flonum)
                       Index
                       (-> Real Flonum)))
(define ((flnewton-invert f f-diff f-inv-guess n) y)
  (let ([y  (fl y)])
    (let loop ([x  (f-inv-guess y)] [n n])
      (let/ec return : Flonum
        (when (zero? n) (return x))
        
        (define dx (/ (- y (f x)) (f-diff x)))
        (when (zero? dx) (return x))
        
        (loop (fl- x dx) (sub1 n))))))

(: sine-diag (-> Real (-> Real Flonum)))
(define (sine-diag d)
  (let ([d  (fl d)])
    (λ (x) (let ([x  (fl x)])
             (+ x (* (/ 1.0 (* 4.0 d)) (flsin (* d x))))))))

(: sine-diag-diff (-> Real (-> Real Flonum)))
(define (sine-diag-diff d)
  (let ([d  (fl d)])
    (λ (x) (let ([x  (fl x)])
             (- (/ (flcos (* d x)) 4.0) 1.0)))))

(: sine-diag-inv (-> Real (-> Real Flonum)))
(define (sine-diag-inv d)
  (flnewton-invert (sine-diag d) (sine-diag-diff d) values 10))

(: cbrt (-> Real Flonum))
(define (cbrt x)
  (let ([x  (fl x)])
    (* (flsgn x) (flexpt (abs x) #i1/3))))

(: cube (-> Real Flonum))
(define (cube x)
  (let ([x  (fl x)])
    (* x x x)))

(: real-log (-> Real Flonum))
(define (real-log x)
  (fllog (fl x)))

(: real-exp (-> Real Flonum))
(define (real-exp x)
  (flexp (fl x)))

(defthing log-transform Axis-Transform
  (λ (x-min x-max old-function)
    (when ((fl x-min) . <= . 0)
      (raise-type-error 'log-transform "positive real" 0 x-min x-max))
    ((make-axis-transform (invertible-function real-log real-exp)) x-min x-max old-function)))

(defthing cbrt-transform Axis-Transform
  (λ (x-min x-max old-function)
    ((make-axis-transform (invertible-function cbrt cube)) x-min x-max old-function)))

(: hand-drawn-transform (-> Positive-Real Axis-Transform))
(define (hand-drawn-transform freq)
  (λ (x-min x-max old-function)
    (define d (/ freq (- x-max x-min)))
    ((make-axis-transform (invertible-function (sine-diag d) (sine-diag-inv d)))
     x-min x-max old-function)))

;; ===================================================================================================

(: stretch (-> Real Real Real (-> Real Real)))
(define (stretch a b s)
  (define d (- b a))
  (define ds (* d s))
  (λ (x)
    (cond [(x . < . a)  x]
          [(x . > . b)  (+ (- x d) ds)]
          [else         (+ a (* (- x a) s))])))

(: stretch-transform (-> Real Real Positive-Real Axis-Transform))
(define (stretch-transform a b scale)
  (when (a . > . b) (error 'stretch-transform "expected a <= b; given ~e and ~e" a b))
  (λ (x-min x-max old-function)
    (match-define (invertible-function old-f old-g) old-function)
    (let ([a  (old-f a)]
          [b  (old-f b)])
      (define f (stretch a b scale))
      (define g (stretch (f a) (f b) (/ 1 scale)))
      ((make-axis-transform (invertible-function f g)) x-min x-max old-function))))

(: collapse-transform (-> Real Real Axis-Transform))
(define (collapse-transform a b)
  (when (a . > . b) (error 'stretch-transform "expected a <= b; given ~e and ~e" a b))
  (λ (x-min x-max old-function)
    (match-define (invertible-function old-f old-g) old-function)
    (let ([a  (old-f a)]
          [b  (old-f b)])
      (define 1/2size (* 1/2 (- b a)))
      (define center (* 1/2 (+ a b)))
      (: f (-> Real Real))
      (: g (-> Real Real))
      (define (f x) (cond [(x . < . a)  (+ x 1/2size)]
                          [(x . > . b)  (- x 1/2size)]
                          [else  center]))
      (define (g x) (cond [(x . < . center)  (- x 1/2size)]
                          [(x . > . center)   (+ x 1/2size)]
                          [else  center]))
      ((make-axis-transform (invertible-function f g)) x-min x-max old-function))))
