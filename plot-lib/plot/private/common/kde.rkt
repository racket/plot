#lang typed/racket/base

(require racket/list racket/promise
         math/base
         math/flonum
         (only-in math/statistics sort-samples)
         "math.rkt"
         "utils.rkt"
         "sample.rkt"
         "type-doc.rkt")

(provide kde)

(: make-kde/windowed (-> (Vectorof Flonum) Flonum (Vectorof Flonum) Flonum (-> Flonum Flonum)))
;; Can assume that xs is sorted
;; Make a naive KDE, but uses windows to keep from adding Gaussians more than max-dist away
(define ((make-kde/windowed xs h ws max-dist) y)
  (define i (vector-find-index (λ ([x : Flonum]) (<= (abs (- x y)) max-dist)) xs))
  (cond [(not i)  0.0]
        [else
         (define j
           (let ([j  (vector-find-index (λ ([x : Flonum]) (> (abs (- x y)) max-dist)) xs i)])
             (if j j (vector-length xs))))
         (for/fold ([p : Flonum  0.0]) ([k  (in-range i j)])
           (define x (vector-ref xs k))
           (define w (vector-ref ws k))
           (define z (/ (- x y) h))
           (+ p (* w (flexp (- (sqr z))))))]))

;; Below this amount, we're fine with a kernel not contributing to the sum
(define eps 1e-06)
;; 1e-06 is the density returned at just over 5 standard deviations away from zero. If the estimate
;; needs more sensitivity, then KDE is almost certainly the wrong thing to do.

(: weight-max-dist (-> Flonum Flonum Flonum))
;; Returns the maximum distance at which unnormalized kernel (with weight w and width h) will
;; contribute at least eps to the sum
(define (weight-max-dist w h)
  (define a (/ w eps))
  (if (a . > . 1.0)
      (* h (* (flsqrt 2.0) (flsqrt (fllog a))))
      0.0))

(:: kde (->* [(Listof Real) Real] [(U (Listof Real) #f)]
             (Values (-> Real Real) (U Real #f) (U Real #f))))
(define (kde xs h [ws #f])
  (define N (length xs))
  (define M (if ws (length ws) N))
  (cond
    [(not (positive? h))
     (raise-argument-error 'kde "Positive-Real" 1 xs h ws)]
    [(and ws (ormap negative? ws))
     (raise-argument-error 'kde "(Listof Nonnegative-Real)" 2 xs h ws)]
    [(not (= N M))
     (raise-argument-error 'kde (format "list of length ~a" N) 2 xs h ws)]
    [(= N 0)
     (values (λ (y) 0) #f #f)]
    [else
     (let*-values ([(xs ws)  (sort-samples < xs ws)]
                   [(xs)  (list->vector (map fl xs))]
                   [(sum-ws)  (sum ws)]
                   [(ws)  (map (λ ([w : Real]) (fl (/ w sum-ws))) ws)]
                   [(h)  (fl h)]
                   [(max-dist)  (apply max (map (λ ([w : Flonum]) (weight-max-dist w h)) ws))]
                   [(ws)  (list->vector ws)])
       (define c (/ 1.0 (* (flsqrt pi) h)))
       ;; The range of non-zero KDE values
       (define x-min (- (vector-ref xs 0) max-dist))
       (define x-max (+ (vector-ref xs (- N 1)) max-dist))
       ;; Parameters for fast-gauss
       ;; Make the KDE functions
       (define kde/windowed (make-kde/windowed xs h ws max-dist))
       (define f
         (λ ([y : Real])
           (let ([y  (fl y)])
             (cond [(< y x-min)  0.0]
                   [(> y x-max)  0.0]
                   [else  (* c (kde/windowed y))]))))
       (values f x-min x-max))]))
