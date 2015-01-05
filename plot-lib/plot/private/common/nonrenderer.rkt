#lang typed/racket/base

(require racket/list
         "type-doc.rkt"
         "math.rkt"
         "ticks.rkt"
         "plot-element.rkt"
         "utils.rkt")

(provide (all-defined-out))

(struct nonrenderer plot-element () #:transparent)

(: x-ticks-fun (-> (Listof tick) Boolean Ticks-Fun))
(define ((x-ticks-fun ts far?) r)
  (let-values ([(ts far-ts)  (if far? (values empty ts) (values ts empty))])
    (cond [(= (vector-length r) 2)  (list ts far-ts empty empty)]
          [(= (vector-length r) 3)  (list ts far-ts empty empty empty empty)]
          [else  (raise-argument-error 'x-ticks-fun "vector of length 2 or 3" r)])))

(: y-ticks-fun (-> (Listof tick) Boolean Ticks-Fun))
(define ((y-ticks-fun ts far?) r)
  (let-values ([(ts far-ts)  (if far? (values empty ts) (values ts empty))])
    (cond [(= (vector-length r) 2)  (list empty empty ts far-ts)]
          [(= (vector-length r) 3)  (list empty empty ts far-ts empty empty)]
          [else  (raise-argument-error 'y-ticks-fun "vector of length 2 or 3" r)])))

(: z-ticks-fun (-> (Listof tick) Boolean Ticks-Fun))
(define ((z-ticks-fun ts far?) r)
  (let-values ([(ts far-ts)  (if far? (values empty ts) (values ts empty))])
    (cond [(= (vector-length r) 3)  (list empty empty empty empty ts far-ts)]
          [else  (raise-argument-error 'z-ticks-fun "vector of length 2 or 3" r)])))

(:: x-ticks (->* [(Listof tick)] [#:far? Boolean] nonrenderer))
(define (x-ticks ts #:far? [far? #f])
  (nonrenderer #f #f (x-ticks-fun ts far?)))

(:: y-ticks (->* [(Listof tick)] [#:far? Boolean] nonrenderer))
(define (y-ticks ts #:far? [far? #f])
  (nonrenderer #f #f (y-ticks-fun ts far?)))

(:: z-ticks (->* [(Listof tick)] [#:far? Boolean] nonrenderer))
(define (z-ticks ts #:far? [far? #f])
  (nonrenderer #f #f (z-ticks-fun ts far?)))

(:: invisible-rect (-> (U Real #f) (U Real #f) (U Real #f) (U Real #f) nonrenderer))
(define (invisible-rect x-min x-max y-min y-max)
  (define fail (make-raise-argument-error 'invisible-rect x-min x-max y-min y-max))
  (cond [(and x-min (not (rational? x-min)))  (fail "#f or rational" 0)]
        [(and x-max (not (rational? x-max)))  (fail "#f or rational" 1)]
        [(and y-min (not (rational? y-min)))  (fail "#f or rational" 2)]
        [(and y-max (not (rational? y-max)))  (fail "#f or rational" 3)]
        [else  (nonrenderer (vector (ivl x-min x-max) (ivl y-min y-max)) #f #f)]))

(:: invisible-rect3d (-> (U Real #f) (U Real #f) (U Real #f) (U Real #f) (U Real #f) (U Real #f)
                         nonrenderer))
(define (invisible-rect3d x-min x-max y-min y-max z-min z-max)
  (define fail (make-raise-argument-error 'invisible-rect3d x-min x-max y-min y-max z-min z-max))
  (cond [(and x-min (not (rational? x-min)))  (fail "#f or rational" 0)]
        [(and x-max (not (rational? x-max)))  (fail "#f or rational" 1)]
        [(and y-min (not (rational? y-min)))  (fail "#f or rational" 2)]
        [(and y-max (not (rational? y-max)))  (fail "#f or rational" 3)]
        [(and z-min (not (rational? z-min)))  (fail "#f or rational" 4)]
        [(and z-max (not (rational? z-max)))  (fail "#f or rational" 5)]
        [else  (nonrenderer (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f #f)]))
