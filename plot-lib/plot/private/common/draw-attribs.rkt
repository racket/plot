#lang typed/racket/base

;; Extra font, color and style functions.

(require typed/racket/draw
         typed/racket/class racket/match racket/list
         (except-in math/base sum)
         (except-in math/flonum flsum)
         "type-doc.rkt"
         "math.rkt"
         "types.rkt"
         "sample.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Anchors

(: opposite-anchor (-> Anchor Anchor))
(define (opposite-anchor a)
  (case a
    [(top-left)  'bottom-right] [(top)  'bottom] [(top-right)  'bottom-left] [(right)  'left]
    [(bottom-right)  'top-left] [(bottom)  'top] [(bottom-left)  'top-right] [(left)  'right]
    [(center)  'center]))

;; ===================================================================================================
;; Draw paramter normalization

(: real->font-size (-> Real Byte))
(define (real->font-size size)
  (cond [(rational? size)  (define i (exact-round size))
                           (assert (min (max i 1) 255) byte?)]
        [(= size +inf.0)  255]
        [else  1]))

(: real->color-byte (-> Real Byte))
(define (real->color-byte f)
  (cond [(rational? f)  (define i (exact-floor f))
                        (assert (min (max i 0) 255) byte?)]
        [(= f +inf.0)  255]
        [else  0]))

(: make-color% (-> Byte Byte Byte (Instance Color%)))
;; Returns an immutable instance of color%. Immutable colors are faster because they don't have to
;; have immutable copies made when they're used in a dc.
(define (make-color% r g b)
  (make-color r g b))

(: make-pen% (-> Byte Byte Byte Nonnegative-Real Pen-Style (Instance Pen%)))
;; Returns an immutable instance of pen%. Same reasoning as for make-color%.
(define (make-pen% r g b w s)
  (make-pen #:color (make-color% r g b) #:width w #:style s))

(: make-brush% (-> Byte Byte Byte Brush-Style (Instance Brush%)))
;; Returns an immutable instance of brush%. Same reasoning as for make-color%.
(define (make-brush% r g b s)
  ;; A strange error in TR forces this annotation
  ((ann make-brush
        (-> [#:color (U String (Instance Color%))]
            [#:style Brush-Style]
            [#:gradient (U #f
                           (Instance Linear-Gradient%)
                           (Instance Radial-Gradient%))]
            [#:transformation (U #f (Vector (Vector Real Real Real
                                                    Real Real Real)
                                            Real Real Real Real Real))]
            [#:immutable? Any]
            (Instance Brush%)))
   #:color (make-color% r g b) #:style s))

(:: ->color (-> Color (List Real Real Real)))
(define (->color c)
  (cond
    [(list? c)    c]
    [(string? c)  (define color (send the-color-database find-color c))
                  (when (not color) (error 'decode-color "unknown color name ~e" c))
                  (->color color)]
    [(symbol? c)  (->color (symbol->string c))]
    [else  (list (send c red) (send c green) (send c blue))]))

(: color->color% (-> (List Real Real Real) (Instance Color%)))
(define (color->color% c)
  (match-define (list r g b) c)
  (make-color% (real->color-byte r) (real->color-byte g) (real->color-byte b)))

(: rgb->hsv (-> (List Real Real Real) (List Real Real Real)))
(define (rgb->hsv rgb)
  (match-define (list r g b) (map (λ ([x : Real]) (/ x 255)) rgb))
  (define mx (max r g b))
  (define mn (min r g b))
  (define c (- mx mn))
  (define h (* 60 (cond [(zero? c)  0]
                        [(= mx r)   (/ (- g b) c)]
                        [(= mx g)   (+ (/ (- b r) c) 2)]
                        [else       (+ (/ (- r g) c) 4)])))
  (list (if (h . < . 0) (+ h 360) h)
        (if (zero? mx) 0 (/ c mx))
        mx))

(: hsv->rgb (-> (List Real Real Real) (List Real Real Real)))
(define (hsv->rgb hsv)
  (match-define (list h s v) hsv)
  (define c (* v s))
  (let ([h  (/ (real-modulo h 360) 60)])
    (define x (* c (- 1 (abs (- (real-modulo h 2) 1)))))
    (define-values (r g b)
      (cond [(and (0 . <= . h) (h . < . 1))  (values c x 0)]
            [(and (1 . <= . h) (h . < . 2))  (values x c 0)]
            [(and (2 . <= . h) (h . < . 3))  (values 0 c x)]
            [(and (3 . <= . h) (h . < . 4))  (values 0 x c)]
            [(and (4 . <= . h) (h . < . 5))  (values x 0 c)]
            [else                            (values c 0 x)]))
    (define m (- v c))
    (list (* 255 (+ r m))
          (* 255 (+ g m))
          (* 255 (+ b m)))))

(: integer->hue (-> Integer Nonnegative-Integer))
(define (integer->hue n)
  (let ([n  (abs n)])
    (define i (+ (case (remainder n 6) [(0) 0] [(1) 2] [(2) 4] [(3) 1] [(4) 3] [else 5])
                 (* 6 3 (quotient n 6))))
    (remainder (* i 59) 360)))

(: integer->gray-value (-> Integer Nonnegative-Exact-Rational))
(define (integer->gray-value n)
  (* 1/7 (remainder (abs n) 8)))

(: integer->pen-color (-> Integer (List Real Real Real)))
(define (integer->pen-color n)
  (define h (integer->hue n))
  (hsv->rgb (list (- h (* 25 (sin (* (/ h 360) (* 3 pi)))))
                  1
                  (+ 1/2 (* 1/6 (sin (* (/ h 360) (* 3 pi))))))))

(: integer->brush-color (-> Integer (List Real Real Real)))
(define (integer->brush-color n)
  (define h (integer->hue n))
  (hsv->rgb (list (let ([y  (* (/ (- (sqrt (+ (/ h 60) 2)) (sqrt 2))
                                  (- (sqrt 8) (sqrt 2)))
                               6)])
                    (- h (* 15 (sin (* (/ y 6) (* 3 pi))))))
                  (+ 3/16 (* 3/32 (sin (* (/ h 360) (* 2 pi)))))
                  1)))

(: integer->gray-pen-color (-> Integer (List Real Real Real)))
(define (integer->gray-pen-color i)
  (define r (* 128 (expt (integer->gray-value i) 3/4)))
  (list r r r))

(: integer->gray-brush-color (-> Integer (List Real Real Real)))
(define (integer->gray-brush-color i)
  (define r (+ 127 (* 128 (expt (max 0 (- 1 (integer->gray-value i))) 3/4))))
  (list r r r))

(: pen-colors (Vectorof (List Byte Byte Byte)))
(define pen-colors
  (for/vector ([color  (in-list (append (list (integer->gray-pen-color 0))
                                        (build-list 120 integer->pen-color)
                                        (build-list 7 (λ ([n : Index])
                                                        (integer->gray-pen-color (- 7 n))))))])
    : (List Byte Byte Byte)
    (match-define (list r g b) color)
    (list (real->color-byte r)
          (real->color-byte g)
          (real->color-byte b))))

(: brush-colors (Vectorof (List Byte Byte Byte)))
(define brush-colors
  (for/vector ([color  (in-list (append (list (integer->gray-brush-color 0))
                                        (build-list 120 integer->brush-color)
                                        (build-list 7 (λ ([n : Index])
                                                        (integer->gray-brush-color (- 7 n))))))])
    : (List Byte Byte Byte)
    (match-define (list r g b) color)
    (list (real->color-byte r)
          (real->color-byte g)
          (real->color-byte b))))

(:: ->pen-color (-> Plot-Color (List Real Real Real)))
(define (->pen-color c)
  (cond [(exact-integer? c)  (vector-ref pen-colors (modulo c 128))]
        [else                (->color c)]))

(:: ->brush-color (-> Plot-Color (List Real Real Real)))
(define (->brush-color c)
  (cond [(exact-integer? c)  (vector-ref brush-colors (modulo c 128))]
        [else                (->color c)]))

(:: ->pen-style (-> Plot-Pen-Style Plot-Pen-Style-Sym))
(define (->pen-style s)
  (cond [(exact-integer? s)  (case (remainder (abs s) 5)
                               [(0)  'solid]
                               [(1)  'dot]
                               [(2)  'long-dash]
                               [(3)  'short-dash]
                               [else 'dot-dash])]
        [(symbol? s)  s]
        [else  (raise-type-error '->pen-style "symbol or integer" s)]))

(:: ->brush-style (-> Plot-Brush-Style Plot-Brush-Style-Sym))
(define (->brush-style s)
  (cond [(exact-integer? s)  (case (remainder (abs s) 7)
                               [(0)  'solid]
                               [(1)  'bdiagonal-hatch]
                               [(2)  'fdiagonal-hatch]
                               [(3)  'crossdiag-hatch]
                               [(4)  'horizontal-hatch]
                               [(5)  'vertical-hatch]
                               [else 'cross-hatch])]
        [(symbol? s)  s]
        [else  (raise-type-error '->brush-style "symbol or integer" s)]))

;; ===================================================================================================
;; Color functions

(:: color-seq (->* [Color Color Natural] [#:start? Boolean #:end? Boolean]
                   (Listof (List Real Real Real))))
(define (color-seq c1 c2 num #:start? [start? #t] #:end? [end? #t])
  (match-define (list r1 g1 b1) (->color c1))
  (match-define (list r2 g2 b2) (->color c2))
  (define rs (linear-seq r1 r2 num #:start? start? #:end? end?))
  (define gs (linear-seq g1 g2 num #:start? start? #:end? end?))
  (define bs (linear-seq b1 b2 num #:start? start? #:end? end?))
  (map (λ ([r : Real] [g : Real] [b : Real]) (list r g b)) rs gs bs))

(:: color-seq* (->* [(Listof Color) Natural] [#:start? Boolean #:end? Boolean]
                    (Listof (List Real Real Real))))
(define (color-seq* colors num #:start? [start? #t] #:end? [end? #t])
  (when (empty? colors) (raise-argument-error 'color-seq* "nonempty (Listof Color)" 0 colors num))
  (match-define (list (list #{rs : (Listof Real)} #{gs : (Listof Real)} #{bs : (Listof Real)}) ...)
    (map ->color colors))
  (let ([rs  (linear-seq* rs num #:start? start? #:end? end?)]
        [gs  (linear-seq* gs num #:start? start? #:end? end?)]
        [bs  (linear-seq* bs num #:start? start? #:end? end?)])
    (map (λ ([r : Real] [g : Real] [b : Real]) (list r g b)) rs gs bs)))
