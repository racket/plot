#lang typed/racket/base

;; Extra drawing functions.

(require typed/racket/draw typed/racket/class racket/match racket/list
         (except-in math/base sum)
         (except-in math/flonum flsum)
         "math.rkt"
         "utils.rkt"
         "types.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Drawing text rotated around an anchor point

(define sin45 (/ 1.0 (sqrt 2.0)))

(: draw-text/anchor (->* [(Instance DC<%>) String Real Real]
                         [Anchor Real Real]
                         Void))
(define (draw-text/anchor dc str x y [anchor 'top-left] [angle 0] [dist 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f #t 0))
  (let ([dist  (case anchor
                 [(top-left bottom-left top-right bottom-right)  (* sin45 dist)]
                 [else  dist])])
    (define dx (case anchor
                 [(top-left left bottom-left)     (- dist)]
                 [(top center bottom)             (* 1/2 width)]
                 [(top-right right bottom-right)  (+ width dist)]
                 [else  (raise-type-error 'draw-text/anchor "anchor/c" anchor)]))
    (define dy (case anchor
                 [(top-left top top-right)           (- dist)]
                 [(left center right)                (* 1/2 height)]
                 [(bottom-left bottom bottom-right)  (+ height dist)]))
    (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
    (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
    
    (send dc draw-text str (- x rdx) (- y rdy) #t 0 angle)))

(: get-text-corners/anchor (->* [(Instance DC<%>) String Real Real]
                                [Anchor Real Real]
                                (Listof (Vector Real Real))))
(define (get-text-corners/anchor dc str x y [anchor 'top-left] [angle 0] [dist 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f #t 0))
  (let ([dist  (case anchor
                 [(top-left bottom-left top-right bottom-right)  (* sin45 dist)]
                 [else  dist])])
    (: dxs (Listof Real))
    (define dxs (case anchor
                  [(top-left left bottom-left)  (list (- dist) (- width dist))]
                  [(top center bottom)          (list (* -1/2 width) (* 1/2 width))]
                  [else                         (list (- dist width) dist)]))
    (: dys (Listof Real))
    (define dys (case anchor
                  [(top-left top top-right)  (list (- dist) (- height dist))]
                  [(left center right)       (list (* -1/2 height) (* 1/2 width))]
                  [else                      (list (- dist height) dist)]))
    
    (for*/list : (Listof (Vector Real Real)) ([dx  (in-list dxs)] [dy  (in-list dys)])
      (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
      (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
      (vector (+ x rdx) (+ y rdy)))))

;; ===================================================================================================
;; Subdividing nonlinearly transformed shapes

(define subdivide-fracs '(3/7 4/7 2/7 5/7 1/7 6/7))

(: subdivide-line (-> (-> (Vectorof Real) (Vectorof Real)) (Vectorof Real) (Vectorof Real)
                      (Listof (Vectorof Real))))
(define (subdivide-line transform v1 v2)
  (let loop ([v1 v1] [v2 v2] [depth 10])
    (let/ec return : (Listof (Vectorof Real))
      (when (zero? depth) (return (list v1 v2)))
      
      (define dc-v1 (transform v1))
      (define dc-v2 (transform v2))
      (define dc-dv (v- dc-v2 dc-v1))
      (when ((vmag dc-dv) . <= . 3)
        (return (list v1 v2)))
      
      (define dv (v- v2 v1))
      (define-values (max-area vc)
        (for/fold ([max-area : Real  0] [vc : (Vectorof Real)  v1])
                  ([frac  (in-list subdivide-fracs)])
          (define test-vc (v+ (v* dv frac) v1))
          (define test-area (abs (vcross2 dc-dv (v- (transform test-vc) dc-v1))))
          (cond [(test-area . > . max-area)  (values test-area test-vc)]
                [else  (values max-area vc)])))
      (when (max-area . <= . 3) (return (list v1 v2)))
      
      ;(plot3d-subdivisions (+ (plot3d-subdivisions) 1))
      (append (loop v1 vc (- depth 1))
              (rest (loop vc v2 (- depth 1)))))))

(: subdivide-lines (-> (-> (Vectorof Real) (Vectorof Real)) (Listof (Vectorof Real))
                       (Listof (Vectorof Real))))
(define (subdivide-lines transform vs)
  (append
   (append*
    (for/list : (Listof (Listof (Vectorof Real))) ([v1  (in-list vs)] [v2  (in-list (rest vs))])
      (define line-vs (subdivide-line transform v1 v2))
      (drop-right line-vs 1)))
   (list (last vs))))

(: subdivide-polygon
   (All (L) (case-> (-> (-> (Vectorof Real) (Vectorof Real)) (Listof (Vectorof Real))
                        (Listof (Vectorof Real)))
                    (-> (-> (Vectorof Real) (Vectorof Real))
                        (Listof (Vectorof Real))
                        (Listof L)
                        (Values (Listof (Vectorof Real))
                                (Listof L))))))
(define subdivide-polygon
  (case-lambda
    [(transform vs)
     (reverse
      (for/fold ([vs : (Listof (Vectorof Real))  empty])
                ([v1  (in-list (cons (last vs) vs))]
                 [v2  (in-list vs)])
        (define line-vs (rest (subdivide-line transform v1 v2)))
        (append (reverse line-vs) vs)))]
    [(transform vs ls)
     (define-values (new-vs new-ls)
       (for/fold ([vs : (Listof (Vectorof Real))  empty]
                  [ls : (Listof L)  empty])
                 ([v1  (in-list (cons (last vs) vs))]
                  [v2  (in-list vs)]
                  [l   (in-list ls)])
         (define line-vs (rest (subdivide-line transform v1 v2)))
         (values (append (reverse line-vs) vs)
                 (append (make-list (length line-vs) l) ls))))
     (values (reverse new-vs)
             (reverse new-ls))]))

;; ===================================================================================================
;; Fixpoint margin computation

;; In calculating margins in 2d-plot-area% and 3d-plot-area%, we have a mutual dependence problem:
;; 1. We can't set the margins without knowing where the ticks and axis labels will be drawn.
;; 2. We can't determine the tick and label angles (and thus their vertexes) without the margins.

;; The margins could be solved exactly using algebra and trigonometry, but the solutions wouldn't
;; be robust, as small changes to the layout algorithms would invalidate them.

;; So we use a fixpoint solution: iterate
;; 1. Getting tick and label vertexes ('get-vs' below); then
;; 2. Calculating new margins by how far off the dc the vertexes would be.

;; As long as this process is monotone and bounded, the distance off the dc is zero in the limit. In
;; practice, only a few iterations drives this distance to less than 1 drawing unit.

(: margin-fixpoint (-> Real Real Real Real Real Real Real Real
                       (-> Real Real Real Real (Listof (Vectorof Real)))
                       (Values Real Real Real Real)))
(define (margin-fixpoint x-min x-max y-min y-max
                         init-left init-right init-top init-bottom
                         get-vs)
  (let/ec return : (Values Real Real Real Real)
    (for/fold ([left   : Real  init-left]
               [right  : Real  init-right]
               [top    : Real  init-top]
               [bottom : Real  init-bottom])
              ([i  (in-range 3)])
      (match-define (list (vector #{xs : (Listof Real)} #{ys : (Listof Real)}) ...)
        (get-vs left right top bottom))
      (define param-x-min (apply min x-min xs))
      (define param-x-max (apply max (sub1 x-max) xs))
      (define param-y-min (apply min y-min ys))
      (define param-y-max (apply max (sub1 y-max) ys))
      
      (define new-left (round (+ left (- x-min param-x-min))))
      (define new-right (round (- right (- (sub1 x-max) param-x-max))))
      (define new-top (round (+ top (- y-min param-y-min))))
      (define new-bottom (round (- bottom (- (sub1 y-max) param-y-max))))
      
      ;; Not enough space?
      (define area-x-min (+ x-min new-left))
      (define area-x-max (- x-max new-right))
      (define area-y-min (+ y-min new-top))
      (define area-y-max (- y-max new-bottom))
      (when (or (area-x-min . > . area-x-max)
                (area-y-min . > . area-y-max))
        (return init-left init-right init-top init-bottom))
      
      ;; Early out: if the margins haven't changed much, another iteration won't change them more
      ;; (hopefully)
      (when (and (= left new-left) (= right new-right)
                 (= top new-top) (= bottom new-bottom))
        (return new-left new-right new-top new-bottom))
      
      (values new-left new-right new-top new-bottom))))

;; ===================================================================================================
;; Origin-neutral pen styles

(struct pen-style ([length : Flonum] [ps : (Listof Flonum)]) #:transparent)

(: make-pen-style (-> (Listof Natural) pen-style))
(define (make-pen-style diff-ps)
  (let* ([diff-ps  (map fl diff-ps)]
         [diff-ps  (if (even? (length diff-ps)) diff-ps (append diff-ps diff-ps))])
    (define ps (map fl (cumulative-sum diff-ps)))
    (define len (last ps))
    (pen-style len ps)))

(define long-dash-pen-style (make-pen-style '(5 4)))
(define short-dash-pen-style (make-pen-style '(3 2)))
(define dot-pen-style (make-pen-style '(1 2)))
(define dot-dash-pen-style (make-pen-style '(1 3 4 3)))

(: scale-pen-style (-> pen-style Real pen-style))
(define (scale-pen-style sty scale)
  (let ([scale  (fl scale)])
    (match-define (pen-style len ps) sty)
    (pen-style (* scale len) (map (λ ([p : Flonum]) (fl* scale p)) ps))))

(: cons-fl (-> (Pair Real Real) (Pair Flonum Flonum)))
(define (cons-fl v)
  (match-define (cons x1 y1) v)
  (cons (fl x1) (fl y1)))

(: cons-fl= (-> (Pair Flonum Flonum) (Pair Flonum Flonum) Boolean))
(define (cons-fl= v1 v2)
  (match-define (cons x1 y1) v1)
  (match-define (cons x2 y2) v2)
  (and (= x1 x2) (= y1 y2)))

(: segment-reverse (All (A) (-> (Listof (Listof A)) (Listof (Listof A)))))
(define (segment-reverse seg)
  (reverse (map (inst reverse A) seg)))

(: segment-join (All (A) (-> (Listof (Listof A)) (Listof (Listof A)) (Listof (Listof A)))))
(define (segment-join s1 s2)
  (let ([s1  (drop-right s1 1)]
        [a  (last s1)]
        [b  (first s2)]
        [s2  (rest s2)])
    (append s1 (list (append a (rest b))) s2)))

(: join-styled-segments (-> (Listof (Listof (Listof (Pair Flonum Flonum))))
                            (Listof (Listof (Listof (Pair Flonum Flonum))))))
(define (join-styled-segments segments)
  (let ([segments  (filter (compose not empty?) segments)])
    (if (empty? segments)
        empty
        (match-let ([(cons current-segment segments)  segments])
          (let loop ([current-segment current-segment] [segments segments])
            (cond [(empty? segments)  (list current-segment)]
                  [else
                   (define lst (last (last current-segment)))
                   (match-let ([(cons segment segments)  segments])
                     (define fst (first (first segment)))
                     (if (cons-fl= lst fst)
                         (loop ((inst segment-join (Pair Flonum Flonum)) current-segment segment)
                               segments)
                         (cons current-segment (loop segment segments))))]))))))

(: styled-segment* (-> Flonum Flonum Flonum Flonum pen-style (-> Flonum Flonum (Pair Flonum Flonum))
                       (Listof (Listof (Pair Flonum Flonum)))))
(define (styled-segment* x1 y1 x2 y2 sty pair)
  (match-define (pen-style len (cons p rest-ps)) sty)
  (define start-x (* len (floor (/ x1 len))))
  (define m (/ (- y2 y1) (- x2 x1)))
  (define b (- y1 (* m x1)))
  (let loop ([xa start-x]
             [base-x 0.0]
             [ps rest-ps]
             [on? #t]
             [res : (Listof (Listof (Pair Flonum Flonum)))  empty])
    (let-values ([(base-x ps)  (cond [(empty? ps)  (values (+ base-x len) rest-ps)]
                                     [else         (values base-x ps)])])
      (cond [(xa . fl>= . x2)  (reverse res)]
            [else
             (match-let ([(cons p ps)  ps])
               (define xb (+ start-x (+ p base-x)))
               (cond [(and on? (xb . fl>= . x1))
                      (define v (let ([xa  (max x1 xa)]
                                      [xb  (min x2 xb)])
                                  (define ya (if (= x1 xa) y1 (+ (* m xa) b)))
                                  (define yb (if (= x2 xb) y2 (+ (* m xb) b)))
                                  (list (pair xa ya) (pair xb yb))))
                      (loop xb base-x ps (not on?) (cons v res))]
                     [else  (loop xb base-x ps (not on?) res)]))]))))

(: styled-segment (-> Flonum Flonum Flonum Flonum pen-style (Listof (Listof (Pair Flonum Flonum)))))
(define (styled-segment x1 y1 x2 y2 sty)
  (define dx (abs (- x2 x1)))
  (define dy (abs (- y2 y1)))
  (cond [(and (= dx 0.0) (= dy 0.0))  (list (list (cons x1 y1) (cons x2 y2)))]
        [(dx . > . dy)
         (define reverse? (x1 . fl> . x2))
         (let-values ([(x1 y1)  (if reverse? (values x2 y2) (values x1 y1))]
                      [(x2 y2)  (if reverse? (values x1 y1) (values x2 y2))])
           (define segment (styled-segment* x1 y1 x2 y2 sty cons))
           (if reverse? (segment-reverse segment) segment))]
        [else
         (define reverse? (y1 . fl> . y2))
         (let-values ([(x1 y1)  (if reverse? (values x2 y2) (values x1 y1))]
                      [(x2 y2)  (if reverse? (values x1 y1) (values x2 y2))])
           (define segment (styled-segment* y1 x1 y2 x2 sty (λ (y x) (cons x y))))
           (if reverse? (segment-reverse segment) segment))]))

(: symbol->style (-> (U 'dot 'long-dash 'short-dash 'dot-dash) pen-style))
(define (symbol->style style-sym)
  (case style-sym
    [(long-dash)   long-dash-pen-style]
    [(short-dash)  short-dash-pen-style]
    [(dot)         dot-pen-style]
    [(dot-dash)    dot-dash-pen-style]))

(: draw-line/pen-style (-> (Instance DC<%>) Real Real Real Real Plot-Pen-Style-Sym Void))
(define (draw-line/pen-style dc x1 y1 x2 y2 style-sym)
  (case style-sym
    [(transparent)  (void)]
    [(solid)        (send dc draw-line x1 y1 x2 y2)]
    [else
     (let ([x1  (fl x1)] [y1  (fl y1)] [x2  (fl x2)] [y2  (fl y2)])
       (define sty (symbol->style style-sym))
       (define pen (send dc get-pen))
       (define scale (max 1.0 (fl (send pen get-width))))
       (define vss (styled-segment x1 y1 x2 y2 (scale-pen-style sty scale)))
       (for ([vs  (in-list vss)] #:when (not (empty? vs)))
         (match-define (list (cons xa ya) (cons xb yb)) vs)
         (send dc draw-line xa ya xb yb)))]))

(: draw-lines* (-> (Instance DC<%>) (Listof (Pair Flonum Flonum)) pen-style Void))
(define (draw-lines* dc vs sty)
  (define vss
    (append* (join-styled-segments
              (for/list ([v1  (in-list vs)] [v2  (in-list (rest vs))])
                (match-define (cons x1 y1) v1)
                (match-define (cons x2 y2) v2)
                (styled-segment x1 y1 x2 y2 sty)))))
  (for ([vs  (in-list vss)])
    (match vs
      [(list (cons x1 y1) (cons x2 y2))  (send dc draw-line x1 y1 x2 y2)]
      [_  (send dc draw-lines vs)])))

(: draw-lines/pen-style (-> (Instance DC<%>) (Listof (Pair Real Real)) Plot-Pen-Style-Sym Void))
(define (draw-lines/pen-style dc vs style-sym)
  (cond [(or (empty? vs) (eq? style-sym 'transparent))  (void)]
        [else
         (let ([vs  (map cons-fl vs)])
           (cond [(eq? style-sym 'solid)  (send dc draw-lines vs)]
                 [else
                  (define pen (send dc get-pen))
                  (define scale (max 1.0 (fl (send pen get-width))))
                  (define sty (scale-pen-style (symbol->style style-sym) scale))
                  (draw-lines* dc vs sty)]))]))

;; ===================================================================================================
;; Drawing a bitmap using 2x supersampling

(: draw-bitmap/supersampling (-> (-> (Instance DC<%>) Any) Positive-Integer Positive-Integer
                                 (Instance Bitmap%)))
(define (draw-bitmap/supersampling draw width height)
  (define bm (make-bitmap width height #:backing-scale 2))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-alignment-scale 2)
  (draw dc)
  bm)

(: draw-bitmap (-> (-> (Instance DC<%>) Any) Positive-Integer Positive-Integer
                   (Instance Bitmap%)))
(define (draw-bitmap draw width height)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (draw dc)
  bm)
