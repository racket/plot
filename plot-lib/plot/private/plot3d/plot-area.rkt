#lang typed/racket/base

(require typed/racket/class typed/racket/draw racket/match racket/list racket/math racket/flonum
         (only-in math fl vector->flvector)
         "../common/type-doc.rkt"
         "../common/types.rkt"
         "../common/math.rkt"
         "../common/plot-device.rkt"
         "../common/ticks.rkt"
         "../common/draw-attribs.rkt"
         "../common/draw.rkt"
         "../common/axis-transform.rkt"
         "../common/parameters.rkt"
         "../common/utils.rkt"
         "vector.rkt"
         "clip.rkt"
         "bsp-trees.rkt"
         "bsp.rkt")

(provide (all-defined-out)
         plot3d-back-layer
         plot3d-area-layer
         plot3d-front-layer
         )

(define plot3d-back-layer 2)
(define plot3d-area-layer 1)
(define plot3d-front-layer 0)

(: plot3d-subdivisions (Parameterof Natural))
(define plot3d-subdivisions (make-parameter 0))

(struct render-tasks ([structural-shapes : (HashTable Integer (Listof BSP-Shape))]
                      [detail-shapes : (HashTable Integer (Listof BSP-Shape))]
                      [bsp-trees : (HashTable Integer BSP-Tree)]))

(struct data ([alpha : Nonnegative-Real]) #:transparent)

(struct poly-data data ([center : FlVector]
                        [pen-color : (List Real Real Real)]
                        [pen-width : Nonnegative-Real]
                        [pen-style : Plot-Pen-Style-Sym]
                        [brush-color : (List Real Real Real)]
                        [brush-style : Plot-Brush-Style-Sym]
                        [face : (U 'front 'back 'both)])
  #:transparent)

(struct line-data data ([pen-color : (List Real Real Real)]
                        [pen-width : Nonnegative-Real]
                        [pen-style : Plot-Pen-Style-Sym])
  #:transparent)

(struct text-data data ([anchor : Anchor]
                        [angle : Real]
                        [dist : Real]
                        [str : String]
                        [font-size : Nonnegative-Real]
                        [font-family : Font-Family]
                        [color : (List Real Real Real)]
                        [outline? : Boolean])
  #:transparent)

(struct glyph-data data ([symbol : Point-Sym]
                         [size : Nonnegative-Real]
                         [pen-color : (List Real Real Real)]
                         [pen-width : Nonnegative-Real]
                         [pen-style : Plot-Pen-Style-Sym]
                         [brush-color : (List Real Real Real)]
                         [brush-style : Plot-Brush-Style-Sym])
  #:transparent)

(struct arrow-data data ([start : FlVector]
                         [end : FlVector]
                         [outline-color : (List Real Real Real)]
                         [pen-color : (List Real Real Real)]
                         [pen-width : Nonnegative-Real]
                         [pen-style : Plot-Pen-Style-Sym])
  #:transparent)


(: structural-shape? (-> BSP-Shape Boolean))
;; Determines whether a shape is view-independent, and thus used to *create* BSP trees
;; Other shapes are view-dependent, so they are inserted into BSP trees before each refresh
(define (structural-shape? s)
  (poly? s))

(deftype 3D-Plot-Area%
  (Class (init-field [bounds-rect Rect]
                     [rx-ticks (Listof tick)]
                     [rx-far-ticks (Listof tick)]
                     [ry-ticks (Listof tick)]
                     [ry-far-ticks (Listof tick)]
                     [rz-ticks (Listof tick)]
                     [rz-far-ticks (Listof tick)])
         (init-field [dc (Instance DC<%>)]
                     [dc-x-min Real]
                     [dc-y-min Real]
                     [dc-x-size Nonnegative-Real]
                     [dc-y-size Nonnegative-Real])
         [put-clip-rect (-> Rect Void)]
         [clear-clip-rect (-> Void)]
         [get-x-ticks (-> (Listof tick))]
         [get-y-ticks (-> (Listof tick))]
         [get-z-ticks (-> (Listof tick))]
         [get-x-far-ticks (-> (Listof tick))]
         [get-y-far-ticks (-> (Listof tick))]
         [get-z-far-ticks (-> (Listof tick))]
         [get-bounds-rect (-> Rect)]
         [get-clip-rect (-> Rect)]
         [get-render-tasks  (-> render-tasks)]
         [set-render-tasks  (-> render-tasks Void)]
         [start-plot (-> Void)]
         [start-renderer (-> Rect Void)]
         [end-renderers (-> Void)]
         [draw-legend (-> (Listof legend-entry) Void)]
         [end-plot (-> Void)]
         [put-alpha  (-> Nonnegative-Real Void)]
         [put-pen (-> Plot-Color Nonnegative-Real Plot-Pen-Style Void)]
         [put-major-pen (->* [] [Plot-Pen-Style] Void)]
         [put-minor-pen (->* [] [Plot-Pen-Style] Void)]
         [put-brush (-> Plot-Color Plot-Brush-Style Void)]
         [put-background (-> Plot-Color Void)]
         [put-font-size (-> Nonnegative-Real Void)]
         [put-font-face (-> (U #f String) Void)]
         [put-font-family (-> Font-Family Void)]
         [put-font-attribs (-> Nonnegative-Real (U #f String) Font-Family Void)]
         [put-text-foreground (-> Plot-Color Void)]
         [reset-drawing-params (-> Void)]
         [put-lines (-> (Listof (Vectorof Real)) Void)]
         [put-line (-> (Vectorof Real) (Vectorof Real) Void)]
         [put-polygon (->* [(Listof (Vectorof Real))] [(U 'front 'back 'both) (Listof Boolean)] Void)]
         [put-rect (-> Rect Void)]
         [put-text (->* [String (Vectorof Real)] [Anchor Real Real Boolean Integer] Void)]
         [put-glyphs (->* [(Listof (Vectorof Real)) Point-Sym Nonnegative-Real] [Integer] Void)]
         [put-arrow (-> (Vectorof Real) (Vectorof Real) Void)]
         ))

(: 3d-plot-area% 3D-Plot-Area%)
(define 3d-plot-area%
  (class object%
    (init-field bounds-rect rx-ticks rx-far-ticks ry-ticks ry-far-ticks rz-ticks rz-far-ticks)
    (init-field dc dc-x-min dc-y-min dc-x-size dc-y-size)
    (super-new)
    
    (: pd (Instance Plot-Device%))
    (define pd (make-object plot-device% dc dc-x-min dc-y-min dc-x-size dc-y-size))
    (send pd reset-drawing-params)
    
    (: char-height Exact-Rational)
    (: half-char-height Exact-Rational)
    (define char-height (send pd get-char-height))
    (define half-char-height (* 1/2 char-height))
    
    (define: x-min : Real  0)
    (define: x-max : Real  0)
    (define: y-min : Real  0)
    (define: y-max : Real  0)
    (define: z-min : Real  0)
    (define: z-max : Real  0)
    (let ()
      (match-define (vector (ivl x-min-val x-max-val) 
                            (ivl y-min-val y-max-val)
                            (ivl z-min-val z-max-val))
        bounds-rect)
      (cond [(and x-min-val x-max-val y-min-val y-max-val z-min-val z-max-val)
             (set! x-min x-min-val)
             (set! x-max x-max-val)
             (set! y-min y-min-val)
             (set! y-max y-max-val)
             (set! z-min z-min-val)
             (set! z-max z-max-val)]
            [else
             (raise-argument-error '3d-plot-area% "rect-known?" bounds-rect)]))
    
    (: x-size Real)
    (: y-size Real)
    (: z-size Real)
    (: x-mid Real)
    (: y-mid Real)
    (: z-mid Real)
    (define x-size (- x-max x-min))
    (define y-size (- y-max y-min))
    (define z-size (- z-max z-min))
    (define x-mid (* 1/2 (+ x-min x-max)))
    (define y-mid (* 1/2 (+ y-min y-max)))
    (define z-mid (* 1/2 (+ z-min z-max)))
    
    (: clipping? Boolean)
    (: clip-x-min Real)
    (: clip-x-max Real)
    (: clip-y-min Real)
    (: clip-y-max Real)
    (: clip-z-min Real)
    (: clip-z-max Real)
    (define clipping? #f)
    (define clip-x-min x-min)
    (define clip-x-max x-max)
    (define clip-y-min y-min)
    (define clip-y-max y-max)
    (define clip-z-min z-min)
    (define clip-z-max z-max)
    
    (define/public (put-clip-rect rect)
      (match-define (vector (ivl rx-min rx-max) (ivl ry-min ry-max) (ivl rz-min rz-max)) rect)
      (define cx-min (if rx-min (max* x-min rx-min) x-min))
      (define cx-max (if rx-max (min* x-max rx-max) x-max))
      (define cy-min (if ry-min (max* y-min ry-min) y-min))
      (define cy-max (if ry-max (min* y-max ry-max) y-max))
      (define cz-min (if rz-min (max* z-min rz-min) z-min))
      (define cz-max (if rz-max (min* z-max rz-max) z-max))
      (let ([cx-min  (min* cx-min cx-max)]
            [cx-max  (max* cx-min cx-max)]
            [cy-min  (min* cy-min cy-max)]
            [cy-max  (max* cy-min cy-max)]
            [cz-min  (min* cz-min cz-max)]
            [cz-max  (max* cz-min cz-max)])
        (set! clip-x-min cx-min)
        (set! clip-x-max cx-max)
        (set! clip-y-min cy-min)
        (set! clip-y-max cy-max)
        (set! clip-z-min cz-min)
        (set! clip-z-max cz-max))
      (set! clipping? #t))
    
    (define/public (clear-clip-rect) (set! clipping? #f))
    
    (: in-bounds? (-> (Vectorof Real) Boolean))
    (define/private (in-bounds? v)
      (or (not clipping?) (point-in-bounds? v
                                            clip-x-min clip-x-max
                                            clip-y-min clip-y-max
                                            clip-z-min clip-z-max)))
    
    (define/public (get-x-ticks) x-ticks)
    (define/public (get-y-ticks) y-ticks)
    (define/public (get-z-ticks) z-ticks)
    (define/public (get-x-far-ticks) x-far-ticks)
    (define/public (get-y-far-ticks) y-far-ticks)
    (define/public (get-z-far-ticks) z-far-ticks)
    
    (define/public (get-bounds-rect) bounds-rect)
    
    (define/public (get-clip-rect)
      (if clipping?
          (vector (ivl clip-x-min clip-x-max) (ivl clip-y-min clip-y-max) (ivl clip-z-min clip-z-max))
          bounds-rect))
    
    (: angle Real)
    (: altitude Real)
    (: theta Real)
    (: rho Real)
    (define angle (plot3d-angle))
    (define altitude (plot3d-altitude))
    ;; FLOATING-POINT HACK: Adding an epsilon to the angle ensures that, when it is 90, 180
    ;; or 270, the x/y/z tick labels are drawn on the left side.
    (define theta (+ (degrees->radians angle) 0.00001))
    (define rho (degrees->radians altitude))
    
    ;; There are four coordinate systems:
    ;;  1. Plot coordinates (original, user-facing coordinate system)
    ;;  2. Normalized coordinates (from plot coordinates: for each axis: transform, center, and scale
    ;;     to [-0.5,0.5]) - these are always flvectors
    ;;  3. View coordinates (from normalized coordinates: rotate)
    ;;  4. Device context coordinates (from view coordinates: project to 2D)
    
    (define: fx : (-> Real Real)  (λ ([x : Real]) x))
    (define: fy : (-> Real Real)  (λ ([y : Real]) y))
    (define: fz : (-> Real Real)  (λ ([z : Real]) z))
    (match-let
        ([(invertible-function fx-val _)  (apply-axis-transform (plot-x-transform) x-min x-max)]
         [(invertible-function fy-val _)  (apply-axis-transform (plot-y-transform) y-min y-max)]
         [(invertible-function fz-val _)  (apply-axis-transform (plot-z-transform) z-min z-max)])
      (set! fx fx-val)
      (set! fy fy-val)
      (set! fz fz-val))
    
    (: identity-transforms? Boolean)
    (define identity-transforms?
      (and (equal? (plot-x-transform) id-transform)
           (equal? (plot-y-transform) id-transform)
           (equal? (plot-z-transform) id-transform)))
    
    (: plot->norm (-> (Vectorof Real) FlVector))
    (define/private (plot->norm v)
      (if identity-transforms?
          (match v
            [(vector (? rational? x) (? rational? y) (? rational? z))
             (flvector (fl (/ (- x x-mid) x-size))
                       (fl (/ (- y y-mid) y-size))
                       (fl (/ (- z z-mid) z-size)))]
            [(vector x y z)
             (flvector +nan.0 +nan.0 +nan.0)])
          (match v
            [(vector (? rational? x) (? rational? y) (? rational? z))
             (let ([x  (fx x)] [y  (fy y)] [z  (fz z)])
               (flvector (if (rational? x) (fl (/ (- (inexact->exact x) x-mid) x-size)) +nan.0)
                         (if (rational? y) (fl (/ (- (inexact->exact y) y-mid) y-size)) +nan.0)
                         (if (rational? z) (fl (/ (- (inexact->exact z) z-mid) z-size)) +nan.0)))]
            [(vector x y z)
             (flvector +nan.0 +nan.0 +nan.0)])))
    
    (: rotate-theta-matrix M3)
    (: rotate-rho-matrix M3)
    (: rotation-matrix M3)
    (define rotate-theta-matrix (m3-rotate-z theta))
    (define rotate-rho-matrix (m3-rotate-x rho))
    (define rotation-matrix (m3* rotate-rho-matrix rotate-theta-matrix))
    
    (: norm->view (-> FlVector FlVector))
    (: plot->view (-> (Vectorof Real) FlVector))
    (: plot->view/no-rho (-> (Vectorof Real) FlVector))
    (: norm->view/no-rho (-> FlVector FlVector))
    (: rotate/rho (-> FlVector FlVector))
    (define/private (norm->view v) (m3-apply rotation-matrix v))
    (define/private (plot->view v) (norm->view (plot->norm v)))
    (define/private (plot->view/no-rho v) (m3-apply rotate-theta-matrix (plot->norm v)))
    (define/private (norm->view/no-rho v) (m3-apply rotate-theta-matrix v))
    (define/private (rotate/rho v) (m3-apply rotate-rho-matrix v))
    
    (: unrotation-matrix M3)
    (: view->norm (-> FlVector FlVector))
    (define unrotation-matrix (m3-transpose rotation-matrix))
    (define/private (view->norm v) (m3-apply unrotation-matrix v))
    
    (: plot->dc (-> (Vectorof Real) (Vectorof Real)))
    (: norm->dc (-> FlVector (Vectorof Real)))    
    (define/private (plot->dc v) (view->dc (plot->view v)))
    (define/private (norm->dc v) (view->dc (norm->view v)))
    
    (define: view-x-size : Real  0)
    (define: view-y-size : Real  0)
    (define: view-z-size : Real  0)
    (match-let ([(vector view-x-ivl view-y-ivl view-z-ivl)
                 (bounding-rect
                  (map (λ ([v : (Vectorof Real)]) (flv3->v (plot->view v)))
                       (list (vector x-min y-min z-min) (vector x-min y-min z-max)
                             (vector x-min y-max z-min) (vector x-min y-max z-max)
                             (vector x-max y-min z-min) (vector x-max y-min z-max)
                             (vector x-max y-max z-min) (vector x-max y-max z-max))))])
      (define view-x-size-val (assert (ivl-length view-x-ivl) values))
      (define view-y-size-val (assert (ivl-length view-y-ivl) values))
      (define view-z-size-val (assert (ivl-length view-z-ivl) values))
      (set! view-x-size view-x-size-val)
      (set! view-y-size view-y-size-val)
      (set! view-z-size view-z-size-val))
    
    (: make-view->dc (-> Real Real Real Real (-> FlVector (Vectorof Real))))
    (define/private (make-view->dc left right top bottom)
      (define area-x-min left)
      (define area-x-max (- dc-x-size right))
      (define area-y-min top)
      (define area-y-max (- dc-y-size bottom))
      (define area-x-mid (* 1/2 (+ area-x-min area-x-max)))
      (define area-y-mid (* 1/2 (+ area-y-min area-y-max)))
      (define area-per-view-x (/ (- area-x-max area-x-min) view-x-size))
      (define area-per-view-z (/ (- area-y-max area-y-min) view-z-size))
      (let ([area-x-mid  (fl area-x-mid)]
            [area-y-mid  (fl area-y-mid)]
            [area-per-view-x  (fl area-per-view-x)]
            [area-per-view-z  (fl area-per-view-z)])
       (λ (v)
         (define x (flvector-ref v 0))
         (define z (flvector-ref v 2))
         (vector (fl+ area-x-mid (fl* x area-per-view-x))
                 (fl- area-y-mid (fl* z area-per-view-z))))))
    
    (: init-top-margin Real)
    (define init-top-margin (if (and (plot-decorations?) (plot-title)) (* 3/2 char-height) 0))
    
    ;; Initial view->dc
    (: view->dc (-> FlVector (Vectorof Real)))
    (define view->dc (make-view->dc 0 0 init-top-margin 0))
    
    (: x-axis-angle (-> Real))
    (define (x-axis-angle)
      (match-define (vector dx dy) (v- (norm->dc (flvector 0.5 0.0 0.0))
                                       (norm->dc (flvector -0.5 0.0 0.0))))
      (- (atan2 (- dy) dx)))
    
    (: y-axis-angle (-> Real))
    (define (y-axis-angle)
      (match-define (vector dx dy) (v- (norm->dc (flvector 0.0 0.5 0.0))
                                       (norm->dc (flvector 0.0 -0.5 0.0))))
      (- (atan2 (- dy) dx)))
    
    (: x-axis-dir (-> (Vectorof Real)))
    (define (x-axis-dir)
      (vnormalize (v- (norm->dc (flvector 0.5 0.0 0.0))
                      (norm->dc (flvector -0.5 0.0 0.0)))))
    
    (: y-axis-dir (-> (Vectorof Real)))
    (define (y-axis-dir)
      (vnormalize (v- (norm->dc (flvector 0.0 0.5 0.0))
                      (norm->dc (flvector 0.0 -0.5 0.0)))))
    
    ;; ===============================================================================================
    ;; Tick and label constants
    
    (: tick-radius Real)
    (: half-tick-radius Real)
    (define tick-radius (* 1/2 (plot-tick-size)))
    (define half-tick-radius (* 1/2 tick-radius))
    
    (: x-axis-y-min? Boolean)
    (: y-axis-x-min? Boolean)
    (define x-axis-y-min? ((cos theta) . >= . 0))  ; #t iff x near labels should be drawn at y-min
    (define y-axis-x-min? ((sin theta) . >= . 0))  ; #t iff y near labels should be drawn at x-min
    
    (: x-axis-y Real)
    (: y-axis-x Real)
    (: z-axis-x Real)
    (: z-axis-y Real)
    (define x-axis-y (if x-axis-y-min? y-min y-max))
    (define y-axis-x (if y-axis-x-min? x-min x-max))
    (define z-axis-x (if x-axis-y-min? x-min x-max))
    (define z-axis-y (if y-axis-x-min? y-max y-min))
    
    (: x-far-axis-y Real)
    (: y-far-axis-x Real)
    (: z-far-axis-x Real)
    (: z-far-axis-y Real)
    (define x-far-axis-y (if x-axis-y-min? y-max y-min))
    (define y-far-axis-x (if y-axis-x-min? x-max x-min))
    (define z-far-axis-x (if x-axis-y-min? x-max x-min))
    (define z-far-axis-y (if y-axis-x-min? y-min y-max))
    
    (: x-axis-norm-y Flonum)
    (: y-axis-norm-x Flonum)
    (: z-axis-norm-x Flonum)
    (: z-axis-norm-y Flonum)
    (define x-axis-norm-y (if x-axis-y-min? -0.5 0.5))
    (define y-axis-norm-x (if y-axis-x-min? -0.5 0.5))
    (define z-axis-norm-x (if x-axis-y-min? -0.5 0.5))
    (define z-axis-norm-y (if y-axis-x-min? 0.5 -0.5))
    
    (: x-far-axis-norm-y Flonum)
    (: y-far-axis-norm-x Flonum)
    (: z-far-axis-norm-x Flonum)
    (: z-far-axis-norm-y Flonum)
    (define x-far-axis-norm-y (if x-axis-y-min? 0.5 -0.5))
    (define y-far-axis-norm-x (if y-axis-x-min? 0.5 -0.5))
    (define z-far-axis-norm-x (if x-axis-y-min? 0.5 -0.5))
    (define z-far-axis-norm-y (if y-axis-x-min? -0.5 0.5))
    
    (: near-dist^2 Real)
    (define near-dist^2 (sqr (* 3 (plot-line-width))))
    
    (: vnear? (-> (Vectorof Real) (Vectorof Real) Boolean))
    (define/private (vnear? v1 v2)
      ((vmag^2 (v- (plot->dc v1) (plot->dc v2))) . <= . near-dist^2))
    
    (: x-ticks-near? (-> Real (-> pre-tick pre-tick Boolean)))
    (define/private ((x-ticks-near? y) t1 t2)
      (vnear? (vector (pre-tick-value t1) y z-min)
              (vector (pre-tick-value t2) y z-min)))
    
    (: y-ticks-near? (-> Real (-> pre-tick pre-tick Boolean)))
    (define/private ((y-ticks-near? x) t1 t2)
      (vnear? (vector x (pre-tick-value t1) z-min)
              (vector x (pre-tick-value t2) z-min)))
    
    (: z-ticks-near? (-> Real Real (-> pre-tick pre-tick Boolean)))
    (define/private ((z-ticks-near? x y) t1 t2)
      (vnear? (vector x y (pre-tick-value t1))
              (vector x y (pre-tick-value t2))))
    
    (: x-ticks (Listof tick))
    (: y-ticks (Listof tick))
    (: z-ticks (Listof tick))
    (define x-ticks
      (collapse-ticks (filter (λ ([t : tick]) (<= x-min (pre-tick-value t) x-max))
                              (map tick-inexact->exact rx-ticks))
                      (x-ticks-near? x-axis-y)))
    (define y-ticks
      (collapse-ticks (filter (λ ([t : tick]) (<= y-min (pre-tick-value t) y-max))
                              (map tick-inexact->exact ry-ticks))
                      (y-ticks-near? y-axis-x)))
    (define z-ticks
      (collapse-ticks (filter (λ ([t : tick]) (<= z-min (pre-tick-value t) z-max))
                              (map tick-inexact->exact rz-ticks))
                      (z-ticks-near? z-axis-x z-axis-y)))
    
    (: x-far-ticks (Listof tick))
    (: y-far-ticks (Listof tick))
    (: z-far-ticks (Listof tick))
    (define x-far-ticks
      (collapse-ticks (filter (λ ([t : tick]) (<= x-min (pre-tick-value t) x-max))
                              (map tick-inexact->exact rx-far-ticks))
                      (x-ticks-near? x-far-axis-y)))
    (define y-far-ticks
      (collapse-ticks (filter (λ ([t : tick]) (<= y-min (pre-tick-value t) y-max))
                              (map tick-inexact->exact ry-far-ticks))
                      (y-ticks-near? y-far-axis-x)))
    (define z-far-ticks
      (collapse-ticks (filter (λ ([t : tick]) (<= z-min (pre-tick-value t) z-max))
                              (map tick-inexact->exact rz-far-ticks))
                      (z-ticks-near? z-far-axis-x z-far-axis-y)))
    
    ;; ===============================================================================================
    ;; Tick and label parameters, and fixpoint margin computation
    
    ;; From here through "All parameters" are functions that compute *just the parameters* of ticks
    ;; and labels that will be drawn on the plot. We have to separate computing parameters from
    ;; actually drawing the ticks and labels so we can solve for the plot margins using a fixpoint
    ;; computation. See ../common/draw.rkt for more explanation. (Search for 'margin-fixpoint'.)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Tick parameters
    
    (: x-tick-value->view (-> Real FlVector))
    (: y-tick-value->view (-> Real FlVector))
    (: x-far-tick-value->view (-> Real FlVector))
    (: y-far-tick-value->view (-> Real FlVector))
    (define/private (x-tick-value->view x) (plot->view (vector x x-axis-y z-min)))
    (define/private (y-tick-value->view y) (plot->view (vector y-axis-x y z-min)))
    (define/private (x-far-tick-value->view x) (plot->view (vector x x-far-axis-y z-min)))
    (define/private (y-far-tick-value->view y) (plot->view (vector y-far-axis-x y z-min)))
    
    (: x-tick-value->dc (-> Real (Vectorof Real)))
    (: y-tick-value->dc (-> Real (Vectorof Real)))
    (: z-tick-value->dc (-> Real (Vectorof Real)))
    (: x-far-tick-value->dc (-> Real (Vectorof Real)))
    (: y-far-tick-value->dc (-> Real (Vectorof Real)))
    (: z-far-tick-value->dc (-> Real (Vectorof Real)))
    (define/private (x-tick-value->dc x) (view->dc (x-tick-value->view x)))
    (define/private (y-tick-value->dc y) (view->dc (y-tick-value->view y)))
    (define/private (z-tick-value->dc z) (plot->dc (vector z-axis-x z-axis-y z)))
    (define/private (x-far-tick-value->dc x) (view->dc (x-far-tick-value->view x)))
    (define/private (y-far-tick-value->dc y) (view->dc (y-far-tick-value->view y)))
    (define/private (z-far-tick-value->dc z) (plot->dc (vector z-far-axis-x z-far-axis-y z)))
    
    (: get-tick-params (-> (Listof tick) (-> Real (Vectorof Real)) Real (Listof Tick-Params)))
    (define/private (get-tick-params ts tick-value->dc angle)
      (for/list : (Listof Tick-Params) ([t  (in-list ts)])
        (match-define (tick p major? _) t)
        (list major? (tick-value->dc p) (if major? tick-radius half-tick-radius) angle)))
    
    (: get-x-tick-params (-> (Listof Tick-Params)))
    (define (get-x-tick-params)
      (if (plot-x-axis?)
          (get-tick-params x-ticks (λ ([x : Real]) (x-tick-value->dc x)) (y-axis-angle))
          empty))
    
    (: get-y-tick-params (-> (Listof Tick-Params)))
    (define (get-y-tick-params)
      (if (plot-y-axis?)
          (get-tick-params y-ticks (λ ([y : Real]) (y-tick-value->dc y)) (x-axis-angle))
          empty))
    
    (: get-z-tick-params (-> (Listof Tick-Params)))
    (define (get-z-tick-params)
      (if (plot-z-axis?)
          (get-tick-params z-ticks (λ ([z : Real]) (z-tick-value->dc z)) 0)
          empty))
    
    (: get-x-far-tick-params (-> (Listof Tick-Params)))
    (define (get-x-far-tick-params)
      (if (plot-x-far-axis?)
          (get-tick-params x-far-ticks (λ ([x : Real]) (x-far-tick-value->dc x)) (y-axis-angle))
          empty))
    
    (: get-y-far-tick-params (-> (Listof Tick-Params)))
    (define (get-y-far-tick-params)
      (if (plot-y-far-axis?)
          (get-tick-params y-far-ticks (λ ([y : Real]) (y-far-tick-value->dc y)) (x-axis-angle))
          empty))
    
    (: get-z-far-tick-params (-> (Listof Tick-Params)))
    (define (get-z-far-tick-params)
      (if (plot-z-far-axis?)
          (get-tick-params z-far-ticks (λ ([z : Real]) (z-far-tick-value->dc z)) 0)
          empty))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Tick label parameters
    
    (: draw-x-far-tick-labels? Boolean)
    (: draw-y-far-tick-labels? Boolean)
    (: draw-z-far-tick-labels? Boolean)
    (define draw-x-far-tick-labels? (not (and (plot-x-axis?) (equal? x-ticks x-far-ticks))))
    (define draw-y-far-tick-labels? (not (and (plot-y-axis?) (equal? y-ticks y-far-ticks))))
    (define draw-z-far-tick-labels? (not (and (plot-z-axis?) (equal? z-ticks z-far-ticks))))
    
    (: sort-ticks (-> (Listof tick) (-> Real FlVector) (Listof tick)))
    (define/private (sort-ticks ts tick-value->view)
      ((inst sort tick Flonum)
       ts fl> #:key (λ ([t : tick]) (flvector-ref (tick-value->view (pre-tick-value t)) 2))
       #:cache-keys? #t))
    
    (: x-tick-label-anchor Anchor)
    (define x-tick-label-anchor
      (let ([s  (sin theta)])
        (cond [(s . < . (sin (degrees->radians -67.5)))  (if x-axis-y-min? 'top-right 'top-left)]
              [(s . < . (sin (degrees->radians -22.5)))  (if x-axis-y-min? 'top-right 'top-left)]
              [(s . < . (sin (degrees->radians 22.5)))   'top]
              [(s . < . (sin (degrees->radians 67.5)))   (if x-axis-y-min? 'top-left 'top-right)]
              [else                                      (if x-axis-y-min? 'top-left 'top-right)])))
    
    (: y-tick-label-anchor Anchor)
    (define y-tick-label-anchor
      (let ([c  (cos theta)])
        (cond [(c . > . (cos (degrees->radians 22.5)))   (if y-axis-x-min? 'top-right 'top-left)]
              [(c . > . (cos (degrees->radians 67.5)))   (if y-axis-x-min? 'top-right 'top-left)]
              [(c . > . (cos (degrees->radians 112.5)))  'top]
              [(c . > . (cos (degrees->radians 157.5)))  (if y-axis-x-min? 'top-left 'top-right)]
              [else                                      (if y-axis-x-min? 'top-left 'top-right)])))
    
    (: z-tick-label-anchor Anchor)
    (define z-tick-label-anchor 'right)
    
    (: x-far-tick-label-anchor Anchor)
    (: y-far-tick-label-anchor Anchor)
    (: z-far-tick-label-anchor Anchor)
    (define x-far-tick-label-anchor (opposite-anchor x-tick-label-anchor))
    (define y-far-tick-label-anchor (opposite-anchor y-tick-label-anchor))
    (define z-far-tick-label-anchor 'left)
    
    (: get-tick-label-params (-> (Listof tick) (-> Real (Vectorof Real)) (Vectorof Real) Anchor
                                 (Listof Label-Params)))
    (define/private (get-tick-label-params ts tick-value->dc offset-dir anchor)
      (define dist (+ (pen-gap) tick-radius))
      (for/list : (Listof Label-Params) ([t  (in-list ts)] #:when (pre-tick-major? t))
        (match-define (tick x _ label) t)
        (list label (v+ (tick-value->dc x) (v* offset-dir dist)) anchor 0)))
    
    (: get-x-tick-label-params (-> (Listof Label-Params)))
    (define (get-x-tick-label-params)
      (if (plot-x-axis?)
          (let ([offset  (if x-axis-y-min? (vneg (y-axis-dir)) (y-axis-dir))])
            (get-tick-label-params (sort-ticks x-ticks (λ ([x : Real]) (x-tick-value->view x)))
                                   (λ ([x : Real]) (x-tick-value->dc x))
                                   offset
                                   x-tick-label-anchor))
          empty))
    
    (: get-y-tick-label-params (-> (Listof Label-Params)))
    (define (get-y-tick-label-params)
      (if (plot-y-axis?)
          (let ([offset  (if y-axis-x-min? (vneg (x-axis-dir)) (x-axis-dir))])
            (get-tick-label-params (sort-ticks y-ticks (λ ([y : Real]) (y-tick-value->view y)))
                                   (λ ([y : Real]) (y-tick-value->dc y))
                                   offset
                                   y-tick-label-anchor))
          empty))
    
    (: get-z-tick-label-params (-> (Listof Label-Params)))
    (define (get-z-tick-label-params)
      (if (plot-z-axis?)
          (get-tick-label-params z-ticks
                                 (λ ([z : Real]) (z-tick-value->dc z))
                                 #(-1 0)
                                 z-tick-label-anchor)
          empty))
    
    (: get-x-far-tick-label-params (-> (Listof Label-Params)))
    (define (get-x-far-tick-label-params)
      (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
          (let ([offset  (if x-axis-y-min? (y-axis-dir) (vneg (y-axis-dir)))])
            (get-tick-label-params (sort-ticks x-far-ticks (λ ([x : Real])
                                                             (x-far-tick-value->view x)))
                                   (λ ([x : Real]) (x-far-tick-value->dc x))
                                   offset
                                   x-far-tick-label-anchor))
          empty))
    
    (: get-y-far-tick-label-params (-> (Listof Label-Params)))
    (define (get-y-far-tick-label-params)
      (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
          (let ([offset  (if y-axis-x-min? (x-axis-dir) (vneg (x-axis-dir)))])
            (get-tick-label-params (sort-ticks y-far-ticks (λ ([y : Real])
                                                             (y-far-tick-value->view y)))
                                   (λ ([y : Real]) (y-far-tick-value->dc y))
                                   offset
                                   y-far-tick-label-anchor))
          empty))
    
    (: get-z-far-tick-label-params (-> (Listof Label-Params)))
    (define (get-z-far-tick-label-params)
      (if (and (plot-z-far-axis?) draw-z-far-tick-labels?)
          (get-tick-label-params z-far-ticks
                                 (λ ([z : Real]) (z-far-tick-value->dc z))
                                 #(1 0)
                                 z-far-tick-label-anchor)
          empty))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Axis label parameters
    
    (: max-tick-offset (-> (Listof tick) Real))
    (define/private (max-tick-offset ts)
      (cond [(empty? ts)  0]
            [(ormap pre-tick-major? ts)  (+ (pen-gap) tick-radius)]
            [else  (+ (pen-gap) (* 1/4 (plot-tick-size)))]))
    
    (: max-x-tick-offset Real)
    (: max-y-tick-offset Real)
    (: max-x-far-tick-offset Real)
    (: max-y-far-tick-offset Real)
    (define max-x-tick-offset (if (plot-x-axis?) (max-tick-offset x-ticks) 0))
    (define max-y-tick-offset (if (plot-y-axis?) (max-tick-offset y-ticks) 0))
    (define max-x-far-tick-offset (if (plot-x-far-axis?) (max-tick-offset x-far-ticks) 0))
    (define max-y-far-tick-offset (if (plot-y-far-axis?) (max-tick-offset y-far-ticks) 0))
    
    (: max-tick-label-height (-> (Listof tick) Real))
    (define/private (max-tick-label-height ts)
      (if (ormap pre-tick-major? ts) char-height 0))
    
    (: max-tick-label-width (-> (Listof tick) Real))
    (define/private (max-tick-label-width ts)
      (apply max 0 (for/list : (Listof Real) ([t  (in-list ts)] #:when (pre-tick-major? t))
                     (send pd get-text-width (tick-label t)))))
    
    (: max-x-tick-label-width Real)
    (: max-y-tick-label-width Real)
    (: max-z-tick-label-width Real)
    (: max-x-tick-label-height Real)
    (: max-y-tick-label-height Real)
    (: max-z-tick-label-height Real)
    (define max-x-tick-label-width (max-tick-label-width x-ticks))
    (define max-y-tick-label-width (max-tick-label-width y-ticks))
    (define max-z-tick-label-width (max-tick-label-width z-ticks))
    (define max-x-tick-label-height (max-tick-label-height x-ticks))
    (define max-y-tick-label-height (max-tick-label-height y-ticks))
    (define max-z-tick-label-height (max-tick-label-height z-ticks))
    
    (: max-x-far-tick-label-width Real)
    (: max-y-far-tick-label-width Real)
    (: max-z-far-tick-label-width Real)
    (: max-x-far-tick-label-height Real)
    (: max-y-far-tick-label-height Real)
    (: max-z-far-tick-label-height Real)
    (define max-x-far-tick-label-width (max-tick-label-width x-far-ticks))
    (define max-y-far-tick-label-width (max-tick-label-width y-far-ticks))
    (define max-z-far-tick-label-width (max-tick-label-width z-far-ticks))
    (define max-x-far-tick-label-height (max-tick-label-height x-far-ticks))
    (define max-y-far-tick-label-height (max-tick-label-height y-far-ticks))
    (define max-z-far-tick-label-height (max-tick-label-height z-far-ticks))
    
    (: max-tick-label-diag (-> (Vectorof Real) Real Real Real))
    (define/private (max-tick-label-diag axis-dc-dir max-tick-label-width max-tick-label-height)
      (match-define (vector dx dy) axis-dc-dir)
      (+ (* (abs dx) max-tick-label-width) (* (abs dy) max-tick-label-height)))
    
    (: max-x-tick-label-diag (-> Real))
    (define (max-x-tick-label-diag)
      (if (plot-x-axis?)
          (max-tick-label-diag (y-axis-dir) max-x-tick-label-width max-x-tick-label-height)
          0))
    
    (: max-y-tick-label-diag (-> Real))
    (define (max-y-tick-label-diag)
      (if (plot-y-axis?)
          (max-tick-label-diag (x-axis-dir) max-y-tick-label-width max-y-tick-label-height)
          0))
    
    (: max-x-far-tick-label-diag (-> Real))
    (define (max-x-far-tick-label-diag)
      (if (and (plot-x-far-axis?) draw-x-far-tick-labels?)
          (max-tick-label-diag (y-axis-dir) max-x-far-tick-label-width max-x-far-tick-label-height)
          0))
    
    (: max-y-far-tick-label-diag (-> Real))
    (define (max-y-far-tick-label-diag)
      (if (and (plot-y-far-axis?) draw-y-far-tick-labels?)
          (max-tick-label-diag (x-axis-dir) max-y-far-tick-label-width max-y-far-tick-label-height)
          0))
    
    (: get-x-label-params (-> Label-Params))
    (define (get-x-label-params)
      (define v0 (norm->dc (flvector 0.0 x-axis-norm-y -0.5)))
      (define dist (+ max-x-tick-offset (max-x-tick-label-diag) half-char-height))
      (list (plot-x-label) (v+ v0 (v* (y-axis-dir) (if x-axis-y-min? (- dist) dist)))
            'top (- (if x-axis-y-min? 0 pi) (x-axis-angle))))
    
    (: get-y-label-params (-> Label-Params))
    (define (get-y-label-params)
      (define v0 (norm->dc (flvector y-axis-norm-x 0.0 -0.5)))
      (define dist (+ max-y-tick-offset (max-y-tick-label-diag) half-char-height))
      (list (plot-y-label) (v+ v0 (v* (x-axis-dir) (if y-axis-x-min? (- dist) dist)))
            'top (- (if y-axis-x-min? pi 0) (y-axis-angle))))
    
    (: get-z-label-params (-> Label-Params))
    (define (get-z-label-params)
      (list (plot-z-label) (v+ (plot->dc (vector z-axis-x z-axis-y z-max))
                               (vector 0 (- half-char-height)))
            'bottom-left 0))
    
    (: get-x-far-label-params (-> Label-Params))
    (define (get-x-far-label-params)
      (define v0 (norm->dc (flvector 0.0 x-far-axis-norm-y -0.5)))
      (define dist (+ max-x-far-tick-offset (max-x-far-tick-label-diag) half-char-height))
      (list (plot-x-far-label) (v+ v0 (v* (y-axis-dir) (if x-axis-y-min? dist (- dist))))
            'bottom (- (if x-axis-y-min? 0 pi) (x-axis-angle))))
    
    (: get-y-far-label-params (-> Label-Params))
    (define (get-y-far-label-params)
      (define v0 (norm->dc (flvector y-far-axis-norm-x 0.0 -0.5)))
      (define dist (+ max-y-far-tick-offset (max-y-far-tick-label-diag) half-char-height))
      (list (plot-y-far-label) (v+ v0 (v* (x-axis-dir) (if y-axis-x-min? dist (- dist))))
            'bottom (- (if y-axis-x-min? pi 0) (y-axis-angle))))
    
    (: get-z-far-label-params (-> Label-Params))
    (define (get-z-far-label-params)
      (list (plot-z-far-label) (v+ (plot->dc (vector z-far-axis-x z-far-axis-y z-max))
                                   (vector 0 (- half-char-height)))
            'bottom-right 0))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; All parameters
    
    ;; Within each get-back-* or get-front-*, the parameters are ordered (roughly) back-to-front
    
    (: get-back-label-params (-> (Listof Label-Params)))
    (define (get-back-label-params)
      (if (plot-decorations?)
          (append (if (plot-x-far-label) (list (get-x-far-label-params)) empty)
                  (if (plot-y-far-label) (list (get-y-far-label-params)) empty)
                  (get-x-far-tick-label-params)
                  (get-y-far-tick-label-params))
          empty))
    
    (: get-front-label-params (-> (Listof Label-Params)))
    (define (get-front-label-params)
      (if (plot-decorations?)
          (append (get-z-tick-label-params)
                  (get-z-far-tick-label-params)
                  (get-x-tick-label-params)
                  (get-y-tick-label-params)
                  (if (plot-x-label) (list (get-x-label-params)) empty)
                  (if (plot-y-label) (list (get-y-label-params)) empty)
                  (if (plot-z-label) (list (get-z-label-params)) empty)
                  (if (plot-z-far-label) (list (get-z-far-label-params)) empty))
          empty))
    
    (: get-all-label-params (-> (Listof Label-Params)))
    (define (get-all-label-params)
      (append (get-back-label-params) (get-front-label-params)))
    
    (: get-back-tick-params (-> (Listof Tick-Params)))
    (define (get-back-tick-params)
      (if (plot-decorations?)
          (append (if (plot-x-far-axis?) (get-x-far-tick-params) empty)
                  (if (plot-y-far-axis?) (get-y-far-tick-params) empty)
                  (if (plot-x-axis?) (get-x-tick-params) empty)
                  (if (plot-y-axis?) (get-y-tick-params) empty))
          empty))
    
    (: get-front-tick-params (-> (Listof Tick-Params)))
    (define (get-front-tick-params)
      (if (plot-decorations?)
          (append (if (plot-z-axis?) (get-z-tick-params) empty)
                  (if (plot-z-far-axis?) (get-z-far-tick-params) empty))
          empty))
    
    (: get-all-tick-params (-> (Listof Tick-Params)))
    (define (get-all-tick-params)
      (append (get-back-tick-params) (get-front-tick-params)))
    
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Fixpoint margin computation
    
    (: get-param-vs/set-view->dc! (-> Real Real Real Real (Listof (Vectorof Real))))
    (define/private (get-param-vs/set-view->dc! left right top bottom)
      ;(printf "margins: ~v ~v ~v ~v~n" left right top bottom)
      ;(printf "label params = ~v~n" (get-all-label-params))
      ;(printf "tick params = ~v~n" (get-all-tick-params))
      (set! view->dc (make-view->dc left right top bottom))
      ;(printf "~v~n" (get-all-tick-params))
      (append (append* (map (λ ([p : Label-Params])
                              (match-define (list label v anchor angle) p)
                              (cond [label  (send pd get-text-corners label v anchor angle)]
                                    [else  empty]))
                            (get-all-label-params)))
              (append* (map (λ ([p : Tick-Params])
                              (match-define (list _ v radius angle) p)
                              (send pd get-tick-endpoints v radius angle))
                            (get-all-tick-params)))))
    
    (define: left : Real  0)
    (define: right : Real  0)
    (define: top : Real  0)
    (define: bottom : Real  0)
    (let-values ([(left-val right-val top-val bottom-val)
                  (margin-fixpoint 0 dc-x-size 0 dc-y-size 0 0 init-top-margin 0
                                   (λ ([left : Real] [right : Real] [top : Real] [bottom : Real])
                                     (get-param-vs/set-view->dc! left right top bottom)))])
      (set! left left-val)
      (set! right right-val)
      (set! top top-val)
      (set! bottom bottom-val))
    
    (: area-x-min Real)
    (: area-x-max Real)
    (: area-y-min Real)
    (: area-y-max Real)
    (define area-x-min left)
    (define area-x-max (- dc-x-size right))
    (define area-y-min top)
    (define area-y-max (- dc-y-size bottom))
    
    ;; ===============================================================================================
    ;; Plot decoration
    
    (: draw-title (-> Void))
    (define/private (draw-title)
      (define title (plot-title))
      (when (and (plot-decorations?) title)
        (send pd draw-text title (vector (* 1/2 dc-x-size) (ann 0 Real)) 'top)))
    
    (: draw-back-axes (-> Void))
    (define/private (draw-back-axes)
      (when (plot-decorations?)
        (send pd set-minor-pen)
        (when (plot-x-axis?)
          (send pd draw-line
                (norm->dc (flvector -0.5 x-axis-norm-y -0.5))
                (norm->dc (flvector 0.5 x-axis-norm-y -0.5))))
        (when (plot-x-far-axis?)
          (send pd draw-line
                (norm->dc (flvector -0.5 x-far-axis-norm-y -0.5))
                (norm->dc (flvector 0.5 x-far-axis-norm-y -0.5))))
        (when (plot-y-axis?)
          (send pd draw-line
                (norm->dc (flvector y-axis-norm-x -0.5 -0.5))
                (norm->dc (flvector y-axis-norm-x 0.5 -0.5))))
        (when (plot-y-far-axis?)
          (send pd draw-line
                (norm->dc (flvector y-far-axis-norm-x -0.5 -0.5))
                (norm->dc (flvector y-far-axis-norm-x 0.5 -0.5))))))
    
    (: draw-front-axes (-> Void))
    (define/private (draw-front-axes)
      (when (plot-decorations?)
        (send pd set-minor-pen)
        (when (plot-z-axis?)
          (send pd draw-line
                (norm->dc (flvector z-axis-norm-x z-axis-norm-y -0.5))
                (norm->dc (flvector z-axis-norm-x z-axis-norm-y 0.5))))
        (when (plot-z-far-axis?)
          (send pd draw-line
                (norm->dc (flvector z-far-axis-norm-x z-far-axis-norm-y -0.5))
                (norm->dc (flvector z-far-axis-norm-x z-far-axis-norm-y 0.5))))))
    
    (: draw-ticks (-> (Listof Tick-Params) Void))
    (define/private (draw-ticks ps)
      (for ([p  (in-list ps)])
        (match-define (list major? v r angle) p)
        (if major? (send pd set-major-pen) (send pd set-minor-pen))
        (send pd draw-tick v r angle)))
    
    (: draw-labels (-> (Listof Label-Params) Void))
    (define/private (draw-labels ps)
      (for ([p  (in-list ps)])
        (match-define (list label v anchor angle) p)
        (when label
          (send pd draw-text label v anchor angle 0 #t))))
    
    ;; ===============================================================================================
    ;; Render list and its BSP representation
    
    (: structural-shapes (HashTable Integer (Listof BSP-Shape)))
    ;; View-independent shapes, used to built initial BSP trees
    (define structural-shapes ((inst make-immutable-hash Integer (Listof BSP-Shape))))
    
    (: detail-shapes (HashTable Integer (Listof BSP-Shape)))
    ;; View-dependent shapes, inserted into BSP trees before each refresh
    (define detail-shapes ((inst make-immutable-hash Integer (Listof BSP-Shape))))
    
    (: bsp-trees (U #f (HashTable Integer BSP-Tree)))
    ;; Structural shapes partitioned in BSP trees, indexed by drawing layer
    ;; #f means not in sync with structural-shapes
    (define bsp-trees #f)
    
    (: add-shape! (-> Integer BSP-Shape Void))
    (define/private (add-shape! layer s)
      (cond [(structural-shape? s)
             (define ss structural-shapes)
             (set! structural-shapes (hash-set ss layer (cons s (hash-ref ss layer (λ () empty)))))
             (set! bsp-trees #f)]
            [else
             (define ss detail-shapes)
             (set! detail-shapes (hash-set ss layer (cons s (hash-ref ss layer (λ () empty)))))]))
    
    (: add-shapes! (-> Integer (Listof BSP-Shape) Void))
    (define/private (add-shapes! layer ss)
      (for ([s  (in-list ss)])
        (add-shape! layer s)))
    
    (: clear-shapes! (-> Void))
    (define/private (clear-shapes!)
      (set! structural-shapes ((inst make-immutable-hash Integer (Listof BSP-Shape))))
      (set! detail-shapes ((inst make-immutable-hash Integer (Listof BSP-Shape))))
      (set! bsp-trees #f))
    
    (define/public (get-render-tasks)
      (define bsp-trees (sync-bsp-trees))
      (render-tasks structural-shapes detail-shapes bsp-trees))
    
    (define/public (set-render-tasks tasks)
      (match-define (render-tasks sts dts bsps) tasks)
      (set! structural-shapes sts)
      (set! detail-shapes dts)
      (set! bsp-trees bsps))
    
    (: sync-bsp-trees (-> (HashTable Integer BSP-Tree)))
    (define/private (sync-bsp-trees)
      (define bsp-trees-val bsp-trees)
      (cond
        [bsp-trees-val  bsp-trees-val]
        [else
         (define bsp-trees-val (build-bsp-trees structural-shapes))
         (set! bsp-trees bsp-trees-val)
         bsp-trees-val]))
    
    (: adjust-detail-shapes (-> (Listof BSP-Shape) (Listof BSP-Shape)))
    (define/private (adjust-detail-shapes ss)
      (define d (view->norm view-dir))
      (define dx (flvector-ref d 0))
      (define dy (flvector-ref d 1))
      (define dz (flvector-ref d 2))
      (define area-size (fl (min (- area-x-max area-x-min)
                                 (- area-y-max area-y-min))))
      
      (for/list : (Listof BSP-Shape) ([s  (in-list ss)])
        (match s
          [(points data vs)
           ;; Bring points forward a smidge so any *on* a polygon will draw on either side
           (define frac #i1/10000)
           (points data (for/list ([v  (in-list vs)])
                          (flvector (+ (flvector-ref v 0) (* dx frac))
                                    (+ (flvector-ref v 1) (* dy frac))
                                    (+ (flvector-ref v 2) (* dz frac)))))]
          [(line data v1 v2)
           ;; Bring line forward by about half its apparent thickness
           (define frac (* 0.5 (/ pen-width area-size)))
           (line data 
                 (flvector (+ (flvector-ref v1 0) (* dx frac))
                           (+ (flvector-ref v1 1) (* dy frac))
                           (+ (flvector-ref v1 2) (* dz frac)))
                 (flvector (+ (flvector-ref v2 0) (* dx frac))
                           (+ (flvector-ref v2 1) (* dy frac))
                           (+ (flvector-ref v2 2) (* dz frac))))]
          [(lines data vs)
           ;; Bring lines forward by about half its apparent thickness
           (define frac (* 0.5 (/ pen-width area-size)))
           (lines data (for/list ([v  (in-list vs)])
                         (flvector (+ (flvector-ref v 0) (* dx frac))
                                   (+ (flvector-ref v 1) (* dy frac))
                                   (+ (flvector-ref v 2) (* dz frac)))))]
          [_  s])))
    
    (: draw-all-shapes (-> Void))
    (define/private (draw-all-shapes)
      (define bsp-trees (sync-bsp-trees))
      
      (define adj-detail-shapes
        (for/hasheq : (HashTable Integer (Listof BSP-Shape)) ([(layer ss)  (in-hash detail-shapes)])
          (values layer (adjust-detail-shapes ss))))
      
      (define all-shapes (walk-bsp-trees bsp-trees (view->norm view-dir) adj-detail-shapes))
      
      (for* ([layer  (in-list (sort (hash-keys all-shapes) >))]
             [s      (in-list (hash-ref all-shapes layer))])
        (draw-shape s)))
    
    ;; ===============================================================================================
    ;; Lighting
    
    (: light FlVector)
    ;; Light position, in normalized view coordinates: 5 units up, ~3 units back and to the left
    ;; (simulates non-noon daylight conditions)
    (define light (m3-apply rotate-rho-matrix (flvector (- -0.5 2.0)
                                                        (- -0.5 2.0)
                                                        (+ 0.5 5.0))))
    
    ;; Do lighting only by direction so we can precalculate light-dir and half-dir
    ;; Conceptually, the viewer and light are at infinity
    
    (: light-dir FlVector)
    ;; Light direction
    (define light-dir (vector->flvector (vnormalize (flv3->v light))))
    
    (: view-dir FlVector)
    ;; View direction, in normalized view coordinates
    (define view-dir (flvector 0.0 -1.0 0.0))
    
    (: half-dir FlVector)
    ;; Blinn-Phong "half angle" direction
    (define half-dir
      (vector->flvector (vnormalize (v* (v+ (flv3->v light-dir) (flv3->v view-dir)) 0.5))))
    
    (: diffuse-light? Boolean)
    (: specular-light? Boolean)
    (: ambient-light Flonum)
    (define diffuse-light? (plot3d-diffuse-light?))
    (define specular-light? (plot3d-specular-light?))
    (define ambient-light (fl (plot3d-ambient-light)))
    
    (: get-light-values (-> FlVector (Values Flonum Flonum)))
    (define/private (get-light-values normal)
      (cond
        [(not (or diffuse-light? specular-light?))  (values 1.0 0.0)]
        [else
         ;; Diffuse lighting: typical Lambertian surface model (using absolute value because we
         ;; can't expect surface normals to point the right direction)
         (define diff
           (cond [diffuse-light?  (flabs (flv3-dot normal light-dir))]
                 [else  1.0]))
         ;; Specular highlighting: Blinn-Phong model
         (define spec
           (cond [specular-light?  (fl* 32.0 (expt (flabs (flv3-dot normal half-dir)) 20.0))]
                 [else  0.0]))
         ;; Blend ambient light with diffuse light, return specular as it is
         ;; As ambient-light -> 1.0, contribution of diffuse -> 0.0
         (values (fl+ ambient-light (fl* (fl- 1.0 ambient-light) diff)) spec)]))
    
    (: illuminate (-> (List Real Real Real) Real Real (List Real Real Real)))
    (define/private (illuminate c diff spec)
      (match-define (list r g b) c)
      (list (+ (* r diff) spec)
            (+ (* g diff) spec)
            (+ (* b diff) spec)))
    
    ;; ===============================================================================================
    ;; Drawing
    
    (: draw-polygon (-> poly Void))
    (define/private (draw-polygon s)
      (match-define (poly (poly-data alpha center
                                     pen-color pen-width pen-style
                                     brush-color brush-style face)
                          vs ls normal)
        s)
      (define view-normal (norm->view normal))
      (define cos-view (flv3-dot view-dir view-normal))
      (cond
        [(and (cos-view . < . 0.0) (eq? face 'front))  (void)]
        [(and (cos-view . > . 0.0) (eq? face 'back))   (void)]
        [else
         (send pd set-alpha alpha)
         (define-values (diff spec) (get-light-values view-normal))
         (let ([pen-color    (illuminate pen-color diff spec)]
               [brush-color  (illuminate brush-color diff spec)]
               [vs  (map (λ ([v : FlVector]) (norm->dc v)) vs)])
           ;(send pd set-pen "black" 0.5 'solid)  ; for BSP debugging
           (send pd set-pen "black" 0 'transparent)
           (send pd set-brush brush-color brush-style)
           (send pd draw-polygon vs)
           ;; Draw lines around polygon
           (send pd set-pen pen-color pen-width pen-style)
           (cond [(andmap (λ ([l : Boolean]) l) ls)
                  ;; Fast path: all lines drawn
                  (send pd draw-lines (cons (last vs) vs))]
                 [else
                  ;; Slow path: draw each as indicated by ls
                  ;; TODO: draw contiguous lines using draw-lines
                  (for ([v1  (in-list (cons (last vs) vs))]
                        [v2  (in-list vs)]
                        [l   (in-list ls)])
                    (when l (send pd draw-line v1 v2)))]))]))
    
    (: draw-line (-> line Void))
    (define/private (draw-line s)
      (match-define (line (line-data alpha pen-color pen-width pen-style) v1 v2) s)
      (send pd set-alpha alpha)
      (send pd set-pen pen-color pen-width pen-style)
      (send pd draw-line (norm->dc v1) (norm->dc v2)))
    
    (: draw-lines (-> lines Void))
    (define/private (draw-lines s)
      (match-define (lines (line-data alpha pen-color pen-width pen-style) vs) s)
      (send pd set-alpha alpha)
      (send pd set-pen pen-color pen-width pen-style)
      (send pd draw-lines (map (λ ([v : FlVector]) (norm->dc v)) vs)))
    
    (: draw-glyph (-> glyph-data (Listof FlVector) Void))
    (define/private (draw-glyph data vs)
      (match-define (glyph-data alpha symbol size
                                pen-color pen-width pen-style
                                brush-color brush-style)
        data)
      (send pd set-alpha alpha)
      (send pd set-pen pen-color pen-width pen-style)
      (send pd set-brush brush-color brush-style)
      (send pd draw-glyphs (map (λ ([v : FlVector]) (norm->dc v)) vs) symbol size))
    
    (: draw-text (-> text-data (Listof FlVector) Void))
    (define/private (draw-text data vs)
      (match-define (text-data alpha anchor angle dist str font-size font-family color outline?) data)
      (send pd set-alpha alpha)
      (send pd set-font-attribs font-size #f font-family)
      (send pd set-text-foreground color)
      (for ([v  (in-list vs)])
        (send pd draw-text str (norm->dc v) anchor angle dist outline?)))
    
    (: draw-arrow (-> arrow-data Void))
    (define/private (draw-arrow data)
      (match-define (arrow-data alpha v1 v2 outline-color pen-color pen-width pen-style) data)
      (let ([v1  (norm->dc v1)]
            [v2  (norm->dc v2)])
        (send pd set-alpha alpha)
        (send pd set-pen outline-color (+ 2 pen-width) 'solid)
        (send pd draw-arrow v1 v2)
        (send pd set-pen pen-color pen-width pen-style)
        (send pd draw-arrow v1 v2)))
    
    (: draw-points (-> points Void))
    (define/private (draw-points s)
      (match-define (points data vs) s)
      (cond [(glyph-data? data)  (draw-glyph data vs)]
            [(text-data? data)   (draw-text data vs)]
            [(arrow-data? data)  (draw-arrow data)]))
    
    (: draw-shape (-> BSP-Shape Void))
    (define/private (draw-shape s)
      (cond [(poly? s)    (draw-polygon s)]
            [(line? s)    (draw-line s)]
            [(lines? s)   (draw-lines s)]
            [(points? s)  (draw-points s)]
            [else  (raise-argument-error 'draw-shape "known shape" s)]))
    
    ;; ===============================================================================================
    ;; Public drawing control (used by plot3d/dc)
    
    (define/public (start-plot)
      (send pd reset-drawing-params)
      (send pd clear)
      (draw-title)
      (draw-labels (get-back-label-params))
      (draw-ticks (get-back-tick-params))
      (draw-back-axes)
      (send pd set-clipping-rect
            (vector (ivl (+ 1/2 (- area-x-min (plot-line-width))) (+ area-x-max (plot-line-width)))
                    (ivl (+ 1/2 (- area-y-min (plot-line-width))) (+ area-y-max (plot-line-width)))))
      (clear-shapes!))
    
    (define/public (start-renderer rend-bounds-rect)
      (reset-drawing-params)
      (put-clip-rect rend-bounds-rect))
    
    (define/public (end-renderers)
      (clear-clip-rect)
      (draw-all-shapes)
      (send pd reset-drawing-params)
      (draw-front-axes)
      (draw-ticks (get-front-tick-params))
      (draw-labels (get-front-label-params)))
    
    (define/public (draw-legend legend-entries)
      (define gap-size (+ (pen-gap) tick-radius))
      (send pd draw-legend legend-entries
            (vector (ivl (+ area-x-min gap-size) (- area-x-max gap-size))
                    (ivl (+ area-y-min gap-size) (- area-y-max gap-size)))))
    
    (define/public (end-plot)
      (send pd restore-drawing-params))
    
    ;; ===============================================================================================
    ;; Public drawing interface (used by renderers)
    
    ;; Drawing parameters
    
    (: alpha Nonnegative-Real)
    (define alpha 1)
    
    (: pen-color (List Real Real Real))
    (: pen-width Nonnegative-Real)
    (: pen-style Plot-Pen-Style-Sym)
    (define pen-color '(0 0 0))
    (define pen-width 1)
    (define pen-style 'solid)
    
    (: brush-color (List Real Real Real))
    (: brush-style Plot-Brush-Style-Sym)
    (define brush-color '(255 255 255))
    (define brush-style 'solid)
    
    (: background-color (List Real Real Real))
    (define background-color '(255 255 255))
    
    (: font-size Nonnegative-Real)
    (: font-face (U #f String))
    (: font-family Font-Family)
    (define font-size 11)
    (define font-face #f)
    (define font-family 'roman)
    
    (: text-foreground (List Real Real Real))
    (define text-foreground '(0 0 0))
    
    ;; Drawing parameter accessors
    
    (define/public (put-alpha a) (set! alpha a))
    
    (define/public (put-pen color width style)
      (set! pen-color (->pen-color color))
      (set! pen-width width)
      (set! pen-style (->pen-style style)))
    
    (define/public (put-major-pen [style 'solid])
      (put-pen (plot-foreground) (plot-line-width) style))
    
    (define/public (put-minor-pen [style 'solid])
      (put-pen (plot-foreground) (* 1/2 (plot-line-width)) style))
    
    (define/public (put-brush color style)
      (set! brush-color (->brush-color color))
      (set! brush-style (->brush-style style)))
    
    (define/public (put-background color)
      (set! background-color (->brush-color color)))
    
    (define/public (put-font-size size) (set! font-size size))
    (define/public (put-font-face face) (set! font-face face))
    (define/public (put-font-family family) (set! font-family family))
    
    (define/public (put-font-attribs size face family)
      (put-font-size size)
      (put-font-face face)
      (put-font-family family))
    
    (define/public (put-text-foreground c)
      (set! text-foreground (->pen-color c)))
    
    (define/public (reset-drawing-params)
      (put-alpha (plot-foreground-alpha))
      (put-pen (plot-foreground) (plot-line-width) 'solid)
      (put-brush (plot-background) 'solid)
      (put-background (plot-background))
      (put-font-attribs (plot-font-size) (plot-font-face) (plot-font-family))
      (put-text-foreground (plot-foreground)))
    
    ;; Drawing shapes
    
    (define/public (put-line v1 v2)
      (let ([v1  (exact-vector3d v1)]
            [v2  (exact-vector3d v2)])
        (when (and v1 v2)
          (let-values ([(v1 v2)  (if clipping?
                                     (clip-line/bounds v1 v2
                                                       clip-x-min clip-x-max
                                                       clip-y-min clip-y-max
                                                       clip-z-min clip-z-max)
                                     (values v1 v2))])
            (when (and v1 v2)
              (cond [identity-transforms?
                     (add-shape! plot3d-area-layer
                                 (line (line-data alpha pen-color pen-width pen-style)
                                       (plot->norm v1)
                                       (plot->norm v2)))]
                    [else
                     (define vs (subdivide-line (λ ([v : (Vectorof Real)]) (plot->dc v)) v1 v2))
                     (add-shape! plot3d-area-layer
                                 (lines (line-data alpha pen-color pen-width pen-style)
                                        (map (λ ([v : (Vectorof Real)]) (plot->norm v)) vs)))]))))))
    
    (define/public (put-lines vs)
      (for ([vs  (in-list (exact-vector3d-sublists vs))])
        (let ([vss  (if clipping?
                        (clip-lines/bounds vs
                                           clip-x-min clip-x-max
                                           clip-y-min clip-y-max
                                           clip-z-min clip-z-max)
                        (list vs))])
          (cond [identity-transforms?
                 (for ([vs  (in-list vss)])
                   (add-shape! plot3d-area-layer
                               (lines (line-data alpha pen-color pen-width pen-style)
                                      (map (λ ([v : (Vectorof Real)]) (plot->norm v)) vs))))]
                [else
                 (for ([vs  (in-list vss)])
                   (let ([vs  (subdivide-lines (λ ([v : (Vectorof Real)]) (plot->dc v)) vs)])
                     (add-shape! plot3d-area-layer
                                 (lines (line-data alpha pen-color pen-width pen-style)
                                        (map (λ ([v : (Vectorof Real)]) (plot->norm v)) vs)))))]))))
    
    (define/public (put-polygon vs [face 'both] [ls (make-list (length vs) #t)])
      (let-values ([(vs ls)  (exact-polygon3d vs ls)])
        (unless (empty? vs)
          (let*-values
              ([(vs ls)  (if clipping?
                             (clip-polygon/bounds vs ls
                                                  clip-x-min clip-x-max
                                                  clip-y-min clip-y-max
                                                  clip-z-min clip-z-max)
                             (values vs ls))]
               [(vs ls)  (if identity-transforms?
                             (values vs ls)
                             (subdivide-polygon (λ ([v : (Vectorof Real)]) (plot->dc v)) vs ls))])
            (unless (empty? vs)
              (define norm-vs (map (λ ([v : (Vectorof Real)]) (plot->norm v)) vs))
              (define normal (flv3-normal norm-vs))
              (define center (flv3-center norm-vs))
              (add-shape! plot3d-area-layer
                          (poly (poly-data alpha center
                                           pen-color pen-width pen-style
                                           brush-color brush-style face)
                                norm-vs ls normal)))))))
    
    (define/public (put-rect r)
      (let ([r  (if (rect-rational? r) (rect-meet r bounds-rect) r)])
        (when (rect-rational? r)
          (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) r)
          (define v-min (plot->norm (vector (inexact->exact (assert x-min values))
                                            (inexact->exact (assert y-min values))
                                            (inexact->exact (assert z-min values)))))
          (define v-max (plot->norm (vector (inexact->exact (assert x-max values))
                                            (inexact->exact (assert y-max values))
                                            (inexact->exact (assert z-max values)))))
          (let ()
            (define x-min (flvector-ref v-min 0))
            (define y-min (flvector-ref v-min 1))
            (define z-min (flvector-ref v-min 2))
            (define x-max (flvector-ref v-max 0))
            (define y-max (flvector-ref v-max 1))
            (define z-max (flvector-ref v-max 2))
            (define x-mid (* 0.5 (+ x-max x-min)))
            (define y-mid (* 0.5 (+ y-max y-min)))
            (define z-mid (* 0.5 (+ z-max z-min)))
            ;; Faces are a list of center, normal, then vertices
            (define faces
              (list 
               ;; Bottom (z-min) face
               (list (flvector x-mid y-mid z-min) (flvector 0.0 0.0 -1.0)
                     (flvector x-min y-min z-min) (flvector x-max y-min z-min)
                     (flvector x-max y-max z-min) (flvector x-min y-max z-min))
               ;; Top (z-max) face
               (list (flvector x-mid y-mid z-max) (flvector 0.0 0.0 1.0)
                     (flvector x-min y-min z-max) (flvector x-max y-min z-max)
                     (flvector x-max y-max z-max) (flvector x-min y-max z-max))
               ;; Front (y-min) face
               (list (flvector x-mid y-min z-mid) (flvector 0.0 -1.0 0.0)
                     (flvector x-min y-min z-min) (flvector x-max y-min z-min)
                     (flvector x-max y-min z-max) (flvector x-min y-min z-max))
               ;; Back (y-max) face
               (list (flvector x-mid y-max z-mid) (flvector 0.0 1.0 0.0)
                     (flvector x-min y-max z-min) (flvector x-max y-max z-min)
                     (flvector x-max y-max z-max) (flvector x-min y-max z-max))
               ;; Left (x-min) face
               (list (flvector x-min y-mid z-mid) (flvector -1.0 0.0 0.0)
                     (flvector x-min y-min z-min) (flvector x-min y-max z-min)
                     (flvector x-min y-max z-max) (flvector x-min y-min z-max))
               ;; Right (x-max) face
               (list (flvector x-max y-mid z-mid) (flvector 1.0 0.0 0.0)
                     (flvector x-max y-min z-min) (flvector x-max y-max z-min)
                     (flvector x-max y-max z-max) (flvector x-max y-min z-max))))
            (define ls (list #t #t #t #t))
            (for ([face  (in-list faces)])
              (match-define (list center normal vs ...) face)
              (add-shape! plot3d-area-layer
                          (poly (poly-data alpha center
                                           pen-color pen-width pen-style
                                           brush-color brush-style 'front)
                                vs ls normal)))))))
    
    (define/public (put-text str v [anchor 'center] [angle 0] [dist 0] [outline? #f]
                             [layer plot3d-area-layer])
      (let ([v  (exact-vector3d v)])
        (when (and v (in-bounds? v))
          (add-shape! layer (points (text-data alpha anchor angle dist str
                                               font-size font-family text-foreground outline?)
                                    (list (plot->norm v)))))))
    
    (define/public (put-glyphs vs symbol size [layer plot3d-area-layer])
      (let ([vs  (filter (λ ([v : (U #f (Vectorof Real))]) (and v (in-bounds? v)))
                         (map exact-vector3d vs))])
        (unless (empty? vs)
          (add-shape! layer (points (glyph-data alpha symbol size
                                                pen-color pen-width pen-style
                                                brush-color brush-style)
                                    (map (λ ([v : (U #f (Vectorof Real))])
                                           (plot->norm (assert v values)))
                                         vs))))))
    
    (define/public (put-arrow v1 v2)
      (let ([v1  (exact-vector3d v1)]
            [v2  (exact-vector3d v2)])
        (when (and v1 v2 (in-bounds? v1))
          (cond [(in-bounds? v2)
                 (define c (v* (v+ v1 v2) 1/2))
                 (define outline-color (->brush-color (plot-background)))
                 (add-shape! plot3d-area-layer
                             (points (arrow-data alpha (plot->norm v1) (plot->norm v2)
                                                 outline-color pen-color pen-width pen-style)
                                     (list (plot->norm c))))]
                [else
                 (put-line v1 v2)]))))
    )) ; end class
