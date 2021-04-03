#lang typed/racket/base

(require (for-syntax racket/base)
         typed/racket/draw
         typed/racket/class
         typed/pict
         "type-doc.rkt"
         "math.rkt")


(provide (all-defined-out))

(deftype (Treeof A) (U A (Listof (Treeof A))))

(deftype Anchor
  (U 'top-left    'top    'top-right
     'left        'center 'right
     'bottom-left 'bottom 'bottom-right
     'auto))
(define-predicate anchor? Anchor)

(deftype Color
  (U (List Real Real Real)
     String
     Symbol
     (Instance Color%)))

(deftype Plot-Color
  (U Integer Color))

(deftype Color-Map (Vectorof (List Byte Byte Byte)))

(deftype Plot-Pen-Style-Sym
  (U 'transparent 'solid    'dot 'long-dash
     'short-dash  'dot-dash))

(deftype Plot-Pen-Style
  (U Integer Plot-Pen-Style-Sym))

(deftype Plot-Brush-Style-Sym
  (U 'transparent      'solid
     'bdiagonal-hatch  'fdiagonal-hatch 'crossdiag-hatch
     'horizontal-hatch 'vertical-hatch  'cross-hatch))

(deftype Plot-Brush-Style
  (U Integer Plot-Brush-Style-Sym))

(deftype Point-Sym
  (U Char String Integer
     (U 'dot               'point            'pixel
        'plus              'times            'asterisk
        '5asterisk         'odot             'oplus
        'otimes            'oasterisk        'o5asterisk
        'circle            'square           'diamond
        'triangle          'fullcircle       'fullsquare
        'fulldiamond       'fulltriangle     'triangleup
        'triangledown      'triangleleft     'triangleright
        'fulltriangleup    'fulltriangledown 'fulltriangleleft
        'fulltriangleright 'rightarrow       'leftarrow
        'uparrow           'downarrow        '4star
        '5star             '6star            '7star
        '8star             'full4star        'full5star
        'full6star         'full7star        'full8star
        'circle1           'circle2          'circle3
        'circle4           'circle5          'circle6
        'circle7           'circle8          'bullet
        'fullcircle1       'fullcircle2      'fullcircle3
        'fullcircle4       'fullcircle5      'fullcircle6
        'fullcircle7       'fullcircle8      'none)))

(deftype (List-Generator A B) (U (Listof B) (-> A (Listof B))))

(: generate-list (All (A B) (-> (List-Generator A B) A (Listof B))))
(define (generate-list f arg)
  (cond [(list? f)  f]
        [else  (f arg)]))

(deftype (Plot-Colors A)       (List-Generator A Plot-Color))
(deftype (Plot-Pen-Styles A)   (List-Generator A Plot-Pen-Style))
(deftype (Pen-Widths A)        (List-Generator A Nonnegative-Real))
(deftype (Plot-Brush-Styles A) (List-Generator A Plot-Brush-Style))
(deftype (Alphas A)            (List-Generator A Nonnegative-Real))
(deftype (Labels A)            (List-Generator A (U String pict #f)))

(deftype Contour-Levels (U 'auto Positive-Integer (Listof Real)))

(define-type Image-File-Format
  (U 'png 'jpeg
     'xbm 'xpm 'bmp
     'ps 'pdf 'svg))

(deftype Legend-Draw-Proc (-> (Instance Plot-Device%) Real Real Void))

(struct legend-entry ([label : (U String pict)] [draw : Legend-Draw-Proc]) #:transparent)

(deftype Legend-Anchor (U Anchor
                          'no-legend
                          'outside-global-top
                          'outside-top-left 'outside-top 'outside-top-right
                          'outside-left-top 'outside-left 'outside-left-bottom
                          'outside-right-top 'outside-right 'outside-right-bottom
                          'outside-bottom-left 'outside-bottom 'outside-bottom-right))
(define (inside-anchor? [a : Legend-Anchor]) (anchor? a))
(define (outside-anchor? [a : Legend-Anchor])
  (and (not (anchor? a)) (not (eq? a 'no-legend))))
(define (legend-anchor->anchor [a : Legend-Anchor]) : Anchor
  (if (anchor? a)
      a
      (case a
        [(outside-top-left outside-left-top) 'top-left]
        [(outside-top outside-global-top) 'top]
        [(outside-top-right outside-right-top) 'top-right]
        [(outside-right) 'right]
        [(outside-bottom-right outside-right-bottom) 'bottom-right]
        [(outside-bottom) 'bottom]
        [(outside-bottom-left outside-left-bottom) 'bottom-left]
        [(outside-left) 'left]
        [else 'auto])))

(deftype Legend-Layout (List (U 'rows 'columns) Positive-Integer (U 'equal-size 'compact)))

(define-type Plot-Device%
  (Class
   (init-field [dc (Instance DC<%>)]
               [dc-x-min Real]
               [dc-y-min Real]
               [dc-x-size Nonnegative-Real]
               [dc-y-size Nonnegative-Real])
   [restore-drawing-params (-> Void)]
   [reset-drawing-params (->* [] [Boolean] Void)]
   [set-pen (->* [Plot-Color Nonnegative-Real Plot-Pen-Style] [Pen-Cap-Style] Void)]
   [set-major-pen (->* [] [Plot-Pen-Style] Void)]
   [set-minor-pen (->* [] [Plot-Pen-Style] Void)]
   [set-brush (-> Plot-Color Plot-Brush-Style Void)]
   [set-alpha (-> Nonnegative-Real Void)]
   [set-background (-> Plot-Color Void)]
   [set-background-alpha (-> Nonnegative-Real Void)]
   [set-font-attribs (-> Real (U String #f) Font-Family Void)]
   [set-font (-> (Instance Font%) Void)]
   [set-font-size (-> Real Void)]
   [get-char-height (-> Exact-Rational)]
   [get-char-baseline (-> Exact-Rational)]
   [get-text-extent (-> (U String pict) (Values Exact-Rational Exact-Rational Exact-Rational Exact-Rational))]
   [get-text-width (-> (U String pict) Exact-Rational)]
   [set-text-foreground (-> Plot-Color Void)]
   [set-arrow-head (-> (U (List '= Nonnegative-Real) Nonnegative-Real) Nonnegative-Real Void)]
   [set-clipping-rect (-> Rect Void)]
   [clear-clipping-rect (-> Void)]
   [clear (-> Void)]
   [draw-point (-> (Vectorof Real) Void)]
   [draw-polygon (-> (Listof (Vectorof Real)) Void)]
   [draw-rect (-> Rect Void)]
   [draw-lines (-> (Listof (Vectorof Real)) Void)]
   [draw-line (-> (Vectorof Real) (Vectorof Real) Void)]
   [draw-text (->* [String (Vectorof Real)] [Anchor Real Real Boolean] Void)]
   [get-text-corners (->* [(U String pict) (Vectorof Real)] [Anchor Real Real] (Listof (Vectorof Real)))]
   [draw-arrow (-> (Vectorof Real) (Vectorof Real) Void)]
   [get-tick-endpoints (-> (Vectorof Real) Real Real (List (Vectorof Real) (Vectorof Real)))]
   [draw-tick (-> (Vectorof Real) Real Real Void)]
   [draw-arrow-glyph (-> (Vectorof Real) Real Real Void)]
   [draw-glyphs (-> (Listof (Vectorof Real)) Point-Sym Nonnegative-Real Void)]
   [draw-pict (->* [pict (Vectorof Real)] (Anchor Real) Void)]
   [calculate-legend-rect (-> (Listof legend-entry) Rect Anchor Rect)]
   [draw-legend (-> (Listof legend-entry) Rect Void)]))

(require "plotmetrics.rkt")
(provide Plot-Metrics<%> Plot-Pict Plot-Metrics-Functions)