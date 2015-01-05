#lang typed/racket/base

(require (for-syntax racket/base)
         typed/racket/draw
         typed/racket/class
         "type-doc.rkt"
         "math.rkt")

(provide (all-defined-out))

(deftype (Treeof A) (U A (Listof (Treeof A))))

(deftype Anchor
  (U 'top-left    'top    'top-right
     'left        'center 'right
     'bottom-left 'bottom 'bottom-right))

(deftype Color
  (U (List Real Real Real)
     String
     Symbol
     (Instance Color%)))

(deftype Plot-Color
  (U Integer Color))

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
        'fullcircle7       'fullcircle8)))

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
(deftype (Labels A)            (List-Generator A (U String #f)))

(deftype Contour-Levels (U 'auto Positive-Integer (Listof Real)))

(define-type Image-File-Format
  (U 'png 'jpeg 
     'xbm 'xpm 'bmp 
     'ps 'pdf 'svg))

(deftype Legend-Draw-Proc (-> (Instance Plot-Device%) Real Real Void))

(struct legend-entry ([label : String] [draw : Legend-Draw-Proc]) #:transparent)

(define-type Plot-Device%
  (Class
   (init-field [dc (Instance DC<%>)]
               [dc-x-min Real]
               [dc-y-min Real]
               [dc-x-size Nonnegative-Real]
               [dc-y-size Nonnegative-Real])
   [restore-drawing-params (-> Void)]
   [reset-drawing-params (->* [] [Boolean] Void)]
   [set-pen (-> Plot-Color Nonnegative-Real Plot-Pen-Style Void)]
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
   [get-text-extent (-> String (Values Exact-Rational Exact-Rational Exact-Rational Exact-Rational))]
   [get-text-width (-> String Exact-Rational)]
   [set-text-foreground (-> Plot-Color Void)]
   [set-clipping-rect (-> Rect Void)]
   [clear-clipping-rect (-> Void)]
   [clear (-> Void)]
   [draw-point (-> (Vectorof Real) Void)]
   [draw-polygon (-> (Listof (Vectorof Real)) Void)]
   [draw-rect (-> Rect Void)]
   [draw-lines (-> (Listof (Vectorof Real)) Void)]
   [draw-line (-> (Vectorof Real) (Vectorof Real) Void)]
   [draw-text (->* [String (Vectorof Real)] [Anchor Real Real Boolean] Void)]
   [get-text-corners (->* [String (Vectorof Real)] [Anchor Real Real] (Listof (Vectorof Real)))]
   [draw-arrow (-> (Vectorof Real) (Vectorof Real) Void)]
   [get-tick-endpoints (-> (Vectorof Real) Real Real (List (Vectorof Real) (Vectorof Real)))]
   [draw-tick (-> (Vectorof Real) Real Real Void)]
   [draw-arrow-glyph (-> (Vectorof Real) Real Real Void)]
   [draw-glyphs (-> (Listof (Vectorof Real)) Point-Sym Nonnegative-Real Void)]
   [draw-legend (-> (Listof legend-entry) Rect Void)]))
