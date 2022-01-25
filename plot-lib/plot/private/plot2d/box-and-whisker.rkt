#lang typed/racket/base

(require (only-in math/statistics quantile)
         racket/sequence
         racket/match
         racket/class
         (only-in typed/pict pict)
         plot/utils
         (only-in "../common/utils.rkt" make-raise-keyword-error)
         (only-in "rectangle.rkt" discrete-histogram-ticks-fun))

(provide box-and-whisker
         make-one-axis-default-ticks-fun)

;; Box and whisker renderer, https://en.wikipedia.org/wiki/Box_plot

;; See also https://en.wikipedia.org/wiki/Box_plot, and the Matplotlib
;; reference
;; https://matplotlib.org/3.2.1/api/_as_gen/matplotlib.pyplot.boxplot.html

;; Hold the quantile and other information for the box and whiskers plot: Q1
;; is the 1st quantile, 25% of the points in the data set are less than this
;; value.  MEDIAN is the median, 50% of the points in the data set are less
;; than this, while Q3 is the 3rd quantile (75%).  The LOWER-WHISKER and
;; UPPWER-WHISKER are lower and upper limits below and above which points are
;; considered outliers, see `samples->bnw-data`.  Finally, OUTLIERS represents
;; a list of outlier points.
(struct bnw-data
  ([q1 : Real]
   [median : Real]
   [q3 : Real]
   [lower-whisker : Real]
   [upper-whisker : Real]
   [outliers : (Listof Real)])
  #:transparent)

;; Construct a BNW-DATA object from the data points VS.  If WS is present, it
;; represents the weights of each data point (if missing, each point has the
;; same weight), see `quantile` from the `racket/math` package on how WS is
;; used.
;;
;; IQR-SCALE is the multiplier used to determine the lower an upper limits
;; below and above which points are considered outliers.  These limits are
;; calculated as (* IQR-SCALE (- Q3 Q1)) above and below the median, with the
;; caveat that the limits are adjusted to represent the highest (or lowest)
;; actual data point at that limit.
(: samples->bnw-data (->* ((Sequenceof Real)) ((U #f (Sequenceof Real)) #:iqr-scale Real) bnw-data))
(define (samples->bnw-data vs [ws #f] #:iqr-scale [iqr-scale 1.5])
  (let* ([q1 (quantile 0.25 < vs ws)]
         [median (quantile 0.5 < vs ws)]
         [q3 (quantile 0.75 < vs ws)]
         [iqr (- q3 q1)]
         [lower-limit (- q1 (* iqr-scale iqr))]
         [upper-limit (+ q3 (* iqr-scale iqr))]
         [lower-whisker (cast (sequence-fold
                               (lambda ([a : Real] [sample : Real])
                                 (if (>= sample lower-limit) (min a sample) a))
                               q1 vs)
                              Real)]
         [upper-whisker (cast (sequence-fold
                               (lambda ([a : Real] [sample : Real])
                                 (if (<= sample upper-limit) (max a sample) a))
                               q3 vs)
                              Real)]
         [outliers (sequence-filter
                    (lambda ([sample : Real])
                      (or (> sample upper-whisker)
                          (< sample lower-whisker))) vs)])
    (bnw-data q1 median q3 lower-whisker upper-whisker (sequence->list outliers))))

;; Wrapper around `default-ticks-fun` to return ticks for one axis only.  If
;; INVERT? is #f, the Y axis (far and near) ticks are returned, if INVERT? is
;; #t, the X axis (far and near) ticks are returned.  This can be used by
;; one-dimensional plots, such as box-and-whisker and violin to avoid placing
;; ticks on the axis that has no dimensions.
(: make-one-axis-default-ticks-fun (-> Boolean Ticks-Fun))
(define ((make-one-axis-default-ticks-fun invert?) rect)
  (match-define (list x-near x-far y-near y-far)
    (default-ticks-fun rect))
  (if invert?
      (list x-near x-far '() '())
      (list '() '() y-near y-far)))

;; Rendering procedure for the box-and-whisker plot.  This does the actual
;; rendering of the plot data, but all information is already preprocessed and
;; passed to it.  For plot snips, this function can be invoked each time the
;; snip is drawn, so the function should be reasonably fast and all things
;; that can be precomputed should be.
(: renderer-proc
   (-> Real Real Real Real Real (Listof (Vectorof Real))
       Real Nonnegative-Real Boolean
       Plot-Color Plot-Brush-Style Plot-Color Plot-Pen-Style Nonnegative-Real Nonnegative-Real
       Boolean Plot-Color Plot-Pen-Style Nonnegative-Real Nonnegative-Real
       Boolean Plot-Color Plot-Pen-Style Nonnegative-Real Nonnegative-Real
       Boolean Plot-Color Point-Sym Plot-Color Nonnegative-Real Nonnegative-Real Nonnegative-Real
       2D-Render-Proc))
(define ((renderer-proc
          q1 median q3 lower-whisker upper-whisker outliers
          x width invert?
          box-color box-style box-line-color box-line-style box-line-width box-alpha
          show-median? median-color median-style median-width median-alpha
          show-whiskers? whisker-color whisker-style whisker-width whisker-alpha
          show-outliers? outlier-color outlier-sym outlier-fill-color outlier-size outlier-line-width outlier-alpha)
         area)
  (define half-width (* 1/2 width))
  (define quater-width (* 1/4 width))
  (: maybe-invert (All (A) (-> A A (Vectorof A))))
  (define maybe-invert (if invert? (lambda (x y) (vector y x)) vector))
  (send area put-pen box-line-color box-line-width box-line-style)
  (send area put-brush box-color box-style)
  (send area put-alpha box-alpha)
  (send area put-rect
        (maybe-invert (ivl (- x half-width) (+ x half-width)) (ivl q1 q3)))
  (when show-median?
    (send area put-pen median-color median-width median-style)
    (send area put-alpha median-alpha)
    (send area put-line
          (maybe-invert (- x half-width) median)
          (maybe-invert (+ x half-width) median)))
  (when show-whiskers?
    (send area put-pen whisker-color whisker-width whisker-style)
    (send area put-alpha whisker-alpha)
    (send area put-line (maybe-invert x lower-whisker) (maybe-invert x q1))
    (send area put-line (maybe-invert x q3) (maybe-invert x upper-whisker))
    (send area put-line
          (maybe-invert (- x quater-width) lower-whisker)
          (maybe-invert (+ x quater-width) lower-whisker))
    (send area put-line
          (maybe-invert (- x quater-width) upper-whisker)
          (maybe-invert (+ x quater-width) upper-whisker)))
  (when show-outliers?
    (send area put-alpha outlier-alpha)
    (send area put-pen outlier-color outlier-line-width 'solid)
    (send area put-brush outlier-fill-color 'solid)
    (send area put-glyphs outliers outlier-sym outlier-size)))

;; Create a box-and-whisker plot from data obtained from a call to
;; `samples->bnw-data`.  X represents the x axis position where the box and
;; whiskers plot should be placed (by default the plot is drawn in a vertical
;; position).  Various X values allows multiple box plots to be drawn side by
;; side.  If INVERT? is #t, the plot is drawn horizontally, and X represents a
;; value on the Y axis.  GAP represents the "width" of the box plot (but the
;; actual plot will be narrower than GAP.  If multiple box plots are
;; displayed, they should be placed GAP interval between each other, in order
;; to look good.
(: box-and-whisker (->* [(Sequenceof Real)]
                         [#:weights (U (Sequenceof Real) #f)
                          #:x Real
                          #:invert? Boolean
                          #:label (U String pict #f)
                          #:iqr-scale Nonnegative-Real
                          #:width Nonnegative-Real

                          #:box-color Plot-Color
                          #:box-style Plot-Brush-Style
                          #:box-line-color Plot-Color
                          #:box-line-width Nonnegative-Real
                          #:box-line-style Plot-Pen-Style
                          #:box-alpha Nonnegative-Real

                          #:show-outliers? Boolean
                          #:outlier-color Plot-Color
                          #:outlier-sym Point-Sym
                          #:outlier-fill-color (U Plot-Color 'auto)
                          #:outlier-size Nonnegative-Real
                          #:outlier-line-width Nonnegative-Real
                          #:outlier-alpha Nonnegative-Real

                          #:show-whiskers? Boolean
                          #:whisker-color Plot-Color
                          #:whisker-width Nonnegative-Real
                          #:whisker-style Plot-Pen-Style
                          #:whisker-alpha Nonnegative-Real

                          #:show-median? Boolean
                          #:median-color Plot-Color
                          #:median-width Nonnegative-Real
                          #:median-style Plot-Pen-Style
                          #:median-alpha Nonnegative-Real

                          #:add-ticks? Boolean
                          #:far-ticks? Boolean]
                         renderer2d))
(define (box-and-whisker
         vs
         #:weights [ws #f]
         #:x [x 0]
         #:invert? (invert? #f)
         #:label (label #f)
         #:iqr-scale (iqr-scale 1.5)

         #:width (width 1)

         #:box-color (box-color (rectangle-color))
         #:box-style (box-style (rectangle-style))
         #:box-line-color (box-line-color (rectangle-line-color))
         #:box-line-width (box-line-width (rectangle-line-width))
         #:box-line-style (box-line-style (rectangle-line-style))
         #:box-alpha (box-alpha (rectangle-alpha))

         #:show-outliers? (show-outliers? #t)
         #:outlier-color (outlier-color (point-color))
         #:outlier-sym (outlier-sym (point-sym))
         #:outlier-fill-color (outlier-fill-color 'auto)
         #:outlier-size (outlier-size (point-size))
         #:outlier-line-width (outlier-line-width (point-line-width))
         #:outlier-alpha (outlier-alpha (point-alpha))

         #:show-whiskers? (show-whiskers? #t)
         #:whisker-color (whisker-color (line-color))
         #:whisker-width (whisker-width (line-width))
         #:whisker-style (whisker-style (line-style))
         #:whisker-alpha (whisker-alpha (line-alpha))

         #:show-median? (show-median? #t)
         #:median-color (median-color (line-color))
         #:median-width (median-width (line-width))
         #:median-style (median-style (line-style))
         #:median-alpha (median-alpha (line-alpha))

         #:add-ticks? [add-ticks? #t]
         #:far-ticks? [far-ticks? #f])

  (define fail/kw (make-raise-keyword-error 'box-and-whisker))
  (cond
    [(not (and (rational? width) (positive? width)))  (fail/kw "positive rational" '#:width width)]
    [(not (rational? box-line-width))  (fail/kw "rational?" '#:box-line-width box-line-width)]
    [(or (not (rational? box-alpha)) (> box-alpha 1) (< box-alpha 0))
     (fail/kw "real in [0,1]" '#:box-alpha box-alpha)]

    [(not (rational? outlier-size)) (fail/kw "rational?" '#:outlier-size outlier-size)]
    [(or (not (rational? outlier-alpha)) (> outlier-alpha 1) (< outlier-alpha 0))
     (fail/kw "real in [0,1]" '#:outlier-alpha outlier-alpha)]
    [(not (rational? outlier-line-width))
     (fail/kw "rational?" '#:outlier-line-width outlier-line-width)]

    [(or (not (rational? whisker-alpha)) (> whisker-alpha 1) (< whisker-alpha 0))
     (fail/kw "real in [0,1]" '#:whisker-alpha whisker-alpha)]
    [(not (rational? whisker-width))
     (fail/kw "rational?" '#:whisker-width whisker-width)]

    [(or (not (rational? median-alpha)) (> median-alpha 1) (< median-alpha 0))
     (fail/kw "real in [0,1]" '#:median-alpha median-alpha)]
    [(not (rational? median-width))
     (fail/kw "rational?" '#:median-width median-width)])

  (match-define (bnw-data q1 median q3 lower-whisker upper-whisker outliers)
    (samples->bnw-data vs ws #:iqr-scale iqr-scale))
  (define y-min*
    (if (null? outliers)
        lower-whisker
        (min lower-whisker (apply min outliers))))
  (define y-max*
    (if (null? outliers)
        upper-whisker
        (max upper-whisker (apply max outliers))))

  ;; Make the Y range slightly larger to make sure outliers fit in nicely at
  ;; the edges.
  (define y-room (* (- y-max* y-min*) 0.07))
  (define y-min (- y-min* y-room))
  (define y-max (+ y-max* y-room))

  (define x-min* (- x (/ width 2)))
  (define x-max* (+ x (/ width 2)))
  (define x-room (* (- x-max* x-min*) 0.07))
  (define x-min (- x-min* x-room))
  (define x-max (+ x-max* x-room))

  (: maybe-invert (All (A) (-> A A (Vectorof A))))
  (define maybe-invert (if invert? (lambda (x y) (vector y x)) vector))

  (define outlier-fill-color*
    (if (eq? outlier-fill-color 'auto) (->pen-color outlier-color) outlier-fill-color))

  (define outlier-vs
    (map (lambda ([y : Real]) (maybe-invert x y)) outliers))

  (renderer2d
   ;; bounds-rect
   (if invert?
       (vector (ivl y-min y-max) (ivl x-min x-max))
       (vector (ivl x-min x-max) (ivl y-min y-max)))
   ;; bounds-fun
   #f
   ;; ticks-fun

   ;; NOTE: for a box and whiskers plot, it does not make sense to put ticks
   ;; on the X axis unless they are the actual box and whiskers labels, so we
   ;; don't use default-ticks-fun here.
   (if (and (string? label) add-ticks?)
       (discrete-histogram-ticks-fun
        (list label) (list x) add-ticks? far-ticks? maybe-invert)
       (make-one-axis-default-ticks-fun invert?))
   ;; label
   (and label
        (lambda (_)
          (rectangle-legend-entry
           label box-color box-style box-line-color box-line-width box-line-style)))
   ;; render-proc
   (renderer-proc
    q1 median q3 lower-whisker upper-whisker outlier-vs
    x width invert?
    box-color box-style box-line-color box-line-style box-line-width box-alpha
    show-median? median-color median-style median-width median-alpha
    show-whiskers? whisker-color whisker-style whisker-width whisker-alpha
    show-outliers? outlier-color outlier-sym outlier-fill-color* outlier-size outlier-line-width outlier-alpha)))
