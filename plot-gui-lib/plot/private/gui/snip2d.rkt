#lang racket/base

(require racket/gui/base racket/class racket/contract racket/match racket/list racket/math
         plot/private/common/math
         plot/private/common/format
         plot/private/common/ticks
         plot/private/common/parameters
         plot/private/common/parameter-groups
         plot/private/common/parameter-group
         plot/private/common/draw-attribs
         plot/private/plot2d/plot-area
         plot/private/plot2d/renderer
         plot/private/no-gui/plot2d-utils
         plot/private/common/contract
         "worker-thread.rkt"
         "snip.rkt")

(define plot-mouse-event-callback/c
  (-> (is-a?/c snip%) (is-a?/c mouse-event%) (or/c real? #f) (or/c real? #f) any/c))
(define 2d-plot-snip%/c
  (class/c
   (set-mouse-event-callback (->m (or/c plot-mouse-event-callback/c #f) any/c))
   (set-overlay-renderers (->m (or/c (treeof renderer2d?) #f) any/c))))

(provide
 (contract-out
  [make-2d-plot-snip (unconstrained-domain-> (instanceof/c 2d-plot-snip%/c))]
  [2d-plot-snip% 2d-plot-snip%/c])
 plot-mouse-event-callback/c)

(define update-delay 16)
(define show-zoom-message? #t)

(struct draw-command (animating? plot-bounds-rect width height) #:transparent)

(define 2d-plot-snip%
  (class plot-snip%
    (init init-bm saved-plot-parameters)
    (init-field make-bm plot-bounds-rect area width height)

    (inherit set-bitmap get-bitmap
             get-saved-plot-parameters
             refresh
             stop-message set-message reset-message-timeout
             update-thread-running? set-update
             get-left-down-here?)
    
    (super-make-object init-bm saved-plot-parameters)
    
    (define (set-message-center)
      (match-define (vector x-mid y-mid) (rect-center (send area get-area-bounds-rect)))
      (send this set-message-center x-mid y-mid))
    
    (set-message-center)
    
    (define/override (copy)
      (define c
        (make-object this%
                     (get-bitmap) (get-saved-plot-parameters)
                     make-bm plot-bounds-rect area width height))
      (when mouse-event-callback
        (send c set-mouse-event-callback mouse-event-callback))
      c)

    (define left-click-x 0)
    (define left-click-y 0)
    (define left-drag-x 0)
    (define left-drag-y 0)

    (define plot-bounds-rects empty)

    (define (area-bounds->plot-bounds rect)
      ;; assumes: (rect-known? rect)
      (match-define (vector (ivl area-x-min area-x-max) (ivl area-y-min area-y-max)) rect)
      (match-define (vector x-min y-min) (send area dc->plot (vector area-x-min area-y-min)))
      (match-define (vector x-max y-max) (send area dc->plot (vector area-x-max area-y-max)))
      (vector (ivl x-min x-max) (ivl y-min y-max)))

    (define (plot-bounds->area-bounds rect)
      ;; assumes (rect-known? rect)
      (match-define (vector (ivl plot-x-min plot-x-max) (ivl plot-y-min plot-y-max)) rect)
      (match-define (vector x-min y-min) (send area plot->dc (vector plot-x-min plot-y-min)))
      (match-define (vector x-max y-max) (send area plot->dc (vector plot-x-max plot-y-max)))
      (vector (ivl x-min x-max) (ivl y-min y-max)))
    
    (define (get-new-area-bounds-rect)
      (rect-meet (send area get-area-bounds-rect)
                 (rect-inexact->exact
                  (vector (ivl left-click-x left-drag-x) (ivl left-click-y left-drag-y)))))
    
    (define dragging? #f)
    
    (define zoom-timer #f)
    (define (set-zoom-timer)
      (when (not zoom-timer)
        (set! zoom-timer (make-object timer%
                           (λ ()
                             (set! zoom-timer #f)
                             (refresh))
                           update-delay #t))))
    
    (define (set-click-message)
      (when show-zoom-message?
        (set-message "Click and drag to zoom\n Click to unzoom once")))
    
    (define (zoom-or-unzoom)
      (cond [dragging?
             (set! dragging? #f)
             (define new-rect (area-bounds->plot-bounds (get-new-area-bounds-rect)))
             (cond [(and (rect-rational? new-rect) (not (rect-zero-area? new-rect)))
                    #;(printf "~a: new-plot-bounds-rect = ~v~n"
                              (current-milliseconds) new-rect)
                    (set! plot-bounds-rects (cons plot-bounds-rect plot-bounds-rects))
                    (set! plot-bounds-rect new-rect)
                    (update-plot)]
                   [else
                    (refresh)])]
            [(not (empty? plot-bounds-rects))
             (set! plot-bounds-rect (first plot-bounds-rects))
             (set! plot-bounds-rects (rest plot-bounds-rects))
             (set! show-zoom-message? #f)
             (update-plot)]))
    
    (define (start-update-thread animating?)
      (send this start-update-thread
            (λ () (make-worker-thread
                   (match-lambda
                     [(draw-command animating? plot-bounds-rect width height)
                      (make-bm animating? plot-bounds-rect width height)])))
            (λ (animating?) (draw-command animating? plot-bounds-rect width height))
            (λ (rth)
              (define-values (new-bm new-area)
                (worker-thread-try-get rth (λ () (values #f #f))))
              (cond [(is-a? new-bm bitmap%)
                     (set! area new-area)
                     (set-bitmap new-bm)
                     (set-message-center)
                     #t]
                    [else  #f]))
            animating?))
    
    (define (update-plot)
      (start-update-thread #f)
      (set-update #t))

    (define (zoom-or-unzoom-mouse-event-handler dc x y editorx editory evt)
      (define evt-type (send evt get-event-type))
      (define mouse-x (- (send evt get-x) x))
      (define mouse-y (- (send evt get-y) y))
      (case evt-type
        [(left-down)  (set! left-click-x mouse-x)
                      (set! left-click-y mouse-y)
                      (set! left-drag-x mouse-x)
                      (set! left-drag-y mouse-y)
                      (set! dragging? #f)
                      (set-message #f)
                      (set-zoom-timer)]
        [(left-up)    (set! left-drag-x mouse-x)
                      (set! left-drag-y mouse-y)
                      (zoom-or-unzoom)]
        [(motion)     (cond [(get-left-down-here?)  ; only #t if clicked on snip
                             (when (not (and (= left-drag-x mouse-x)
                                             (= left-drag-y mouse-y)))
                               (set! left-drag-x mouse-x)
                               (set! left-drag-y mouse-y)
                               (set! dragging? #t)
                               (set-zoom-timer))]
                            [(and (not (send evt get-left-down))
                                  (<= 0 mouse-x (send (get-bitmap) get-width))
                                  (<= 0 mouse-y (send (get-bitmap) get-height)))
                             (set-click-message)])]))

    (define mouse-event-callback #f)
    (define mouse-event-handler zoom-or-unzoom-mouse-event-handler)

    (define (user-mouse-event-handler dc x y editorx editory evt)
      (define mouse-x (- (send evt get-x) x))
      (define mouse-y (- (send evt get-y) y))
      (if (rect-contains? (send area get-area-bounds-rect) (vector mouse-x mouse-y))
          (match-let (((vector px py) (send area dc->plot (vector mouse-x mouse-y))))
            (mouse-event-callback this evt px py))
          (mouse-event-callback this evt #f #f)))

    (define/public (set-mouse-event-callback callback)
      (set! mouse-event-callback callback)
      (set! mouse-event-handler
            (if mouse-event-callback
                user-mouse-event-handler
                zoom-or-unzoom-mouse-event-handler)))

    (define the-overlay-renderers #f)

    (define/public (set-overlay-renderers renderers)
      (set! the-overlay-renderers renderers)
      (refresh))

    (define (draw-overlay-renderers dc x y left top right bottom)
      (when the-overlay-renderers
        ;; Implementation notes:
        ;;
        ;; * the `plot-area` routine used to draw plots, expects the origin of
        ;; the DC to be set to the origin or (0, 0) of the plot, see
        ;; `set-origin` call.
        ;;
        ;; * Since the DC origin has been adjusted to start at X, Y, the LEFT,
        ;; TOP, RIGHT and BOTTOM values have to be adjusted accordingly.
        ;;
        ;; * plot Y axis grows upwards (lower values are at the bottom, higher
        ;; values are at the top), draw context Y axis grows downwards (lower
        ;; values are at the top, higher values are at the bottom).  This
        ;; results in some non-obvious `plot->dc` and `dc->plot` calls.
        ;;
        ;; * The area bounded by LEFT, TOP, RIGHT and BOTTOM might cover an
        ;; area outside the plot area (e.g. where axis are drawn).  We need to
        ;; intersect the current plot bounds with this area to obtain the
        ;; final overlay redraw area.
        ;;
        ;; * If the redraw area is at the edge of the visible part of the plot
        ;; snip, we seem to have an off-by-one error and pixels are "left
        ;; over" at the edge.  This is adjusted using the `add1`, `sub1` calls
        ;; below.

        (match-define (vector (ivl cleft cright) (ivl ctop cbottom))
          (plot-bounds->area-bounds plot-bounds-rect))

        (define dc-x-min (max cleft (add1 (- left x))))
        (define dc-x-max (min cright (sub1 (- right x))))
        (define dc-y-min (max ctop (add1 (- top y))))
        (define dc-y-max (min cbottom (sub1 (- bottom y))))

        (when (and (> dc-x-max dc-x-min) (> dc-y-max dc-y-min))
          (define overlay-plot-bounds
            (area-bounds->plot-bounds (vector (ivl dc-x-min dc-x-max) (ivl dc-y-min dc-y-max))))

          (define-values (scale-x scale-y) (send dc get-scale))
          (define-values (origin-x origin-y) (send dc get-origin))
          (send dc set-origin (+ origin-x (* scale-x x)) (+ origin-y (* scale-y y)))

          ;; Use the same plot parameters as the main plot -- this ensures
          ;; that any axis transforms (e.g. logarithmic, stretch, etc) are
          ;; applied to the overlays as well.  We than omit the decorations
          ;; and specify a transparent background so the main plot underneath
          ;; is visible.
          (parameterize/group ([plot-parameters  (get-saved-plot-parameters)])
            (parameterize ([plot-decorations? #f]
                           [plot-background-alpha 0])
              ;; The new overlay area has to be constructed inside the
              ;; parameterize call, as it picks up the value of the
              ;; plot-decorations? parameter.
              (define overlay-area
                (make-object 2d-plot-area%
                             overlay-plot-bounds
                             '() '() '() '()
                             dc
                             dc-x-min dc-y-min
                             (- dc-x-max dc-x-min) (- dc-y-max dc-y-min)))
              (plot-area overlay-area the-overlay-renderers)))

          (send dc set-origin origin-x origin-y))))

    (define/override (on-event dc x y editorx editory evt)
      (apply mouse-event-handler dc x y editorx editory evt '())
      (super on-event dc x y editorx editory evt))
    
    (define (draw-selection dc dc-x-min dc-y-min rect)
      (with-handlers ([exn?  (λ (e) (printf "draw-selection: ~v~n" e))])
      (when (and (rect-rational? rect) (not (rect-zero-area? rect)))
        (define width (send (get-bitmap) get-width))
        (define height (send (get-bitmap) get-height))
        
        (define-values (scale-x scale-y) (send dc get-scale))
        (define-values (origin-x origin-y) (send dc get-origin))
        (define smoothing (send dc get-smoothing))
        (define text-mode (send dc get-text-mode))
        (define font (send dc get-font))
        (define pen (send dc get-pen))
        (define brush (send dc get-brush))
        (define alpha (send dc get-alpha))
        (define text-foreground (send dc get-text-foreground))
        
        (send dc set-origin
              (+ origin-x (* scale-x dc-x-min))
              (+ origin-y (* scale-y dc-y-min)))
        (send dc set-smoothing 'smoothed)
        (send dc set-text-mode 'transparent)
        (send dc set-font (send the-font-list find-or-create-font
                                (real->font-size (plot-font-size))
                                (plot-font-face)
                                (plot-font-family)
                                'normal
                                'normal))
        
        (match-define (vector (ivl sel-x-min sel-x-max) (ivl sel-y-min sel-y-max)) rect)
        (define sel-x-size (- sel-x-max sel-x-min))
        (define sel-y-size (- sel-y-max sel-y-min))
        
        (define select-color (get-highlight-background-color))
        (define pen-color (color->color% (->pen-color (plot-foreground))))
        (define brush-color (color->color% (->brush-color (plot-background))))
        
        ;; inside selection
        (send dc set-pen select-color 1 'transparent)
        (send dc set-brush select-color 'solid)
        (send dc set-alpha 1/4)
        (send dc draw-rectangle sel-x-min sel-y-min sel-x-size sel-y-size)
        
        ;; selection border
        (send dc set-pen select-color (* 1/2 (plot-line-width)) 'solid)
        (send dc set-brush select-color 'transparent)
        (send dc set-alpha 3/4)
        (send dc draw-rectangle sel-x-min sel-y-min sel-x-size sel-y-size)
        
        ;; format side labels
        (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) plot-bounds-rect)
        (match-define (vector (ivl new-x-min new-x-max) (ivl new-y-min new-y-max))
          (area-bounds->plot-bounds rect))
        
        (match-define (list new-x-min-str new-x-max-str)
          (format-tick-labels (plot-x-ticks) x-min x-max (list new-x-min new-x-max)))
        
        (match-define (list new-y-min-str new-y-max-str)
          (format-tick-labels (plot-y-ticks) y-min y-max (list new-y-min new-y-max)))
        
        ;; draw side labels
        (match-define (vector (ivl new-area-x-min new-area-x-max)
                              (ivl new-area-y-min new-area-y-max))
          rect)
        (define new-area-x-mid (* 1/2 (+ new-area-x-min new-area-x-max)))
        (define new-area-y-mid (* 1/2 (+ new-area-y-min new-area-y-max)))
        
        (send dc set-alpha 1)
        
        (send this draw-text dc new-x-min-str new-area-x-min new-area-y-mid
              pen-color brush-color 'center (* 1/2 pi) 0)
        (send this draw-text dc new-x-max-str new-area-x-max new-area-y-mid
              pen-color brush-color 'center (* 1/2 pi) 0)
        (send this draw-text dc new-y-min-str new-area-x-mid new-area-y-max
              pen-color brush-color 'center 0 0)
        (send this draw-text dc new-y-max-str new-area-x-mid new-area-y-min
              pen-color brush-color 'center 0 0)
        
        (send dc set-origin origin-x origin-y)
        (send dc set-smoothing smoothing)
        (send dc set-text-mode text-mode)
        (send dc set-font font)
        (send dc set-pen pen)
        (send dc set-brush brush)
        (send dc set-alpha alpha)
        (send dc set-text-foreground text-foreground))))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      ;(printf "~a: drawing~n" (current-milliseconds))
      (super draw dc x y left top right bottom dx dy draw-caret)
      (when dragging?
        (parameterize/group ([plot-parameters  (get-saved-plot-parameters)])
          (draw-selection dc x y (get-new-area-bounds-rect))))
      (draw-overlay-renderers dc x y left top right bottom))

    (define/override (resize w h)
      (when (not (and (= w width) (= h height)))
        (set! width w)
        (set! height h)
        (stop-message)
        (when (not (update-thread-running?))
          (start-update-thread #t))
        (set-update #t))
      (super resize w h))
    ))

(define (make-2d-plot-snip
         init-bm saved-plot-parameters
         make-bm plot-bounds-rect area width height)
  (make-object 2d-plot-snip%
    init-bm saved-plot-parameters
    make-bm plot-bounds-rect area width height))
