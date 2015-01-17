#lang racket/base

(require racket/gui/base racket/class racket/match racket/list racket/math
         plot/private/common/math
         plot/private/common/format
         plot/private/common/ticks
         plot/private/common/parameters
         plot/private/common/parameter-groups
         plot/private/common/parameter-group
         plot/private/common/draw-attribs
         "worker-thread.rkt"
         "snip.rkt")

(provide 2d-plot-snip% make-2d-plot-snip)

(define update-delay 16)
(define show-zoom-message? #t)

(struct draw-command (animating? plot-bounds-rect width height) #:transparent)

(define 2d-plot-snip%
  (class plot-snip%
    (init init-bm saved-plot-parameters)
    (init-field make-bm plot-bounds-rect area-bounds-rect area-bounds->plot-bounds width height)
    
    (inherit set-bitmap get-bitmap
             get-saved-plot-parameters
             refresh
             stop-message set-message reset-message-timeout
             update-thread-running? set-update
             get-left-down-here?)
    
    (super-make-object init-bm saved-plot-parameters)
    
    (define (set-message-center)
      (match-define (vector x-mid y-mid) (rect-center area-bounds-rect))
      (send this set-message-center x-mid y-mid))
    
    (set-message-center)
    
    (define/override (copy)
      (make-object this%
        (get-bitmap) (get-saved-plot-parameters)
        make-bm plot-bounds-rect area-bounds-rect area-bounds->plot-bounds width height))
    
    (define left-click-x 0)
    (define left-click-y 0)
    (define left-drag-x 0)
    (define left-drag-y 0)
    
    (define plot-bounds-rects empty)
    
    (define (get-new-area-bounds-rect)
      (rect-meet area-bounds-rect
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
              (define-values (new-bm new-area-bounds-rect new-area-bounds->plot-bounds)
                (worker-thread-try-get rth (λ () (values #f #f #f))))
              (cond [(is-a? new-bm bitmap%)
                     (set! area-bounds-rect new-area-bounds-rect)
                     (set! area-bounds->plot-bounds new-area-bounds->plot-bounds)
                     (set-bitmap new-bm)
                     (set-message-center)
                     #t]
                    [else  #f]))
            animating?))
    
    (define (update-plot)
      (start-update-thread #f)
      (set-update #t))
    
    (define/override (on-event dc x y editorx editory evt)
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
                             (set-click-message)])])
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
          (draw-selection dc x y (get-new-area-bounds-rect)))))
    
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
         make-bm plot-bounds-rect area-bounds-rect area-bounds->plot-bounds width height)
  (make-object 2d-plot-snip%
    init-bm saved-plot-parameters
    make-bm plot-bounds-rect area-bounds-rect area-bounds->plot-bounds width height))
