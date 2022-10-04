#lang racket/base

(require racket/gui/base racket/class racket/match
         plot/private/common/math
         plot/private/common/parameters
         (submod plot/private/common/plotmetrics untyped)
         "worker-thread.rkt"
         "snip.rkt")

(provide 3d-plot-snip% make-3d-plot-snip)

(define show-rotate-message? #t)

(struct draw-command (animating? angle altitude width height) #:transparent)

(define (clamp x mn mx) (min* (max* x mn) mx))

(define 3d-plot-snip%
  (class* plot-snip% (plot-metrics<%>)
    (init init-bm saved-plot-parameters)
    (init-field make-bm angle altitude area width height)
    
    (inherit set-bitmap get-bitmap
             get-saved-plot-parameters
             set-message stop-message reset-message-timeout
             update-thread-running? set-update
             get-left-down-here?)
    
    (super-make-object init-bm saved-plot-parameters)
    
    (define (set-message-center)
      (match-define (vector x-mid y-mid) (rect-center (send area get-area-bounds-rect)))
      (send this set-message-center x-mid y-mid))
    
    (set-message-center)

    (define/override (copy)
      (make-object this%
        (get-bitmap) (get-saved-plot-parameters)
        make-bm angle altitude area width height))
    
    (define mouse-x 0)
    (define mouse-y 0)
    (define left-click-x 0)
    (define left-click-y 0)
    
    (define last-angle angle)
    (define last-altitude altitude)
    
    (define (set-angle!)
      (define degrees-per-pixel (/ 180 (send (get-bitmap) get-width)))
      (define dx (- mouse-x left-click-x))
      (set! angle (real-modulo (+ last-angle (* dx degrees-per-pixel)) 360)))
    
    (define (set-altitude!)
      (define degrees-per-pixel (/ 180 (send (get-bitmap) get-height)))
      (define dy (- mouse-y left-click-y))
      (set! altitude (clamp (+ last-altitude (* dy degrees-per-pixel)) -90 90)))
    
    (define (start-update-thread animating?)
      (send this start-update-thread
            (λ () (make-worker-thread
                   (match-lambda
                     [(draw-command animating? angle altitude width height)
                      (make-bm animating? angle altitude width height)])))
            (λ (animating?) (draw-command animating? angle altitude width height))
            (λ (rth)
              (define-values (new-bm new-area)
                (worker-thread-try-get rth (λ () (values #f #f))))
              (cond [(is-a? new-bm bitmap%)
                     (set! area new-area)
                     (set! plot-metrics-ok? #f)
                     (set-bitmap new-bm)
                     (set-message-center)
                     (when (not (and (= last-angle angle)
                                     (= last-altitude altitude)))
                       (set-angles-message))
                     #t]
                    [else  #f]))
            animating?))
    
    (define (set-angles-message)
      (set-message (format "angle = ~a\naltitude = ~a"
                           (number->string (inexact->exact (round angle)))
                           (number->string (inexact->exact (round altitude))))
                   #:refresh? #f))
    
    (define (set-click-message)
      (when show-rotate-message?
        (set-message "Click and drag to rotate")))
    
    (define (on-left-down)
      (set! left-click-x mouse-x)
      (set! left-click-y mouse-y)
      (set! last-angle angle)
      (set! last-altitude altitude)
      (set-angles-message)
      (start-update-thread #t)
      (set-update #t))
    
    (define (on-left-up)
      (when (get-left-down-here?)
        (set! last-angle angle)
        (set! last-altitude altitude)
        (when (update-thread-running?)
          (start-update-thread #f)
          (set-update #t))))
    
    (define (on-motion evt last-mouse-x last-mouse-y)
      (cond [(get-left-down-here?)
             (when (not (and (= last-mouse-x mouse-x)
                             (= last-mouse-y mouse-y)))
               (set! show-rotate-message? #f)
               (set-angle!)
               (set-altitude!)
               (unless (update-thread-running?)
                 (start-update-thread #t))
               (set-update #t))]
            [(and (not (send evt get-left-down))
                  (<= 0 mouse-x (send (get-bitmap) get-width))
                  (<= 0 mouse-y (send (get-bitmap) get-height)))
             (set-click-message)]))
    
    (define/override (on-event dc x y editorx editory evt)
      (define evt-type (send evt get-event-type))
      (define last-mouse-x mouse-x)
      (define last-mouse-y mouse-y)
      (set! mouse-x (- (send evt get-x) x))
      (set! mouse-y (- (send evt get-y) y))
      (case evt-type
        [(left-down)  (on-left-down)]
        [(left-up)    (on-left-up)]
        [(motion)     (on-motion evt last-mouse-x last-mouse-y)])
      (super on-event dc x y editorx editory evt))
    
    (define/override (resize w h)
      (when (not (and (= w width) (= h height)))
        (set! width w)
        (set! height h)
        (stop-message)
        (when (not (update-thread-running?))
          (start-update-thread #t))
        (set-update #t))
      (super resize w h))

    (define plot-metrics-ok? #f)
    (match-define (list bounds ->dc ->plot plane)
      (send area get-plot-metrics-functions))
    (define (update-metrics)
      (match-define (list new-bounds new-->dc new-->plot new-plane)
        (send area get-plot-metrics-functions))
      (set! bounds new-bounds)
      (set! ->dc new-->dc)
      (set! ->plot new-->plot)
      (set! plane new-plane)
      (set! plot-metrics-ok? #t))
    (define/public (get-plot-bounds) (unless plot-metrics-ok? (update-metrics)) (bounds))
    (define/public (plot->dc coords) (unless plot-metrics-ok? (update-metrics)) (->dc coords))
    (define/public (dc->plot coords) (unless plot-metrics-ok? (update-metrics)) (->plot coords))
    (define/public (plane-vector)    (unless plot-metrics-ok? (update-metrics)) (plane))
    (define/public (get-plot-metrics-functions) (unless plot-metrics-ok? (update-metrics)) (list bounds ->dc ->plot plane))
    ))

(define (make-3d-plot-snip
         init-bm saved-plot-parameters
         make-bm angle altitude area width height)
  (make-object 3d-plot-snip%
    init-bm saved-plot-parameters
    make-bm angle altitude area width height))
