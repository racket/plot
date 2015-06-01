#lang racket

;; Ensure plot/dc, plot-bitmap, plot3d/dc, and plot3d-bitmap are fast enough for smooth animations

(require racket/gui
         plot)

(plot-animating? #t)

(define max-fps 30)

(define frame (new frame% [label "Animation Test"] [width 400] [height 400]))
(define canvas (new canvas% [parent frame] [style '(no-autoclear)]))
(define dc (send canvas get-dc))

(send frame show #t)
(sleep/yield 2)

(let loop ([n 0])
  (when (and (< n 100) (send frame is-shown?))
    (define-values (width height)
      (let-values ([(width height)  (send dc get-size)])
        (values (max 1 (exact-ceiling width)) (max 1 (exact-ceiling height)))))
    (define-values (_ cpu real gc)
      (time-apply
       (λ ()
         (cond
           [(zero? (modulo n 2))
            (send dc draw-bitmap
                  (plot3d-bitmap
                   (contour-intervals3d (λ (x y)
                                          (* (sin (* n #i1/40 2 pi)) (+ (sin x) (cos y))))
                                        -5 5 -5 5)
                   #:width width #:height height
                   #:z-min -2 #:z-max 2
                   #:title "Animated Plot"
                   #:x-label "x"
                   #:y-label "y"
                   #:z-label "z"
                   #:legend-anchor 'top-left)
                  0 0)]
           [else
            (define bm (make-bitmap width height))
            (define bm-dc (make-object bitmap-dc% bm))
            (plot3d/dc (contour-intervals3d (λ (x y)
                                              (* (sin (* n #i1/40 2 pi)) (+ (sin x) (cos y))))
                                            -5 5 -5 5)
                       bm-dc 0 0 width height
                       #:z-min -2 #:z-max 2
                       #:title "Animated Plot"
                       #:x-label "x"
                       #:y-label "y"
                       #:z-label "z"
                       #:legend-anchor 'top-left)
            (send dc draw-bitmap bm 0 0)]))
       empty))
    (sleep/yield (* #i1/1000 (max 1 (- (/ 1000 max-fps) real))))
    (loop (+ n 1))))

(let loop ([n 0])
  (when (and (< n 100) (send frame is-shown?))
    (define-values (width height)
      (let-values ([(width height)  (send dc get-size)])
        (values (max 1 (exact-ceiling width)) (max 1 (exact-ceiling height)))))
    (define-values (_ cpu real gc)
      (time-apply
       (λ ()
         (cond
           [(zero? (modulo n 2))
            (send dc draw-bitmap
                  (plot-bitmap
                   (function-interval (λ (x) (* (sin (* n #i1/40 2 pi)) (sin x)))
                                      (λ (x) (* (sin (* n #i1/40 2 pi)) (cos x)))
                                      -5 5)
                   #:width width #:height height
                   #:y-min -1 #:y-max 1
                   #:title "Animated Plot"
                   #:x-label "x"
                   #:y-label "y"
                   #:legend-anchor 'top-left)
                  0 0)]
           [else
            (define bm (make-bitmap width height))
            (define bm-dc (make-object bitmap-dc% bm))
            (plot/dc (function-interval (λ (x) (* (sin (* n #i1/40 2 pi)) (sin x)))
                                        (λ (x) (* (sin (* n #i1/40 2 pi)) (cos x)))
                                        -5 5)
                     bm-dc 0 0 width height
                     #:y-min -1 #:y-max 1
                     #:title "Animated Plot"
                     #:x-label "x"
                     #:y-label "y"
                     #:legend-anchor 'top-left)
            (send dc draw-bitmap bm 0 0)]))
       '()))
    (sleep/yield (* #i1/1000 (max 1 (- (/ 1000 max-fps) real))))
    (loop (+ n 1))))

(sleep/yield 2)
(send frame show #f)
