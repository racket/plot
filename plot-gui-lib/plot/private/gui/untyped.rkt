#lang racket/base

(require racket/lazy-require)

(provide (rename-out [-make-2d-plot-snip  make-2d-plot-snip]
                     [-make-3d-plot-snip  make-3d-plot-snip]
                     [-make-snip-frame  make-snip-frame]))

;; Require lazily, in case someone wants to just (require plot) in a headless setup
(lazy-require ["snip2d.rkt" (make-2d-plot-snip)]
              ["snip3d.rkt" (make-3d-plot-snip)]
              ["gui.rkt" (make-snip-frame)])

(define (-make-2d-plot-snip
         init-bm saved-plot-parameters
         make-bm plot-bounds-rect area-bounds-rect area-bounds->plot-bounds width height)
  (make-2d-plot-snip
   init-bm saved-plot-parameters
   make-bm plot-bounds-rect area-bounds-rect area-bounds->plot-bounds width height))

(define (-make-3d-plot-snip
         init-bm saved-plot-parameters
         make-bm angle altitude width height)
  (make-3d-plot-snip
   init-bm saved-plot-parameters
   make-bm angle altitude width height))

(define (-make-snip-frame snip width height label)
  (make-snip-frame snip width height label))
