#lang typed/racket/base

(require plot/no-gui
         "private/gui/plot2d.rkt"
         "private/gui/plot3d.rkt")

(provide (all-from-out plot/no-gui)
         plot-snip
         plot-frame
         plot
         plot3d-snip
         plot3d-frame
         plot3d)
