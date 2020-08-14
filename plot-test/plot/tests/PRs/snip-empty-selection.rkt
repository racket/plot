#lang racket
(require plot racket/gui/base racket/draw rackunit)

;; 2D interactive plots would occasionally thrown an exception if the user
;; clicked on the plot.  This was because the system would interpret it as a
;; drag-select operation which started and finished at the same place,
;; resulting in an empty selection -- empty selections are represented using
;; +nan.0, which cannot be converted to an exact number.
;;
;; In this test we simulate an empty selection using mouse events sent
;; directly to the snips `on-event` method.

(define empty-selection-zoom
  (test-suite
   "empty-selection-zoom"
   (test-case "empty-selection-zoom"

     ;; Scaffolding, construct a pasteboard to hold our plot snip
     (define tl (new frame% [label "hello"] [width 800] [height 600]))
     (define pb [new pasteboard%])
     (define editor (new editor-canvas% [parent tl] [editor pb]))

     ;; Construct the plot snip and add it to the pasteboard so it has an
     ;; administrator.
     (define snip (plot-snip (function sin -3 3)))
     (send pb insert snip)

     ;; Show the frame -- this is not strictly needed, but will ensure that
     ;; all widgets have their proper dimensions set and mouse events will be
     ;; "interpreted" according to correct snip positions.
     (send tl show #t)

     ;; Construct a dummy DC and the left-click, drag, left-release events
     ;; which will simulate the drag-selection on the snip.  Note that the
     ;; start and end events have the same X, Y coordinates, which will result
     ;; in an empty rectangle being selected.
     (define dc (new record-dc% [width 800] [height 600]))
     (define click (new mouse-event% [event-type 'left-down] [x 10] [y 10]))
     (define drag (new mouse-event% [event-type 'motion] [left-down #t] [x 11] [y 11]))
     (define unclick (new mouse-event% [event-type 'left-up] [x 10] [y 10]))

     ;; Send the snip the events -- the snip will think the user is selecting
     ;; a region on the plot.
     (send snip on-event dc 0 0 0 0 click)
     (send snip on-event dc 0 0 0 0 drag)
     (after
      (check-not-exn
       ;; zoom is triggered when the user releases the mouse event -- this
       ;; used to throw an exception as it tried to operate on an empty
       ;; rectangle, containing +nan.0 numbers.
       (lambda () (send snip on-event dc 0 0 0 0 unclick)))
      (send tl show #f)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests empty-selection-zoom))
