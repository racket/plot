#lang racket
(require plot racket/gui/base racket/draw rackunit)

;; Refactoring done as part of #49 changed the number of init fields for the
;; 2d-plot-area%, but didn't update the overlay-renderer plot area.  This unit
;; test exists to ensure that the overlay plot area is created at least once
;; during the testing process, and any initialization problems are caught
;; early.
;;
;; An additional bug was found where the `set-overlay-renderers` would only
;; accept a flat list of renderers instead of a renderer tree as advertised by
;; the method contract -- this unit tests also verifies that
;; `set-overlay-renderers` does in fact accept renderer trees.

(define snip-overlay-renderers
  (test-suite
   "snip-overlay-renderers"
   (test-case "snip-overlay-renderers"

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

     ;; Construct a dummy DC on which to draw the snip
     (define dc (new record-dc% [width 800] [height 600]))
     
     (after
      (check-not-exn
       (lambda ()
         ;; The 2d-plot-area% for the overlay area is created on-demand when
         ;; overlay renderers are present.  Also the overlay renderers are a
         ;; renderer tree (nested lists) to ensure that these are accepted by
         ;; the method.
         (send snip set-overlay-renderers
               (list (function cos -3 3) (list (vrule -1) (vrule 1))))
         (send snip draw dc 0 0 0 0 10 10 0 0 'no-caret)

         ;; Ensure that #f handled correctly, see
         ;; https://github.com/racket/racket/issues/3753
         (send snip set-overlay-renderers #f)
         (send snip draw dc 0 0 0 0 10 10 0 0 'no-caret)))
      (send tl show #f)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests snip-overlay-renderers))
