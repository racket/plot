#lang racket
(require plot racket/draw rackunit file/gzip file/gunzip)

(provide check-draw-steps check-draw-steps-3d)

;; Helper functions to write plot tests which verify that the renderers work
;; correctly.  See `check-draw-steps` and `check-draw-steps-3d`.
;;
;; The overall testing strategy is that the user verifies a set of renderers
;; to work correctly, generates some draw steps and a sample image and uses
;; `check-draw-steps` (or `check-draw-steps-3d` for 3d plots) to verify that
;; the plot package continues to render the plots using the same draw commands
;; (therefore producing identical plots).
;;
;; If a test fails, the user has a sample image to compare what went wrong as
;; well as a new set of draw steps to compare against the saved one.

;; Unless overridden, use these dimensions for the plots we generate for test
;; purposes.
(plot-width 1024)
(plot-height 768)

;; A record-dc% to store the draw commands issued by plot or plot3d.  We
;; override the font related functions to return constant and predictable
;; values -- this allows the tests to generate and verify the same draw
;; commands on different platforms where, due to different fonts installed,
;; the positions of various plot elements would be slightly different.
(define mock-record-dc%
  (class record-dc%
    (init)
    (super-new)
    (define/override (get-text-extent text (font #f) (combine? #f) (offset 0))
      (values (* 10 (string-length text)) 10 0 0))
    (define/override (get-char-width) 10)
    (define/override (get-char-height) 10)
    ))

;; Generate the draw steps required to plot the RENDERER-TREE.  This function
;; creates a `mock-record-dc%`, uses `plot/dc` to render to this DC and
;; retrieves the recorded steps from it.
(define (generate-draw-steps renderer-tree
                             #:x-min (x-min #f)
                             #:x-max (x-max #f)
                             #:y-min (y-min #f)
                             #:y-max (y-max #f)
                             #:width (width (plot-width))
                             #:height (height (plot-height))
                             #:title (title (plot-title))
                             #:x-label (x-label (plot-x-label))
                             #:y-label (y-label (plot-y-label))
                             #:legend-anchor (legend-anchor (plot-legend-anchor)))
  (define dc (new mock-record-dc% [width width] [height height]))
  (plot/dc renderer-tree dc 0 0 width height
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
           #:title title
           #:x-label x-label #:y-label y-label
           #:legend-anchor legend-anchor)
  (send dc get-recorded-datum))

;; Same as `generate-draw-steps` but uses `plot3d/dc` and should be used for
;; 3D plots.
(define (generate-draw-steps-3d renderer-tree
                                #:x-min (x-min #f)
                                #:x-max (x-max #f)
                                #:y-min (y-min #f)
                                #:y-max (y-max #f)
                                #:z-min (z-min #f)
                                #:z-max (z-max #f)
                                #:width (width (plot-width))
                                #:height (height (plot-height))
                                #:title (title #f)
                                #:x-label (x-label (plot-x-label))
                                #:y-label (y-label (plot-y-label))
                                #:z-label (z-label (plot-z-label))
                                #:angle (angle (plot3d-angle))
                                #:altitude (altitude (plot3d-altitude))
                                #:legend-anchor (legend-anchor (plot-legend-anchor)))
  (define dc (new mock-record-dc% [width width] [height height]))
  (plot3d/dc renderer-tree dc 0 0 width height
             #:x-min x-min #:x-max x-max
             #:y-min y-min #:y-max y-max
             #:z-min z-min #:z-max z-max
             #:title title
             #:x-label x-label #:y-label y-label #:z-label z-label
             #:angle angle
             #:altitude altitude
             #:legend-anchor legend-anchor)
  (send dc get-recorded-datum))

;; Return #t if two sets of draw steps, SET1 and SET2 are the same, false
;; otherwise.  This function recursively traverses the cons tree of SET1 and
;; SET1 comparing elements using equal?, unless the elements are numbers in
;; which case it checks that the difference between them is small.
(define (same-draw-steps? set1 set2)
  (cond ((and (pair? set1) (pair? set2))
         (and
          (same-draw-steps? (car set1) (car set2))
          (same-draw-steps? (cdr set1) (cdr set2))))
        ((and (number? set1) (number? set2))
         (let ([difference (abs (- set1 set2))])
           (< difference 1e-4)))
        (#t
         (equal? set1 set2))))

;; Write a new set of draw steps to file.  NEW-DRAW-STEPS are the new draw
;; steps, as produced by `generate-draw-steps` or `generate-draw-steps-3d` and
;; ORIGINAL-FILE is the original draw step file.  The data is compressed and
;; saved in a file name prefixed with "new-".
(define (write-new-draw-steps new-draw-steps original-file)
  (define-values (base name must-be-dir?) (split-path original-file))
  (make-directory* base)                ; ensure the base path exists
  (define data-file (build-path
                     base
                     (string-append "new-" (path->string name))))
  (define data (call-with-output-string (lambda (out) (write new-draw-steps out))))
  (call-with-input-string
   data
   (lambda (in)
     (call-with-output-file
       data-file
       (lambda (out)
         (gzip-through-ports in out #f (current-seconds)))
       #:exists 'replace))))

;; Decompress and read draw steps from STEPS-FILE.  If the file does not
;; exist, just return an empty list.
(define (read-draw-steps steps-file)
  (if (file-exists? steps-file)
      (let ()
        (define data
          (call-with-output-string
           (lambda (out)
             (call-with-input-file
               steps-file
               (lambda (in)
                 (gunzip-through-ports in out))))))
        (call-with-input-string data read))
      null))

;; Verify that the draw steps produced by PLOT-FUNCTION match the ones in
;; SAVED-STEPS-FILE.  If they do, this function returns VOID, otherwise it
;; fails using `fail`, so it is suitable for use in rackunit tests.
;;
;; When the function fails, the current set of draw steps as well as an image
;; of the current plot is saved in the same directory as SAVED-STEPS-FILE,
;; with the prefix "new-" they can be compared against the saved data to
;; determine what went wrong.
;;
;; The PLOT-FUNCTION is a function that must be supplied by the user to
;; generate the plot.  Instead of calling `plot` directly, PLOT-FUNCTION will
;; take a function argument which will be used to pass the renderer tree to
;; (this allows calling plot inside a parameterize call).
;;
;; For example to test that the following plot works correctly:
;;
;;     (parameterize ([plot-title "Hello"])
;;       (plot (function (lambda (x) x) -1 1)))
;;
;; You would need to write the following plot function:
;;
;;     (define (plot-function output-fn)
;;       (parameterize ([plot-title "Hello"])
;;         (output-fn (function (lambda (x) x) -1 1)))
;;
;; This way of writing the function allows testing it in different conditions.
;; For example, to generate an interactive plot in the DrRacket REPL you can
;; run:
;;
;;    (plot-function plot)
;;
;; To write the plot to an image:
;;
;;    (plot-function (lambda (rt) (plot-file rt "plot-image.png")))
;;
(define (check-draw-steps plot-function saved-steps-file)
  (define saved (read-draw-steps saved-steps-file))
  (define current (plot-function generate-draw-steps))
  (unless (same-draw-steps? saved current)
    (write-new-draw-steps current saved-steps-file)
    (define-values (base name must-be-dir?) (split-path saved-steps-file))
    (define data-file (build-path
                       base
                       (string-append "new-" (path->string name))))
    ;; Also generate an image of the current plot
    (define image-file (path-replace-extension data-file ".png"))
    (plot-function
     (lambda (rt
              #:x-min (x-min #f)
              #:x-max (x-max #f)
              #:y-min (y-min #f)
              #:y-max (y-max #f)
              #:width (width (plot-width))
              #:height (height (plot-height))
              #:title (title (plot-title))
              #:x-label (x-label (plot-x-label))
              #:y-label (y-label (plot-y-label))
              #:legend-anchor (legend-anchor (plot-legend-anchor)))
       (plot-file rt image-file
                  #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                  #:width width #:height height
                  #:title title
                  #:x-label x-label #:y-label y-label
                  #:legend-anchor legend-anchor)))
    (fail (format "draw steps not the same, new set written to ~a" data-file))))

;; Same as `check-draw-steps` but for 3D plots.
(define (check-draw-steps-3d plot-function saved-steps-file)
  (define saved (read-draw-steps saved-steps-file))
  (define current (plot-function generate-draw-steps-3d))
  (unless (same-draw-steps? saved current)
    (write-new-draw-steps current saved-steps-file)
    (define-values (base name must-be-dir?) (split-path saved-steps-file))
    (define data-file (build-path
                       base
                       (string-append "new-" (path->string name))))
    ;; Also generate an image of the current plot
    (define image-file (path-replace-extension data-file ".png"))
    (plot-function (lambda (rt
                            #:x-min (x-min #f)
                            #:x-max (x-max #f)
                            #:y-min (y-min #f)
                            #:y-max (y-max #f)
                            #:z-min (z-min #f)
                            #:z-max (z-max #f)
                            #:width (width (plot-width))
                            #:height (height (plot-height))
                            #:title (title #f)
                            #:x-label (x-label (plot-x-label))
                            #:y-label (y-label (plot-y-label))
                            #:z-label (z-label (plot-z-label))
                            #:angle (angle (plot3d-angle))
                            #:altitude (altitude (plot3d-altitude))
                            #:legend-anchor (legend-anchor (plot-legend-anchor)))
                     (plot3d-file rt image-file
                                  #:x-min x-min #:x-max x-max
                                  #:y-min y-min #:y-max y-max
                                  #:z-min z-min #:z-max z-max
                                  #:width width #:height height
                                  #:title title
                                  #:x-label x-label
                                  #:y-label y-label
                                  #:z-label z-label
                                  #:angle angle
                                  #:altitude altitude
                                  #:legend-anchor legend-anchor)))
    (fail (format "draw steps not the same, new set written to ~a" data-file))))
