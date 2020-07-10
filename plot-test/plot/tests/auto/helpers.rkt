#lang racket
(require plot racket/draw rackunit)

;; Unless overridden, use these dimensions for the plots we generate for test
;; purposes.
(plot-width 1024)
(plot-height 768)

(define mock-record-dc%
  (class record-dc%
    (init)
    (super-new)
    (define/override (get-text-extent text (font #f) (combine? #f) (offset 0))
      (values (* 10 (string-length text)) 10 0 0))))

(define (generate-draw-steps renderer-tree)
  (define dc (new mock-record-dc% [width (plot-width)] [height (plot-height)]))
  (plot/dc renderer-tree dc 0 0 (plot-width) (plot-height))
  (send dc get-recorded-datum))

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

(define (check-draw-steps plot-function saved-steps-file)
  (define saved (call-with-input-file saved-steps-file read))
  (define current (plot-function generate-draw-steps))
  (unless (same-draw-steps? saved current)
    ;; Save the current draw steps to file, so they can be compared against
    ;; the original (and maybe updated)
    (define-values (base name must-be-dir?) (split-path saved-steps-file))
    (define data-file (build-path
                       base
                       (string-append "new-" (path->string name))))
    (call-with-output-file data-file (lambda (out) (write current out)) #:exists 'replace)
    ;; Also generate an image of the current plot
    (define image-file (path-replace-extension data-file ".png"))
    (plot-function (lambda (rt) (plot-file rt image-file)))
    (fail (format "draw steps not the same, new set written to ~a" data-file))))

(provide generate-draw-steps same-draw-steps? check-draw-steps)
