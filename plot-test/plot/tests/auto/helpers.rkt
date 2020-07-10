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

#;(define (same-draw-commands? set1 set2)
  (cond ((and (pair? set1) (pair? set2))
         (and
          (same-draw-commands? (car set1) (car set2))
          (same-draw-commands? (cdr set1) (cdr set2))))
        ((and (number? set1) (number? set2))
         (let ([difference (abs (- set1 set2))])
           (< difference 1e-4)))
        (#t
         (equal? set1 set2))))

#;(define (same-draw-steps? set1 set2)
  (for/and ([step1 (in-list set1)]
            [step2 (in-list set2)])
    (define command1 (car step1))
    (define command2 (car step2))
    (define result
      (and (equal? command1 command2)
           (case command1
             ((draw-text)
              (match-define (list 'draw-text text1 x1 y1 combine1? offset1 angle1) step1)
              (match-define (list 'draw-text text2 x2 y2 combine2? offset2 angle2) step2)
              (and (equal? text1 text2)
                   (equal? combine1? combine2?)
                   (let ([difference (abs (- offset1 offset2))])
                     (< difference 1e-4))
                   (let ([difference (abs (- angle1 angle2))])
                     (< difference 1e-4))))
             (else
              (same-draw-commands? step1 step2)))))
    (unless result
      (printf "mismatch at ~a, ~a~%" step1 step2))
    result))

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
