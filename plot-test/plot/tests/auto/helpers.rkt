#lang racket
(require plot racket/draw rackunit)

(plot-width 300)
(plot-height 300)

(define (generate-draw-steps renderer-tree)
  (define dc (new record-dc% [width (plot-width)] [height (plot-height)]))
  (plot/dc renderer-tree dc 0 0 (plot-width) (plot-height))
  (send dc get-recorded-datum))

(define (check-same-commands set1 set2)
  (cond ((and (pair? set1) (pair? set2))
         (check-same-commands (car set1) (car set2))
         (check-same-commands (cdr set1) (cdr set2)))
        ((and (number? set1) (number? set2))
         (check-= set1 set2 1e-4))
        (#t
         (check-equal? set1 set2))))

(define (same-draw-commands? set1 set2)
  (cond ((and (pair? set1) (pair? set2))
         (and
          (same-draw-commands? (car set1) (car set2))
          (same-draw-commands? (cdr set1) (cdr set2))))
        ((and (number? set1) (number? set2))
         (let ([difference (abs (- set1 set2))])
           (< difference 1e-4)))
        (#t
         (equal? set1 set2))))

(define (check-same-draw-commands set1 set2)
  (check-equal? (length set1) (length set2))
  (for ([command1 (in-list set1)]
        [command2 (in-list set2)])
    (define tag1 (car command1))
    (define tag2 (car command2))
    (printf "checking ~a - ~a~%" tag1 tag2)
    (cond ((and (equal? tag1 'draw-text)
                (equal? tag2 'draw-text))
           ;; Because of different fonts, the coordinates of the draw-text
           ;; command will be different depending on platform.  We let that
           ;; pass, checking the other args only.
           (match-define (list cmd1 text1 x1 y1 combine1? offset1 angle1) command1)
           (match-define (list cmd2 text2 x2 y2 combine2? offset2 angle2) command2)
           (check-equal? text1 text2)
           (check-equal? combine1? combine2?)
           (check-= offset1 offset2 1e-4)
           (check-= angle1 angle2 1e-4))
          ((and (equal? tag1 'draw-polygon)
                (equal? tag2 'draw-polygon))
           ;; later
           (void))
          (#t
           (check-same-commands command1 command2)))))

(provide generate-draw-steps check-same-draw-commands
         same-draw-commands?)
