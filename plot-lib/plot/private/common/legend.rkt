#lang typed/racket/base

;; Functions that create legend entries and lists of legend entries.

(require racket/class racket/match racket/list racket/string
         "type-doc.rkt"
         "math.rkt"
         "format.rkt"
         "utils.rkt"
         "types.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Line legends

(:: line-legend-entry (-> String Plot-Color Nonnegative-Real Plot-Pen-Style legend-entry))
(define (line-legend-entry label color width style)
  (legend-entry label (λ (pd x-size y-size)
                        (define y (* 1/2 y-size))
                        (send pd set-pen color width style)
                        (send pd set-alpha 1)
                        (send pd draw-line (vector (ann 0 Real) y) (vector x-size y)))))

(:: line-legend-entries (-> String (Listof Real) (Listof String)
                            (Plot-Colors (Listof Real))
                            (Pen-Widths (Listof Real))
                            (Plot-Pen-Styles (Listof Real))
                            (Listof legend-entry)))
(define (line-legend-entries label zs z-labels colors widths styles)
  (define hash
    (for/fold ([hash : (Listof (Pair (List Plot-Color Nonnegative-Real Plot-Pen-Style)
                                     (Pair String (Listof String))))
                     empty])
              ([z        (in-list zs)]
               [z-label  (in-list z-labels)]
               [color    (in-cycle* (in-list (generate-list colors zs)))]
               ;; The following annotation shouldn't be necessary
               [width : Nonnegative-Real  (in-cycle* (in-list (generate-list widths zs)))]
               [style    (in-cycle* (in-list (generate-list styles zs)))])
      (assoc-cons hash (list color width style) z-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons args vs) entry)
     (apply line-legend-entry
            (cond [(= 1 (length vs))  (format "~a = ~a" label (first vs))]
                  [else  (format "~a ∈ {~a}" label (string-join (reverse vs) ","))])
            args))))

;; ===================================================================================================
;; Rectangle legends

(:: rectangle-legend-entry (-> String
                               Plot-Color Plot-Brush-Style
                               Plot-Color Nonnegative-Real Plot-Pen-Style
                               legend-entry))
(define (rectangle-legend-entry label color style line-color line-width line-style)
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-brush color style)
                        (send pd set-pen line-color line-width line-style)
                        (send pd set-alpha 1)
                        (send pd draw-rect (vector (ivl 0 x-size) (ivl 0 y-size))))))

(:: rectangle-legend-entries
    (-> String (Listof Real)
        (Plot-Colors (Listof Real)) (Plot-Brush-Styles (Listof Real))
        (Plot-Colors (Listof Real)) (Pen-Widths (Listof Real)) (Plot-Pen-Styles (Listof Real))
        (Listof legend-entry)))
(define (rectangle-legend-entries label zs colors styles line-colors line-widths line-styles)
  (define z-min (first zs))
  (define z-max (last zs))
  (define digits (digits-for-range z-min z-max))
  (define hash
    (for/fold ([hash : (Listof (Pair (List Plot-Color Plot-Brush-Style
                                           Plot-Color Nonnegative-Real Plot-Pen-Style)
                                     (Pair String (Listof String))))
                     empty])
              ([z           (in-list zs)]
               [color       (in-cycle* (in-list (generate-list colors zs)))]
               [style       (in-cycle* (in-list (generate-list styles zs)))]
               [line-color  (in-cycle* (in-list (generate-list line-colors zs)))]
               ;; The following annotation shouldn't be necessary
               [line-width : Nonnegative-Real  (in-cycle* (in-list (generate-list line-widths zs)))]
               [line-style  (in-cycle* (in-list (generate-list line-styles zs)))])
      (define entry-label (real->plot-label z digits))
      (assoc-cons hash (list color style line-color line-width line-style) entry-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons (list color style line-color line-width line-style) vs) entry)
     (rectangle-legend-entry (if (= 1 (length vs))
                                 (format "~a = ~a" label (first vs))
                                 (format "~a ∈ {~a}" label (string-join (reverse vs) ",")))
                             color style line-color line-width line-style))))

;; ===================================================================================================
;; Interval legends

(:: interval-legend-entry (-> String
                              Plot-Color Plot-Brush-Style
                              Plot-Color Nonnegative-Real Plot-Pen-Style
                              Plot-Color Nonnegative-Real Plot-Pen-Style
                              Plot-Color Nonnegative-Real Plot-Pen-Style
                              legend-entry))
(define (interval-legend-entry label color style
                               line-color line-width line-style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style)
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-alpha 1)
                        ;; rectangle
                        (send pd set-pen line-color line-width line-style)
                        (send pd set-brush color style)
                        (send pd draw-rect (vector (ivl 0 x-size) (ivl 0 y-size)))
                        ;; bottom line
                        (send pd set-pen line1-color line1-width line1-style)
                        (send pd draw-line (vector (ann 0 Real) y-size) (vector x-size y-size))
                        ;; top line
                        (send pd set-pen line2-color line2-width line2-style)
                        (send pd draw-line
                              (vector (ann 0 Real) (ann 0 Real))
                              (vector x-size (ann 0 Real))))))

(:: interval-legend-entries
    (-> String (Listof ivl) (Listof String)
        (Plot-Colors (Listof ivl)) (Plot-Brush-Styles (Listof ivl))
        (Plot-Colors (Listof ivl)) (Pen-Widths (Listof ivl)) (Plot-Pen-Styles (Listof ivl))
        (Plot-Colors (Listof ivl)) (Pen-Widths (Listof ivl)) (Plot-Pen-Styles (Listof ivl))
        (Plot-Colors (Listof ivl)) (Pen-Widths (Listof ivl)) (Plot-Pen-Styles (Listof ivl))
        (Listof legend-entry)))
(define (interval-legend-entries label ivls ivl-labels colors styles
                                 line-colors line-widths line-styles
                                 line1-colors line1-widths line1-styles
                                 line2-colors line2-widths line2-styles)
  (define hash
    (for/fold
     ([hash : (Listof (Pair (List Plot-Color Plot-Brush-Style
                                  Plot-Color Nonnegative-Real Plot-Pen-Style
                                  Plot-Color Nonnegative-Real Plot-Pen-Style
                                  Plot-Color Nonnegative-Real Plot-Pen-Style)
                            (Pair String (Listof String))))
            empty])
     ([ivl-label    (in-list ivl-labels)]
      [color        (in-cycle* (in-list (generate-list colors ivls)))]
      [style        (in-cycle* (in-list (generate-list styles ivls)))]
      [line-color   (in-cycle* (in-list (generate-list line-colors ivls)))]
      ;; The following annotation shouldn't be necessary
      [line-width : Nonnegative-Real  (in-cycle* (in-list (generate-list line-widths ivls)))]
      [line-style   (in-cycle* (in-list (generate-list line-styles ivls)))]
      [line1-color  (in-cycle* (in-list (generate-list line1-colors ivls)))]
      ;; The following annotation shouldn't be necessary
      [line1-width : Nonnegative-Real  (in-cycle* (in-list (generate-list line1-widths ivls)))]
      [line1-style  (in-cycle* (in-list (generate-list line1-styles ivls)))]
      [line2-color  (in-cycle* (in-list (generate-list line2-colors ivls)))]
      ;; The following annotation shouldn't be necessary
      [line2-width : Nonnegative-Real  (in-cycle* (in-list (generate-list line2-widths ivls)))]
      [line2-style  (in-cycle* (in-list (generate-list line2-styles ivls)))])
      (assoc-cons hash
                  (list color style
                        line-color line-width line-style
                        line1-color line1-width line1-style
                        line2-color line2-width line2-style)
                  ivl-label)))
  
  (reverse
   (for/list ([entry  (in-list hash)])
     (match-define (cons (list color style line-color line-width line-style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style)
                         ivl-labels)
       entry)
     (interval-legend-entry (format "~a ∈ ~a" label (string-join (reverse ivl-labels) " ∪ "))
                            color style line-color line-width line-style
                            line1-color line1-width line1-style
                            line2-color line2-width line2-style))))

;; ===================================================================================================
;; Point legends

(:: point-legend-entry (-> String Point-Sym Plot-Color Plot-Color Nonnegative-Real Nonnegative-Real
                           legend-entry))
(define (point-legend-entry label sym color fill-color size line-width)
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-pen color line-width 'solid)
                        (send pd set-brush fill-color 'solid)
                        (send pd set-alpha 1)
                        (send pd draw-glyphs
                              (list (vector (* 1/2 x-size) (* 1/2 y-size))) sym size))))

(:: arrow-legend-entry (-> String Plot-Color Nonnegative-Real Plot-Pen-Style legend-entry))
(define (arrow-legend-entry label color line-width line-style)
  (legend-entry label (λ (pd x-size y-size)
                        (send pd set-pen color line-width line-style)
                        (send pd set-alpha 1)
                        (send pd draw-arrow-glyph
                              (vector (* 1/2 x-size) (* 1/2 y-size)) (* 1/4 x-size) 0))))
