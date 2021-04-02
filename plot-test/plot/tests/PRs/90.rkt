#lang racket
(require rackunit
         racket/draw pict (only-in racket/gui/base sleep/yield)
         plot (submod plot/private/common/plotmetrics untyped))

; tests for PR#90, https://github.com/racket/plot/pull/90
; "Plotmetrics: access/calculate data about the plot area"


(define pr90-test-suite
  (make-test-suite
   "PR#90: plotmetrics"
   (append
    (let* ([bm (make-object bitmap% 400 400)]
           [dc (make-object bitmap-dc% bm)]
           [pm (plot/dc (polar (λ (x) x)) dc 0 0 400 400)])
      (list (test-suite "PR90: plot-metrics get-plot-bounds"
                        (check-within (send pm get-plot-bounds)
                                      #(#(-3.2883704095701205 6.283185307179586)
                                        #(-4.8144539337330965 1.81970262809755))
                                      1e-15))
            (test-suite "PR90: plot-metrics plot->dc & dc->plot"
                        (check-within (send pm dc->plot (send pm plot->dc #(0 0)))
                                      #(0 0) 1e-15))
            (test-suite "PR90: plot-metrics plane-vector"
                        (check-equal? (send pm plane-vector)
                                      #(0 0 1)))))
    
    (let ([pmbm (plot3d-bitmap (polar3d (λ (ϕ θ) ϕ)))])
      (list (test-suite "PR90: bitmap" (check-true (is-a? pmbm bitmap%)))
            (test-suite "PR90: 3d/bitmap get-plot-bounds"
                        (check-within (send pmbm get-plot-bounds)
                                      #(#(-3.287703432872698 6.282003882664296)
                                        #(-4.812750657123522 1.819088586099858)
                                        #(-6.283185307179587 6.283185307179587))
                                      1e-15))
            (test-suite "PR90: 3d/bitmap plot->dc & dc->plot"
                        (check-within (send pmbm plot->dc (send pmbm dc->plot #(200 200)))
                                      #(200 200) 1e-15))
            (test-suite "PR90: 3d/bitmap plane-vector"
                        (check-equal? (send pmbm plot->dc (send pmbm plane-vector))
                                      (send pmbm plot->dc #(0 0 0))))))
    
    (let ([pp (plot-pict (function (λ (x) x) 1 2))])
      (list (test-suite "PR90: plot-pict" (check-true (and (plot-pict? pp) (pict? pp))))
            (test-suite "PR90: plot-pict get-plot-bounds"
                        (check-equal? (plot-pict-bounds pp)
                                      #(#(1 2) #(1 2))))
            (test-suite "PR90: plot-pict plot->dc & dc->plot"
                        (check-within ((plot-pict-dc->plot pp) ((plot-pict-plot->dc pp) #(0 0)))
                                      #(0 0) 1e-15))
            (test-suite "PR90: plot-pict plane-vector"
                        (check-equal? (plot-pict-plane-vector pp)
                                      #(0 0 1)))))

    (let* ([ps (plot3d-snip (surface3d (λ (x y) (+ x y)) 0 1 1 2))]
           [plotcoords #(1 1 1)]
           [coords (send ps plot->dc plotcoords)])
      (list (test-suite "PR90: 3d/snip" (check-true (is-a? ps plot-metrics<%>)))
            (test-suite "PR90: 3d/snip get-plot-bounds"
                        (check-equal? (send ps get-plot-bounds)
                                      #(#(0 1) #(1 2) #(1 3))))
            (test-suite "PR90: 3d/snip plot->dc & dc->plot"
                        (check-within (send ps plot->dc (send ps dc->plot #(200 200)))
                                      #(200 200) 1e-15))
            (test-suite "PR90: 3d/snip plane-vector"
                        (check-equal? (send ps plot->dc (send ps plane-vector))
                                      (send ps plot->dc #(0 0 0))))
            (test-suite "PR90: 3d/snip before resize"
                        (check-equal? (send ps plot->dc plotcoords) coords))
            (test-suite "PR90: 3d/snip after resize"
                        (send ps resize 800 800)
                        (sleep/yield .1)
                        (check-within (send ps plot->dc (send ps dc->plot #(200 200)))
                                      #(200 200) 5e-14)
                        (check-not-equal? (send ps plot->dc plotcoords) coords)))))
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests pr90-test-suite))
