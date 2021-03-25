#lang typed/racket/base

;; Untyped interface / contract
(module untyped racket/base
  (require racket/class
           racket/contract
           racket/match
           racket/draw)

  (provide plot-metrics<%>
           plot-metrics-object/c)

  (define plot-metrics<%> (interface ()
                            get-plot-bounds
                            dc->plot
                            plot->dc
                            plane-vector
                            get-plot-metrics-functions))

  (define plot-metrics-object/c
    (object/c [get-plot-bounds            (->m (-> (vectorof (vector/c real? real?))))]
              [plot->dc                   (->m (vectorof real?) (vectorof real?))]
              [dc->plot                   (->m (vectorof real?) (vectorof real?))]
              [plane-vector               (->m (vectorof real?))]
              [get-plot-metrics-functions (->m (values (-> (vectorof (vector/c real? real?)))
                                                       (-> (vectorof real?) (vectorof real?))
                                                       (-> (vectorof real?) (vectorof real?))
                                                       (-> (vectorof real?))))]))
  )

;; Typed  Types / mixin / structures
(require typed/pict
         typed/racket/class
         racket/match)
  
(provide plot-metrics-mixin Plot-Metrics-Functions Plot-Metrics<%> plot-metrics%
         (struct-out plot-pict) Plot-Pict pict->pp)

(define-type Metrics-Object (Object [get-plot-metrics-functions (-> Plot-Metrics-Functions)]))
(define-type Plot-Metrics-Functions (List (-> (Vectorof (Vectorof Real)))
                                          (-> (Vectorof Real) (Vectorof Real))
                                          (-> (Vectorof Real) (Vectorof Real))
                                          (-> (Vectorof Real))))
(define-type Plot-Metrics<%>
  (Class
   [get-plot-bounds (-> (Vectorof (Vectorof Real)))]
   [plot->dc        (-> (Vectorof Real) (Vectorof Real))]
   [dc->plot        (-> (Vectorof Real) (Vectorof Real))]
   [plane-vector    (-> (Vectorof Real))]
   [get-plot-metrics-functions (-> Plot-Metrics-Functions)]))

(: plot-metrics-mixin (All (A #:row) (-> (Class #:row-var A)
                                         (Class [init [->metrics-object (-> Metrics-Object)]]
                                                #:row-var A
                                                #:implements Plot-Metrics<%>))))
(define (plot-metrics-mixin %)
  (class %
    (init ->metrics-object)
    (define (load) : Void
      (match-define (list new-bounds new-->dc new-->plot new-plane)
        (send (->metrics-object) get-plot-metrics-functions))
      (set! bounds new-bounds)
      (set! ->dc new-->dc)
      (set! ->plot new-->plot)
      (set! plane new-plane)
      (set! getall (λ () (list bounds ->dc ->plot plane)))
      (set! load void))

    (define (bounds) : (Vectorof (Vectorof Real)) (load)(bounds))
    (define (plane)  : (Vectorof Real)            (load)(plane))
    (define (getall) : Plot-Metrics-Functions     (load)(list bounds ->dc ->plot plane))
    (define (->dc   [v : (Vectorof Real)]) : (Vectorof Real) (load)(->dc v))
    (define (->plot [v : (Vectorof Real)]) : (Vectorof Real) (load)(->plot v))
      
    (super-make-object)
    (define/public (get-plot-bounds) (bounds))
    (define/public (dc->plot coords) (->plot coords))
    (define/public (plot->dc coords) (->dc coords))
    (define/public (plane-vector) (plane))
    (define/public (get-plot-metrics-functions) (getall))))

(define plot-metrics% (plot-metrics-mixin object%))


(struct plot-pict pict ([bounds : (Vectorof (Vectorof Real))]
                        [plot->dc : (-> (Vectorof Real) (Vectorof Real))]
                        [dc->plot : (-> (Vectorof Real) (Vectorof Real))]
                        [plane-vector : (Vectorof Real)]))
(define-type Plot-Pict plot-pict)
(define (pict->pp [P : pict]
                  [metrics-object : Metrics-Object]) : plot-pict
  (match-define (list bounds ->dc ->plot plane)
    (send metrics-object get-plot-metrics-functions))
  
  (plot-pict (pict-draw P) (pict-width P) (pict-height P) (pict-ascent P)
             (pict-descent P) (pict-children P) (pict-panbox P) (pict-last P)
             (bounds) ->dc ->plot (plane)))