#lang typed/racket/base

;; Data structure that represents a tick, and functions that produce ticks.

(require racket/string racket/list racket/match racket/math math/flonum
         "type-doc.rkt"
         "types.rkt"
         "math.rkt"
         "format.rkt"
         "utils.rkt"
         "axis-transform.rkt"
         "sample.rkt"
         "date-time.rkt"
         "currency.rkt")

(provide (all-defined-out))

(struct pre-tick ([value : Real] [major? : Boolean]) #:transparent)
(struct tick pre-tick ([label : String]) #:transparent)

(:: pre-tick-inexact->exact (-> pre-tick pre-tick))
(define (pre-tick-inexact->exact t)
  (match-define (pre-tick x major?) t)
  (pre-tick (inexact->exact x) major?))

(:: tick-inexact->exact (-> tick tick))
(define (tick-inexact->exact t)
  (match-define (tick x major? label) t)
  (tick (inexact->exact x) major? label))

(deftype Ticks-Layout (-> Real Real (Listof pre-tick)))
(deftype Ticks-Format (-> Real Real (Listof pre-tick) (Listof String)))

(struct ticks ([layout : Ticks-Layout] [format : Ticks-Format]) #:transparent)

(:: ticks-generate (-> ticks Real Real (Listof tick)))
(define (ticks-generate t x-min x-max)
  (match-define (ticks layout format) t)
  (define ts (map pre-tick-inexact->exact (layout x-min x-max)))
  (match-define (list (pre-tick #{xs : (Listof Real)} #{majors : (Listof Boolean)}) ...) ts)
  (map tick xs majors (format x-min x-max ts)))

(defparam ticks-default-number Positive-Integer 4)

;; ===================================================================================================
;; Helpers

(define-syntax-rule (with-exact-bounds x-min x-max body ...)
  (cond [(x-min . > . x-max)
         (error 'bounds-check "expected min <= max; given min = ~e and max = ~e" x-min x-max)]
        [else  (let ([x-min  (inexact->exact x-min)]
                     [x-max  (inexact->exact x-max)])
                 body ...)]))

(: linear-seq-args (-> Real Real Real (Values Real Real Integer)))
(define (linear-seq-args x-min x-max step)
  (define start (* (ceiling (/ x-min step)) step))
  (define end (* (floor (/ x-max step)) step))
  (cond [(zero? step)
         (raise-argument-error 'linear-seq-args "nonzero Real" 2 x-min x-max step)]
        [else
         (define num (+ 1 (exact-round (/ (- end start) step))))
         (values start end num)]))

(: linear-values/step (-> Real Real Real (Listof Real)))
(define (linear-values/step x-min x-max step)
  (define-values (start end num) (linear-seq-args x-min x-max step))
  (if (negative? num) empty (linear-seq start end num)))

(: tick-values->pre-ticks (-> (Listof Real) (Listof Real) (Listof pre-tick)))
(define (tick-values->pre-ticks major-xs minor-xs)
  (define major-ts (map (λ ([x : Real]) (pre-tick x #t)) major-xs))
  (define minor-ts (map (λ ([x : Real]) (pre-tick x #f)) minor-xs))
  ((inst sort pre-tick Real) (append major-ts minor-ts) < #:key pre-tick-value))

;; ===================================================================================================
;; Linear ticks (default tick function, evenly spaced)

(: linear-tick-step (-> Real Real Positive-Integer Positive-Integer (Listof Positive-Integer)
                        Real))
(define (linear-tick-step x-min x-max num-ticks base divisors)
  (define range (- x-max x-min))
  (cond
    [(<= range 0)
     (raise-argument-error 'linear-tick-step (format "Real > ~a" x-min)
                           1 x-min x-max num-ticks base divisors)]
    [else
     (define mag (expt base (floor-log/base base range)))
     (define epsilon (expt 10 (- (digits-for-range x-min x-max))))
     (define e-start (floor-log/base base num-ticks))
     (define-values (step diff)
       (for*/fold ([step : (U Real #f)  #f] [diff : Real  +inf.0])
                  ([e  (in-range e-start -2 -1)]
                   [d  (in-list (sort divisors <))])
         ;; when num-ticks > base, we sometimes must divide by (expt base e) instead of just base
         (define new-step (/ mag d (expt base e)))
         ;; find the start, end and number of ticks with this step size
         (define-values (new-start new-end new-num) (linear-seq-args x-min x-max new-step))
         ;; endpoints don't count in the number of ticks (a concession for contour-ticks, which
         ;; seems to work well outside of contour plots anyway)
         (let* ([new-num  (if ((abs (- new-start x-min)) . < . epsilon) (- new-num 1) new-num)]
                [new-num  (if ((abs (- new-end x-max)) . < . epsilon) (- new-num 1) new-num)])
           ;; keep the step size that generates the number of ticks closest to num-ticks
           (define new-diff (abs (- new-num num-ticks)))
           (cond [(new-diff . <= . diff)  (values new-step new-diff)]
                 [else  (values step diff)]))))
     (if step step (/ range num-ticks))]))

(: linear-tick-values (-> Real Real Positive-Integer Positive-Integer (Listof Positive-Integer)
                          (Values (Listof Real) (Listof Real))))
(define (linear-tick-values x-min x-max num-ticks base divisors)
  (with-exact-bounds x-min x-max
    (cond
      [(= x-min x-max)  (values empty empty)]
      [else
       (define major-step (linear-tick-step x-min x-max num-ticks base divisors))
       (define major-xs (linear-values/step x-min x-max major-step))
       (define num-major-ticks (length major-xs))
       
       (: minor-xs (Listof Real))
       (define minor-xs
         (let loop ([mult : Positive-Integer  2])
           (cond
             [(mult . > . 4)  empty]
             [else
              (define minor-step (linear-tick-step x-min x-max (* mult num-ticks) base divisors))
              (define minor-xs (linear-values/step x-min x-max minor-step))
              (cond [(empty? (remove* minor-xs major-xs))
                     ;; this covers the major ticks as well; check for additional minor ticks
                     (define real-minor-xs (remove* major-xs minor-xs))
                     (cond [(empty? real-minor-xs)  (loop (+ 1 mult))]
                           [else  real-minor-xs])]
                    [else  (loop (+ 1 mult))])])))
       
       (values major-xs minor-xs)])))

(:: linear-ticks-layout (->* []
                             [#:number Positive-Integer
                              #:base Positive-Integer
                              #:divisors (Listof Positive-Integer)]
                             Ticks-Layout))
(define (linear-ticks-layout #:number [number (ticks-default-number)]
                             #:base [base 10]
                             #:divisors [divisors '(1 2 4 5)])
  (cond
    [(< base 2)  (error 'linear-ticks-layout "expected base >= 2; given ~e" base)]
    [else
     (λ (x-min x-max)
       (define-values (major-xs minor-xs) (linear-tick-values x-min x-max number base divisors))
       (tick-values->pre-ticks major-xs minor-xs))]))

(:: linear-ticks-format (-> Ticks-Format))
(define (linear-ticks-format)
  (λ (x-min x-max ts)
    (with-exact-bounds x-min x-max
      (define digits (digits-for-range x-min x-max))
      (for/list ([t  (in-list ts)])
        (real->plot-label (pre-tick-value t) digits)))))

(:: linear-ticks (->* []
                      [#:number Positive-Integer
                       #:base Positive-Integer
                       #:divisors (Listof Positive-Integer)]
                      ticks))
(define (linear-ticks #:number [number (ticks-default-number)]
                      #:base [base 10]
                      #:divisors [divisors '(1 2 4 5)])
  (ticks (linear-ticks-layout #:number number
                              #:base base
                              #:divisors divisors)
         (linear-ticks-format)))

;; ===================================================================================================
;; No ticks

(defthing no-ticks-layout Ticks-Layout
  (λ (x-min x-max) empty))

(defthing no-ticks-format Ticks-Format
  (λ (x-min x-max pre-ticks)
    (map (λ (_) "") pre-ticks)))

(defthing no-ticks ticks
  (ticks no-ticks-layout no-ticks-format))

;; ===================================================================================================
;; Exponential ticks (for log scale)

(:: log-ticks-layout (->* [] [#:number Positive-Integer #:base Positive-Integer] Ticks-Layout))
(define ((log-ticks-layout #:number [number (ticks-default-number)] #:base [base 10]) x-min x-max)
  (cond
    [(< base 2)  (error 'log-ticks-layout "expected base >= 2; given ~e" base)]
    [else
     (with-exact-bounds x-min x-max
       (cond
         [(or (x-min . <= . 0) ((fl x-min) . <= . 0))
          (raise-argument-error 'log-ticks-layout "positive Real" 0 x-min x-max)]
         [(x-max . < . x-min)
          (raise-argument-error 'log-ticks-layout (format "Real > ~a" x-min) 1 x-min x-max)]
         [else
          (define log-start (ceiling-log/base base x-min))
          (define log-end (floor-log/base base x-max))
          (define skip (max 1 (ceiling (/ (+ 1 (- log-end log-start)) number))))
          (filter
           (λ ([t : pre-tick]) (<= x-min (pre-tick-value t) x-max))
           (append*
            (for/list : (Listof (Listof pre-tick)) ([log-x  (in-range (- log-start 1) (+ log-end 2))]
                                                    [m      (in-cycle (in-range skip))])
              (define x (expt base log-x))
              (cond [(= skip 1)  (for/list : (Listof pre-tick) ([i  (in-range 0 (sub1 base))])
                                   (pre-tick (+ x (* i x))
                                             (and (zero? i) (zero? m))))]
                    [else  (list (pre-tick x (zero? m)))]))))]))]))

(:: log-ticks-format (->* [] [#:base Positive-Integer] Ticks-Format))
(define (log-ticks-format #:base [base 10])
  (cond
    [(< base 2)  (error 'log-ticks-format "expected base >= 2; given ~e" base)]
    [else
     (define base-str (number->string base))
     (λ (x-min x-max ts)
       (with-exact-bounds x-min x-max
         (define epsilon (expt 10 (- (digits-for-range x-min x-max))))
         (define base-digits (digits-for-range 0 base))
         (for/list ([t  (in-list ts)])
           (define x (pre-tick-value t))
           (cond [(<= x 0)  (raise-argument-error 'log-ticks-format
                                                  "(Listof pre-tick) with positive positions"
                                                  2 x-min x-max ts)]
                 [else
                  (define log-x (floor-log/base base x))
                  (define (major-str)
                    (if (zero? log-x) "1" (format "~a~a" base-str (integer->superscript log-x))))
                  (cond [((abs (- x (expt base log-x))) . < . epsilon)  (major-str)]
                        [(zero? log-x)  (real->plot-label x base-digits)]
                        [else  (format "~a×~a"
                                       (real->plot-label (/ x (expt base log-x)) base-digits)
                                       (major-str))])]))))]))

(:: log-ticks (->* [] [#:number Positive-Integer #:base Positive-Integer] ticks))
(define (log-ticks #:number [number (ticks-default-number)] #:base [base 10])
  (cond
    [(< base 2)  (error 'log-ticks "expected base >= 2; given ~e" base)]
    [else  (ticks (log-ticks-layout #:number number #:base base)
                  (log-ticks-format #:base base))]))

;; ===================================================================================================
;; Date/time helpers

(: find-linear-tick-step (-> Real Real Positive-Integer (Listof Real) Real))
(define (find-linear-tick-step x-min x-max num-ticks steps)
  (with-exact-bounds x-min x-max
    (define epsilon (expt 10 (- (digits-for-range x-min x-max))))
    (define-values (step diff)
      (for/fold ([step : (U Real #f)  #f] [diff : Real  +inf.0])
                ([new-step  (in-list (sort steps <))])
        (define-values (new-start new-end new-num) (linear-seq-args x-min x-max new-step))
        ;; endpoints don't count in number of ticks (see linear-tick-step)
        (let* ([new-num  (if ((abs (- new-start x-min)) . < . epsilon) (- new-num 1) new-num)]
               [new-num  (if ((abs (- new-end x-max)) . < . epsilon) (- new-num 1) new-num)])
          (define new-diff (abs (- new-num num-ticks)))
          (cond [(new-diff . <= . diff)  (values new-step new-diff)]
                [else  (values step diff)]))))
    (assert step values)))

(: count-changing-fields (-> (-> Symbol Real (U String #f)) (Listof (U String Symbol)) (Listof Real)
                             Index))
(define (count-changing-fields formatter fmt-list xs)
  (let ([fmt-list  (filter symbol? fmt-list)])
    (define formatted-dates (for/list : (Listof (Listof String)) ([x  (in-list xs)])
                              (apply-formatter formatter fmt-list x)))
    (count (λ ([fields : (Listof (U String #f))]) (not (apply equal?* fields)))
           (transpose formatted-dates))))

(: choose-format-list (-> (-> Symbol Real (U String #f))
                          (Listof (Listof (U String Symbol)))
                          (Listof Real)
                          (Listof (U String Symbol))))
;; Find the shortest format string that has the maximum number of changing fields
(define (choose-format-list formatter fmt-lists xs)
  (let ([fmt-lists  ((inst sort (Listof (U String Symbol)) Index)
                     fmt-lists <
                     #:key (λ ([fmt-list : (Listof (U String Symbol))]) (count symbol? fmt-list))
                     #:cache-keys? #t)])
    (argmax (λ ([fmt-list : (Listof (U String Symbol))])
              (count-changing-fields formatter fmt-list xs))
            fmt-lists)))

;; ===================================================================================================
;; Date ticks

(defthing 24h-descending-date-ticks-formats (Listof String) #:document-value
  '("~Y-~m-~d ~H:~M:~f"
    "~Y-~m-~d ~H:~M"
    "~Y-~m-~d ~Hh"
    "~Y-~m-~d"
    "~Y-~m"
    "~Y"
    "~m-~d ~H:~M:~f"
    "~m-~d ~H:~M"
    "~m-~d ~Hh"
    "~m-~d"
    "~H:~M:~f"
    "~H:~M"
    "~Hh"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defthing 12h-descending-date-ticks-formats (Listof String) #:document-value
  '("~Y-~m-~d ~I:~M:~f ~p"
    "~Y-~m-~d ~I:~M ~p"
    "~Y-~m-~d ~I ~p"
    "~Y-~m-~d"
    "~Y-~m"
    "~Y"
    "~m-~d ~I:~M:~f ~p"
    "~m-~d ~I:~M ~p"
    "~m-~d ~I ~p"
    "~m-~d"
    "~I:~M:~f ~p"
    "~I:~M ~p"
    "~I ~p"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defparam date-ticks-formats (Listof String) 24h-descending-date-ticks-formats)

(: date-steps (Listof Positive-Exact-Rational))
;; Tick steps to try, in seconds
(define date-steps
  (list 1 2 4 5 10 15 20 30 40 45
        seconds-per-minute
        (* 2 seconds-per-minute)
        (* 4 seconds-per-minute)
        (* 5 seconds-per-minute)
        (* 10 seconds-per-minute)
        (* 15 seconds-per-minute)
        (* 20 seconds-per-minute)
        (* 30 seconds-per-minute)
        (* 45 seconds-per-minute)
        seconds-per-hour
        (* 2 seconds-per-hour)
        (* 3 seconds-per-hour)
        (* 4 seconds-per-hour)
        (* 6 seconds-per-hour)
        (* 8 seconds-per-hour)
        (* 12 seconds-per-hour)
        (* 18 seconds-per-hour)
        seconds-per-day
        (* 2 seconds-per-day)
        (* 4 seconds-per-day)
        (* 5 seconds-per-day)
        (* 10 seconds-per-day)
        seconds-per-week
        (* 2 seconds-per-week)
        avg-seconds-per-month
        (* 2 avg-seconds-per-month)
        (* 3 avg-seconds-per-month)
        (* 4 avg-seconds-per-month)
        (* 6 avg-seconds-per-month)
        (* 8 avg-seconds-per-month)
        (* 9 avg-seconds-per-month)
        avg-seconds-per-year
        (* 2 avg-seconds-per-year)
        (* 4 avg-seconds-per-year)
        (* 5 avg-seconds-per-year)))

(: date-tick-values (-> Real Real Positive-Integer (Listof Real)))
(define (date-tick-values x-min x-max num-ticks)
  (with-exact-bounds x-min x-max
    (define range (- x-max x-min))
    (cond [(<= range 0)  empty]
          [else
           (define step
             (cond [(range . < . (* num-ticks (first date-steps)))
                    (linear-tick-step x-min x-max num-ticks 10 '(1 2 4 5))]
                   [(range . > . (* num-ticks (last date-steps)))
                    (* avg-seconds-per-year
                       (linear-tick-step (/ x-min avg-seconds-per-year) (/ x-max avg-seconds-per-year)
                                         num-ticks 10 '(1 2 4 5)))]
                   [else  (find-linear-tick-step x-min x-max num-ticks date-steps)]))
           (define date-round
             (cond [(step . >= . avg-seconds-per-year)   utc-seconds-round-year]
                   [(step . >= . avg-seconds-per-month)  utc-seconds-round-month]
                   [else  (λ ([d : Real]) d)]))
           (map date-round
                (linear-values/step x-min x-max step))])))

(:: date-ticks-layout (->* [] [#:number Positive-Integer] Ticks-Layout))
(define (date-ticks-layout #:number [number (ticks-default-number)])
  (λ (x-min x-max)
    (define major-xs (date-tick-values x-min x-max number))
    (tick-values->pre-ticks major-xs empty)))

(:: date-ticks-format (->* [] [#:formats (Listof String)] Ticks-Format))
(define (date-ticks-format #:formats [formats (date-ticks-formats)])
  (define fmt-lists (map parse-format-string formats))
  (λ (x-min x-max ts)
    (with-exact-bounds x-min x-max
      (define formatter (plot-date-formatter x-min x-max))
      (define xs (map pre-tick-value ts))
      (cond [(empty? xs)  empty]
            [else
             (define fmt-list (choose-format-list formatter fmt-lists xs))
             (cons (string-append* (apply-formatter formatter fmt-list (first xs)))
                   (for/list : (Listof String) ([last-x  (in-list xs)] [x  (in-list (rest xs))])
                     (define fmt-list (choose-format-list formatter fmt-lists (list last-x x)))
                     (string-append* (apply-formatter formatter fmt-list x))))]))))

(:: date-ticks (->* [] [#:number Positive-Integer #:formats (Listof String)] ticks))
(define (date-ticks #:number [number (ticks-default-number)]
                    #:formats [formats (date-ticks-formats)])
  (ticks (date-ticks-layout #:number number)
         (date-ticks-format #:formats formats)))

;; ===================================================================================================
;; Time ticks

(defthing 24h-descending-time-ticks-formats (Listof String) #:document-value
  '("~dd ~H:~M:~f"
    "~dd ~H:~M"
    "~dd ~Hh"
    "~dd"
    "~H:~M:~f"
    "~H:~M"
    "~Hh"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defthing 12h-descending-time-ticks-formats (Listof String) #:document-value
  '("~dd ~I:~M:~f ~p"
    "~dd ~I:~M ~p"
    "~dd ~I ~p"
    "~dd"
    "~I:~M:~f ~p"
    "~I:~M ~p"
    "~I ~p"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defparam time-ticks-formats (Listof String) 24h-descending-time-ticks-formats)

(: time-steps (Listof Positive-Exact-Rational))
;; Tick steps to try, in seconds
(define time-steps
  (list 1 2 4 5 10 15 20 30 40 45
        seconds-per-minute
        (* 2 seconds-per-minute)
        (* 4 seconds-per-minute)
        (* 5 seconds-per-minute)
        (* 10 seconds-per-minute)
        (* 15 seconds-per-minute)
        (* 20 seconds-per-minute)
        (* 30 seconds-per-minute)
        (* 45 seconds-per-minute)
        seconds-per-hour
        (* 2 seconds-per-hour)
        (* 3 seconds-per-hour)
        (* 4 seconds-per-hour)
        (* 6 seconds-per-hour)
        (* 8 seconds-per-hour)
        (* 12 seconds-per-hour)
        (* 18 seconds-per-hour)
        seconds-per-day
        (* 2 seconds-per-day)
        (* 4 seconds-per-day)
        (* 5 seconds-per-day)
        (* 10 seconds-per-day)
        (* 15 seconds-per-day)
        (* 30 seconds-per-day)
        (* 60 seconds-per-day)
        (* 90 seconds-per-day)))

(: time-tick-values (-> Real Real Positive-Integer (Listof Real)))
(define (time-tick-values x-min x-max num-ticks)
  (with-exact-bounds x-min x-max
    (define range (- x-max x-min))
    (cond [(<= range 0)  empty]
          [else
           (define step
             (cond [(range . < . (* num-ticks (first time-steps)))
                    (linear-tick-step x-min x-max num-ticks 10 '(1 2 4 5))]
                   [(range . > . (* num-ticks (last time-steps)))
                    (* seconds-per-day
                       (linear-tick-step (/ x-min seconds-per-day) (/ x-max seconds-per-day)
                                         num-ticks 10 '(1 2 4 5)))]
                   [else
                    (find-linear-tick-step x-min x-max num-ticks time-steps)]))
           (linear-values/step x-min x-max step)])))

(:: time-ticks-layout (->* [] [#:number Positive-Integer] Ticks-Layout))
(define (time-ticks-layout #:number [number (ticks-default-number)])
  (λ (x-min x-max)
    (define major-xs (time-tick-values x-min x-max number))
    (tick-values->pre-ticks major-xs empty)))

(:: time-ticks-format (->* [] [#:formats (Listof String)] Ticks-Format))
(define (time-ticks-format #:formats [formats (time-ticks-formats)])
  (define fmt-lists (map parse-format-string formats))
  (λ (x-min x-max ts)
    (with-exact-bounds x-min x-max
      (define formatter (plot-time-formatter x-min x-max))
      (define xs (map pre-tick-value ts))
      (cond [(empty? xs)  empty]
            [else
             (define fmt-list (choose-format-list formatter fmt-lists xs))
             (cons (string-append* (apply-formatter formatter fmt-list (first xs)))
                   (for/list : (Listof String) ([last-x  (in-list xs)] [x  (in-list (rest xs))])
                     (define fmt-list (choose-format-list formatter fmt-lists (list last-x x)))
                     (string-append* (apply-formatter formatter fmt-list x))))]))))

(: time-ticks (->* [] [#:number Positive-Integer #:formats (Listof String)] ticks))
(define (time-ticks #:number [number (ticks-default-number)]
                    #:formats [formats (time-ticks-formats)])
  (ticks (time-ticks-layout #:number number)
         (time-ticks-format #:formats formats)))

;; ===================================================================================================
;; Byte and bit ticks

(: byte-suffixes (Vectorof String))
(: bit-suffixes (Vectorof String))
;; "", Kilo, Mega, Giga, Tera, Peta, Exa, Zeta, Yotta
(define byte-suffixes #("B" "KB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB"))
(define bit-suffixes #("b" "Kb" "Mb" "Gb" "Tb" "Pb" "Eb" "Zb" "Yb"))

(:: bit/byte-ticks-format (->* [] [#:size (U 'byte 'bit) #:kind (U 'CS 'SI)] Ticks-Format))
(define (bit/byte-ticks-format #:size [size 'byte] #:kind [kind 'CS])
  (λ (x-min x-max ts)
    (with-exact-bounds x-min x-max
      (define suffixes (if (eq? size 'bit) bit-suffixes byte-suffixes))
      (define-values (base pow) (case kind
                                  [(SI)  (values 10 3)]
                                  [else  (values 2 10)]))
      (define x-largest (max* (abs x-min) (abs x-max)))
      (define b
        (if (positive? x-largest) (floor-log/base (expt base pow) x-largest) 0))
      (define format-str
        (cond [(and (b . >= . 0) (b . < . (vector-length suffixes)))
               (format "~a ~a" "~a" (vector-ref suffixes b))]
              [else
               (format "~a×~a~a ~a" "~a"
                       base (integer->superscript (* b pow)) (vector-ref suffixes 0))]))
      (define unit (expt base (* b pow)))
      (define digits (digits-for-range (/ x-min unit) (/ x-max unit)))
      (for/list ([t  (in-list ts)])
        (define unit-x (/ (pre-tick-value t) unit))
        (format format-str (real->plot-label unit-x digits #f))))))

(:: bit/byte-ticks (->* [] [#:number Positive-Integer #:size (U 'byte 'bit) #:kind (U 'CS 'SI)]
                        ticks))
(define (bit/byte-ticks #:number [number (ticks-default-number)]
                        #:size [size 'byte]
                        #:kind [kind 'CS])
  (define si? (eq? kind 'SI))
  (ticks (linear-ticks-layout #:number number #:base (if si? 10 2)
                              #:divisors (if si? '(1 2 4 5) '(1 2)))
         (bit/byte-ticks-format #:size size #:kind kind)))

;; ===================================================================================================
;; Currency

;; US "short scale" suffixes
(defthing us-currency-scales (Listof String) #:document-value '("" "K" "M" "B" "T"))
;; The UK officially uses the short scale since 1974
;; Million is abbreviated "m" instead of "mn" because "mn" stands for minutes
(defthing uk-currency-scales (Listof String) #:document-value '("" "k" "m" "bn" "tr"))
;; European countries use the long scale: million, milliard, billion
(defthing eu-currency-scales (Listof String) #:document-value '("" "K" "M" "Md" "B"))
;; The larger the scale suffixes get, the less standardized they are; so we stop at billion (long)

;; US negative amounts are in parenthesis:
(defthing us-currency-formats (List String String String) #:document-value
  '("~$~w.~f~s" "(~$~w.~f~s)" "~$0"))
;; The UK is more reasonable, using a negative sign for negative amounts:
(defthing uk-currency-formats (List String String String) #:document-value
  '("~$~w.~f~s" "-~$~w.~f~s" "~$0"))
;; The more common EU format (e.g. France, Germany, Italy, Spain):
(defthing eu-currency-formats (List String String String) #:document-value
  '("~w,~f ~s~$" "-~w,~f ~s~$" "0 ~$"))

(defparam currency-ticks-scales (Listof String) us-currency-scales)
(defparam currency-ticks-formats (List String String String) us-currency-formats)

(struct amount-data ([sign : String]
                     [amount : Real]
                     [unit : Positive-Exact-Rational]
                     [suffix : String])
  #:transparent)

(: currency-formatter (-> Real Real (-> Symbol amount-data (U String #f))))
(define (currency-formatter x-min x-max)
  (λ (fmt data)
    (case fmt
      [(~$)  (amount-data-sign data)]
      [(~s)  (amount-data-suffix data)]
      [(~w ~f)
       (match-define (amount-data _sign amt unit _suffix) data)
       (define digits (digits-for-range (/ x-min unit) (/ x-max unit)))
       (define n (max 2 digits))
       (define 10^n (expt 10 n))
       (define x (/ (round (* (inexact->exact amt) 10^n)) 10^n))
       (define whole (floor x))
       (case fmt
         [(~w)  (number->string whole)]
         [(~f)  (define frac (- x whole))
                (cond [(= 1 unit)    (substring (real->decimal-string* frac 2 n) 2)]
                      [(zero? frac)  "0"]
                      [else          (substring (real->decimal-string* frac 1 n) 2)])])]
      [else  #f])))

(:: currency-ticks-format (->* [] [#:kind (U String Symbol)
                                   #:scales (Listof String)
                                   #:formats (List String String String)]
                               Ticks-Format))
(define (currency-ticks-format #:kind [kind 'USD]
                               #:scales [scales (currency-ticks-scales)]
                               #:formats [formats (currency-ticks-formats)])
  (match-define (list positive-format-string negative-format-string zero-format-string) formats)
  (define positive-format-list (parse-format-string positive-format-string))
  (define negative-format-list (parse-format-string negative-format-string))
  (define zero-format-list (parse-format-string zero-format-string))
  (define suffixes (list->vector scales))
  (define n (vector-length suffixes))
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define formatter (currency-formatter x-min x-max))
     (define sign (cond [(string? kind)  kind]
                        [else  (hash-ref currency-code->sign kind (λ () (symbol->string kind)))]))
     (define x-largest (max* (abs x-min) (abs x-max)))
     (define b
       (if (positive? x-largest)
           (let ([b  (floor-log/base 1000 x-largest)])
             (if (b . < . 0) (+ b 1) b))
           0))
     (define suffix
       (cond [(and (b . >= . 0) (b . < . n))  (vector-ref suffixes b)]
             [else  (format "×10~a" (integer->superscript (* b 3)))]))
     (define unit
       (cond [(= 0 (string-length suffix))  1]
             [else  (expt 1000 b)]))
     (for/list ([t  (in-list ts)])
       (define x (pre-tick-value t))
       (define format-list (cond [(positive? x)  positive-format-list]
                                 [(negative? x)  negative-format-list]
                                 [else           zero-format-list]))
       (define unit-x (/ (abs x) unit))
       (string-append*
        (apply-formatter formatter format-list
                         (amount-data sign unit-x unit suffix)))))))

(:: currency-ticks (->* [] [#:number Positive-Integer
                            #:kind (U String Symbol)
                            #:scales (Listof String)
                            #:formats (List String String String)]
                        ticks))
(define (currency-ticks #:number [number (ticks-default-number)]
                        #:kind [kind 'USD]
                        #:scales [scales (currency-ticks-scales)]
                        #:formats [formats (currency-ticks-formats)])
  (ticks (linear-ticks-layout #:number number)
         (currency-ticks-format #:kind kind #:scales scales
                                #:formats formats)))

;; ===================================================================================================
;; Fractions

(: format-fraction (-> Exact-Rational String))
(define (format-fraction x)
  (cond [(x . < . 0)  (format "-~a" (format-fraction (- x)))]
        [(x . = . 0)  "0"]
        [(x . < . 1)  (format "~a/~a" (numerator x) (denominator x))]
        [else
         (define d (denominator x))
         (cond [(d . = . 1)  (format "~a" (numerator x))]
               [else
                (define w (floor x))
                (let ([x  (- x w)])
                  (format "~a ~a/~a" w (numerator x) (denominator x)))])]))

(:: fraction-ticks-format (->* [] [#:base Positive-Integer #:divisors (Listof Positive-Integer)]
                               Ticks-Format))
(define (fraction-ticks-format #:base [base 10] #:divisors [divisors '(1 2 3 4 5)])
  (cond
    [(< base 2)  (error 'fraction-ticks-format "expected base >= 2; given ~e" base)]
    [else
     (define fracs (remove-duplicates (map (λ ([d : Positive-Integer]) (/ d base)) divisors)))
     (λ (x-min x-max ts)
       (define digits (digits-for-range x-min x-max base (ceiling-log/base base 1000)))
       (define fracs (remove-duplicates (map (λ ([d : Positive-Integer])
                                               (* (/ base d) (expt base (- digits))))
                                             divisors)))
       (for/list ([t  (in-list ts)])
         (define x (inexact->exact (pre-tick-value t)))
         (define xs
           (for/list : (Listof Exact-Rational) ([frac  (in-list fracs)])
             (* frac (round (/ x frac)))))
         (format-fraction (argmin (λ ([y : Exact-Rational]) (abs (- x y))) xs))))]))

(:: fraction-ticks (->* [] [#:base Positive-Integer #:divisors (Listof Positive-Integer)] ticks))
(define (fraction-ticks #:base [base 10] #:divisors [divisors '(1 2 3 4 5)])
  (cond
    [(< base 2)  (error 'fraction-ticks "expected base >= 2; given ~e" base)]
    [else  (ticks (linear-ticks-layout #:base base #:divisors divisors)
                  (fraction-ticks-format #:base base #:divisors divisors))]))

;; ===================================================================================================
;; Tick combinators

(:: ticks-mimic (-> (-> ticks) ticks))
(define (ticks-mimic thunk) ticks?
  (ticks (λ (x-min x-max) ((ticks-layout (thunk)) x-min x-max))
         (λ (x-min x-max ts) ((ticks-format (thunk)) x-min x-max ts))))

(:: ticks-scale (-> ticks invertible-function ticks))
(define (ticks-scale t fun)
  (match-define (invertible-function f g) fun)
  (match-define (ticks layout format) t)
  (ticks (λ (x-min x-max)
           (define ts (layout (f x-min) (f x-max)))
           (for/list ([t  (in-list ts)])
             (match-define (pre-tick x major?) t)
             (pre-tick (g x) major?)))
         (λ (x-min x-max ts)
           (format (f x-min) (f x-max) (map (λ (t)
                                              (match-define (pre-tick x major?) t)
                                              (pre-tick (f x) major?))
                                            ts)))))

(:: ticks-add (->* [ticks (Listof Real)] [Boolean] ticks))
(define (ticks-add t xs [major? #t])
  (match-define (ticks layout format) t)
  (ticks (λ (x-min x-max)
           (append (layout x-min x-max)
                   (for/list : (Listof pre-tick) ([x  (in-list xs)])
                     (pre-tick x major?))))
         format))

(:: linear-scale (->* [Real] [Real] invertible-function))
(define (linear-scale m [b 0])
  (cond [(not (rational? m))  (raise-argument-error 'linear-scale "rational?" 0 m b)]
        [(not (rational? b))  (raise-argument-error 'linear-scale "rational?" 1 m b)]
        [else  (invertible-function (λ (x) (+ (* m x) b))
                                    (λ (y) (/ (- y b) m)))]))

;; ===================================================================================================
;; Tick utils

(: same-label? (-> tick tick Boolean))
(define (same-label? t1 t2) (string=? (tick-label t1) (tick-label t2)))

(: collapse-equiv-ticks (-> (Listof tick) String tick))
(define (collapse-equiv-ticks ts near-format-string)
  (match-define (list (tick #{xs : (Listof Real)}
                            #{majors : (Listof Boolean)}
                            #{labels : (Listof String)})
                      ...)
    ts)
  (define x (/ (apply + xs) (length ts)))
  (define major? (ormap (λ ([b : Boolean]) b) majors))
  (define label1 (first labels))
  (define label2 (last labels))
  (define label
    (cond [(string=? label1 label2)  label1]
          [else  (format near-format-string label1 label2)]))
  (tick x major? label))

(:: collapse-ticks (->* [(Listof tick) (-> tick tick Boolean)] [String] (Listof tick)))
(define (collapse-ticks ts near? [near-format-string "~a|~a"])
  (let ([ts  ((inst sort tick Real) ts < #:key pre-tick-value)])
    (define tss
      (group-neighbors ts (λ ([t1 : tick] [t2 : tick]) (or (same-label? t1 t2) (near? t1 t2)))))
    (append*
     (for/list : (Listof (Listof tick)) ([ts  (in-list tss)])
       (define n (length ts))
       (define m (count pre-tick-major? ts))
       (cond [(n . <= . 1)  ts]
             [(m . = . 0)  (list (collapse-equiv-ticks ts near-format-string))]
             [(m . = . 1)  (filter pre-tick-major? ts)]
             [else  (list (collapse-equiv-ticks (filter pre-tick-major? ts) near-format-string))])))))

(:: contour-ticks (-> ticks Real Real Contour-Levels Boolean (Listof tick)))
(define (contour-ticks z-ticks z-min z-max levels intervals?)
  (define epsilon (expt 10 (- (digits-for-range z-min z-max))))
  (match-define (ticks layout format) z-ticks)
  ;; initial tick layout
  (define ts
    (cond [(eq? levels 'auto)  (filter pre-tick-major? (layout z-min z-max))]
          [else  (define zs
                   (cond [(list? levels)  (filter (λ ([z : Real]) (<= z-min z z-max)) levels)]
                         [else  (linear-seq z-min z-max levels #:start? #f #:end? #f)]))
                 (map (λ ([z : Real]) (pre-tick z #t)) zs)]))
  (let* (;; remove z-min tick (or the one close to it) if present
         [ts  (if (and (not (empty? ts))
                       ((abs (- z-min (pre-tick-value (first ts)))) . < . epsilon))
                  (rest ts)
                  ts)]
         ;; remove z-max tick (or the one close to it) if present
         [ts  (if (and (not (empty? ts))
                       ((abs (- z-max (pre-tick-value (last ts)))) . < . epsilon))
                  (drop-right ts 1)
                  ts)]
         ;; add z-min and z-max if doing intervals
         [ts  (cond [(not intervals?)  ts]
                    [else  (append (list (pre-tick z-min #t)) ts (list (pre-tick z-max #t)))])])
    ;; format the ticks
    (match-define (list (pre-tick #{zs : (Listof Real)} #{majors : (Listof Boolean)}) ...) ts)
    (define labels (format z-min z-max ts))
    (map tick zs majors labels)))

(:: format-tick-labels (-> ticks Real Real (Listof Real) (Listof String)))
(define (format-tick-labels x-ticks x-min x-max xs)
  (match-define (ticks layout format) x-ticks)
  (let* ([tick-xs  (map pre-tick-value (filter pre-tick-major? (layout x-min x-max)))]
         [tick-xs  (remove* xs tick-xs)]
         [tick-xs  (if (empty? tick-xs) empty (list (apply min tick-xs) (apply max tick-xs)))]
         [tick-xs  (sort (append xs tick-xs) <)])
    (define ts (map (λ ([x : Real]) (pre-tick x #t)) tick-xs))
    (for/list ([x  (in-list tick-xs)]
               [l  (in-list (format x-min x-max ts))]
               #:when (member x xs))
      l)))
