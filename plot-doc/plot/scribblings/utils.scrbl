#lang scribble/manual

@(require "common.rkt")

@title[#:tag "utils"]{Plot Utilities}

@declare-exporting[plot/utils]
@defmodule*/no-declare[(plot/utils)]

@;====================================================================================================
@section{Formatting}

@defproc[(digits-for-range [x-min real?] [x-max real?]
                           [base (and/c exact-integer? (>=/c 2)) 10]
                           [extra-digits exact-integer? 3])
         exact-integer?]{
Given a range, returns the number of decimal places necessary to distinguish numbers in the range.
This may return negative numbers for large ranges.

@examples[#:eval plot-eval
                 (digits-for-range 0.01 0.02)
                 (digits-for-range 0 100000)]
}

@defproc[(real->plot-label [x real?] [digits exact-integer?] [scientific? boolean? #t]) string?]{
Converts a real number to a plot label.
Used to format axis tick labels, @(racket point-label)s, and numbers in legend entries.

@examples[#:eval plot-eval
                 (let ([d  (digits-for-range 0.01 0.03)])
                   (real->plot-label 0.02555555 d))
                 (real->plot-label 2352343 -2)
                 (real->plot-label 1000000000. 4)
                 (real->plot-label 1000000000.1234 4)]
}

@defproc[(ivl->plot-label [i ivl?] [extra-digits exact-integer? 3]) string?]{
Converts an interval to a plot label.

If @racket[i] = @racket[(ivl x-min x-max)], the number of digits used is @racket[(digits-for-range x-min x-max 10 extra-digits)] when both endpoints are @racket[rational?].
Otherwise, it is unspecified---but will probably remain @racket[15].
@examples[#:eval plot-eval
                 (ivl->plot-label (ivl -10.52312 10.99232))
                 (ivl->plot-label (ivl -inf.0 pi))]
}

@defproc[(->plot-label [a any/c] [digits exact-integer? 7]) string?]{
Converts a Racket value to a label. Used by @(racket discrete-histogram) and @(racket discrete-histogram3d).
}

@defproc[(real->string/trunc [x real?] [e exact-integer?]) string?]{
Like @(racket real->decimal-string), but removes any trailing zeros and any trailing decimal point.
}

@defproc[(real->decimal-string* [x  real?]
                                [min-digits exact-nonnegative-integer?]
                                [max-digits exact-nonnegative-integer? min-digits])
         string?]{
Like @racket[real->decimal-string], but accepts both a maximum and minimum number of digits.
@examples[#:eval plot-eval
                 (real->decimal-string* 1 5 10)
                 (real->decimal-string* 1.123456 5 10)
                 (real->decimal-string* 1.123456789123456 5 10)]
Applying @racket[(real->decimal-string* x min-digits)] yields the same value as @racket[(real->decimal-string x min-digits)].
}

@defproc[(integer->superscript [x exact-integer?]) string?]{
Converts an integer into a string of superscript Unicode characters.
@examples[#:eval plot-eval
                 (integer->superscript -1234567890)]
Systems running some out-of-date versions of Windows XP have difficulty with Unicode superscripts for 4 and up.
Because @racket[integer->superscript] is used by every number formatting function to format exponents, if you have such a system, @(plot-name) will apparently not format all numbers with exponents correctly (until you update it).
}

@;{
format-tick-labels
}
}

@;====================================================================================================
@section{Sampling}

@defproc[(linear-seq [start real?] [end real?] [num exact-nonnegative-integer?]
                     [#:start? start? boolean? #t]
                     [#:end? end? boolean? #t])
         (listof real?)]{
Returns a list of uniformly spaced real numbers between @(racket start) and @(racket end).
If @(racket start?) is @(racket #t), the list includes @(racket start).
If @(racket end?) is @(racket #t), the list includes @(racket end).

This function is used internally to generate sample points.

@examples[#:eval plot-eval
                 (linear-seq 0 1 5)
                 (linear-seq 0 1 5 #:start? #f)
                 (linear-seq 0 1 5 #:end? #f)
                 (linear-seq 0 1 5 #:start? #f #:end? #f)
                 (define xs (linear-seq -1 1 11))
                 (plot (lines (map vector xs (map sqr xs))))]
}

@defproc[(linear-seq* [points (listof real?)] [num exact-nonnegative-integer?]
                      [#:start? start? boolean? #t]
                      [#:end? end? boolean? #t])
         (listof real?)]{
Like @(racket linear-seq), but accepts a list of reals instead of a start and end.
The @(racket #:start?) and @(racket #:end?) keyword arguments work as in @(racket linear-seq).
This function does not guarantee that each inner value will be in the returned list.

@examples[#:eval plot-eval
                 (linear-seq* '(0 1 2) 5)
                 (linear-seq* '(0 1 2) 6)
                 (linear-seq* '(0 1 0) 5)]
}

@defproc[(nonlinear-seq [start real?] [end real?] [num exact-nonnegative-integer?]
                        [transform axis-transform/c]
                        [#:start? start? boolean? #t]
                        [#:end? end? boolean? #t])
         (listof real?)]{
Generates a list of reals that, if transformed using @(racket transform), would be uniformly spaced.
This is used to generate samples for transformed axes.
@examples[#:eval plot-eval
                 (linear-seq 1 10 4)
                 (nonlinear-seq 1 10 4 log-transform)
                 (parameterize ([plot-x-transform  log-transform])
                   (plot (area-histogram sqr (nonlinear-seq 1 10 4 log-transform))))]
}

@;{
build-linear-seq
make-function->sampler
make-2d-function->sampler
make-3d-function->sampler
sample-exact->inexact
2d-sample-exact->inexact
3d-sample-exact->inexact
}

@defproc[(kde [xs (listof real?)]
              [h (>/c 0)]
              [ws (or/c (listof (>=/c 0)) #f) #f])
         (values (-> real? real?)
                 (or/c rational? #f)
                 (or/c rational? #f))]{
Given optionally weighted samples and a kernel bandwidth, returns a function representing a kernel density estimate, and bounds, outside of which the density estimate is zero.
Used by @(racket density).
}

@;====================================================================================================
@section{Plot Colors and Styles}

@defproc[(color-seq [c1 color/c] [c2 color/c] [num exact-nonnegative-integer?]
                    [#:start? start? boolean? #t]
                    [#:end? end? boolean? #t])
         (listof (list/c real? real? real?))]{
Interpolates between colors---red, green and blue components separately---using @(racket linear-seq).
The @(racket #:start?) and @(racket #:end?) keyword arguments work as in @(racket linear-seq).

@examples[#:eval plot-eval (plot (contour-intervals (λ (x y) (+ x y)) -2 2 -2 2
                                                    #:levels 4 #:contour-styles '(transparent)
                                                    #:colors (color-seq "red" "blue" 5)))]
}

@defproc[(color-seq* [colors (listof color/c)] [num exact-nonnegative-integer?]
                     [#:start? start? boolean? #t]
                     [#:end? end? boolean? #t])
         (listof (list/c real? real? real?))]{
Interpolates between colors---red, green and blue components separately---using @(racket linear-seq*).
The @(racket #:start?) and @(racket #:end?) keyword arguments work as in @(racket linear-seq).

@examples[#:eval plot-eval (plot (contour-intervals (λ (x y) (+ x y)) -2 2 -2 2
                                                    #:levels 4 #:contour-styles '(transparent)
                                                    #:colors (color-seq* '(red white blue) 5)))]
}

@defproc[(->color [c color/c]) (list/c real? real? real?)]{
Converts a non-integer plot color to an RGB triplet.

Symbols are converted to strings, and strings are looked up in a @(racket color-database<%>).
Lists are unchanged, and @(racket color%) objects are converted straightforwardly.

@examples[#:eval plot-eval
                 (->color 'navy)
                 (->color "navy")
                 (->color '(36 36 140))
                 (->color (make-object color% 36 36 140))]

This function does not convert integers to RGB triplets, because there is no way for it to know whether the color will be used for a pen or for a brush.
Use @(racket ->pen-color) and @(racket ->brush-color) to convert integers.
}

@defproc[(->pen-color [c plot-color/c]) (list/c real? real? real?)]{
Converts a @italic{line} color to an RGB triplet. This function interprets integer colors as darker and more saturated than @(racket ->brush-color) does.

Non-integer colors are converted using @(racket ->color).
Integer colors are chosen for good pairwise contrast, especially between neighbors.
Integer colors repeat starting with @(racket 128).

@examples[#:eval plot-eval
                 (equal? (->pen-color 0) (->pen-color 8))
                 (plot (contour-intervals
                        (λ (x y) (+ x y)) -2 2 -2 2
                        #:levels 7 #:contour-styles '(transparent)
                        #:colors (map ->pen-color (build-list 8 values))))]
}

@defproc[(->brush-color [c plot-color/c]) (list/c real? real? real?)]{
Converts a @italic{fill} color to an RGB triplet. This function interprets integer colors as lighter and less saturated than @(racket ->pen-color) does.

Non-integer colors are converted using @(racket ->color).
Integer colors are chosen for good pairwise contrast, especially between neighbors.
Integer colors repeat starting with @(racket 128).

@examples[#:eval plot-eval
                 (equal? (->brush-color 0) (->brush-color 8))
                 (plot (contour-intervals
                        (λ (x y) (+ x y)) -2 2 -2 2
                        #:levels 7 #:contour-styles '(transparent)
                        #:colors (map ->brush-color (build-list 8 values))))]

In the above example, @(racket map)ping @(racket ->brush-color) over the list is actually unnecessary, because @(racket contour-intervals) uses @(racket ->brush-color) internally to convert fill colors.

The @(racket function-interval) function generally plots areas using a fill color and lines using a line color.
Both kinds of color have the default value @(racket 3).
The following example reverses the default behavior; i.e it draws areas using @italic{line} color @(racket 3) and lines using @italic{fill} color @(racket 3):
@interaction[#:eval plot-eval (plot (function-interval sin (λ (x) 0) -4 4
                                                       #:color (->pen-color 3)
                                                       #:line1-color (->brush-color 3)
                                                       #:line2-color (->brush-color 3)
                                                       #:line1-width 4 #:line2-width 4))]
}

@defproc[(->pen-style [s plot-pen-style/c]) symbol?]{
Converts a symbolic pen style or a number to a symbolic pen style.
Symbols are unchanged.
Integer pen styles repeat starting at @(racket 5).

@examples[#:eval plot-eval
                 (eq? (->pen-style 0) (->pen-style 5))
                 (map ->pen-style '(0 1 2 3 4))]
}

@defproc[(->brush-style [s plot-brush-style/c]) symbol?]{
Converts a symbolic brush style or a number to a symbolic brush style.
Symbols are unchanged.
Integer brush styles repeat starting at @(racket 7).

@examples[#:eval plot-eval
                 (eq? (->brush-style 0) (->brush-style 7))
                 (map ->brush-style '(0 1 2 3))
                 (map ->brush-style '(4 5 6))]
}

@;====================================================================================================
@section{Plot-Specific Math}

@;----------------------------------------------------------------------------------------------------
@subsection{Real Functions}

@defproc[(polar->cartesian [θ real?] [r real?]) (vector/c real? real?)]{
Converts 2D polar coordinates to 3D cartesian coordinates.
}

@defproc[(3d-polar->3d-cartesian [θ real?] [ρ real?] [r real?]) (vector/c real? real? real?)]{
Converts 3D polar coordinates to 3D cartesian coordinates.
See @racket[parametric3d] for an example of use.
}

@defproc[(ceiling-log/base [b (and/c exact-integer? (>=/c 2))] [x (>/c 0)]) exact-integer?]{
Like @racket[(ceiling (/ (log x) (log b)))], but @racket[ceiling-log/base] is not susceptible to floating-point error.
@examples[#:eval plot-eval
                 (ceiling (/ (log 100) (log 10)))
                 (ceiling-log/base 10 100)
                 (ceiling (/ (log 1/1000) (log 10)))
                 (ceiling-log/base 10 1/1000)]
Various number-formatting functions use this.
}

@defproc[(floor-log/base [b (and/c exact-integer? (>=/c 2))] [x (>/c 0)]) exact-integer?]{
Like @racket[(floor (/ (log x) (log b)))], but @racket[floor-log/base] is not susceptible to floating-point error.
@examples[#:eval plot-eval
                 (floor (/ (log 100) (log 10)))
                 (floor-log/base 10 100)
                 (floor (/ (log 1000) (log 10)))
                 (floor-log/base 10 1000)]
This is a generalization of @racket[order-of-magnitude].
}

@defproc[(maybe-inexact->exact [x (or/c rational? #f)]) (or/c rational? #f)]{
Returns @racket[#f] if @racket[x] is @racket[#f]; otherwise @racket[(inexact->exact x)].
Use this to convert interval endpoints, which may be @racket[#f], to exact numbers.
}

@;----------------------------------------------------------------------------------------------------
@subsection[#:tag "math.vectors"]{Vector Functions}

@deftogether[(@defproc[(v+ [v1 (vectorof real?)] [v2 (vectorof real?)]) (vectorof real?)]
              @defproc[(v- [v1 (vectorof real?)] [v2 (vectorof real?)]) (vectorof real?)]
              @defproc[(vneg [v (vectorof real?)]) (vectorof real?)]
              @defproc[(v* [v (vectorof real?)] [c real?]) (vectorof real?)]
              @defproc[(v/ [v (vectorof real?)] [c real?]) (vectorof real?)])]{
Vector arithmetic. Equivalent to @racket[vector-map]p-ing arithmetic operators over vectors, but specialized so that 2- and 3-vector operations are much faster.
@examples[#:eval plot-eval
                 (v+ #(1 2) #(3 4))
                 (v- #(1 2) #(3 4))
                 (vneg #(1 2))
                 (v* #(1 2 3) 2)
                 (v/ #(1 2 3) 2)]
}

@defproc[(v= [v1 (vectorof real?)] [v2 (vectorof real?)]) boolean?]{
Like @racket[equal?] specialized to numeric vectors, but compares elements using @racket[=].
@examples[#:eval plot-eval
                 (equal? #(1 2) #(1 2))
                 (equal? #(1 2) #(1.0 2.0))
                 (v= #(1 2) #(1.0 2.0))]
}

@defproc[(vcross [v1 (vector/c real? real? real?)] [v2 (vector/c real? real? real?)])
         (vector/c real? real? real?)]{
Returns the right-hand vector cross product of @racket[v1] and @racket[v2].
@examples[#:eval plot-eval
                 (vcross #(1 0 0) #(0 1 0))
                 (vcross #(0 1 0) #(1 0 0))
                 (vcross #(0 0 1) #(0 0 1))]
}

@defproc[(vcross2 [v1 (vector/c real? real?)] [v2 (vector/c real? real?)]) real?]{
Returns the signed area of the 2D parallelogram with sides @racket[v1] and @racket[v2].
Equivalent to @racket[(vector-ref (vcross (vector-append v1 #(0)) (vector-append v2 #(0))) 2)] but faster.
@examples[#:eval plot-eval
                 (vcross2 #(1 0) #(0 1))
                 (vcross2 #(0 1) #(1 0))]
}

@defproc[(vdot [v1 (vectorof real?)] [v2 (vectorof real?)]) real?]{
Returns the dot product of @racket[v1] and @racket[v2].
}

@defproc[(vmag^2 [v (vectorof real?)]) real?]{
Returns the squared magnitude of @racket[v]. Equivalent to @racket[(vdot v v)].
}

@defproc[(vmag [v (vectorof real?)]) real?]{
Returns the magnitude of @racket[v]. Equivalent to @racket[(sqrt (vmag^2 v))].
}

@defproc[(vnormalize [v (vectorof real?)]) (vectorof real?)]{
Returns a normal vector in the same direction as @racket[v]. If @racket[v] is a zero vector, returns @racket[v].
@examples[#:eval plot-eval
                 (vnormalize #(1 1 0))
                 (vnormalize #(1 1 1))
                 (vnormalize #(0 0 0.0))]
}

@defproc[(vcenter [vs (listof (vectorof real?))]) (vectorof real?)]{
Returns the center of the smallest bounding box that contains @racket[vs].
@examples[#:eval plot-eval
                 (vcenter '(#(1 1) #(2 2)))]
}

@defproc[(vrational? [v (vectorof real?)]) boolean?]{
Returns @racket[#t] if every element of @racket[v] is @racket[rational?].
@examples[#:eval plot-eval
                 (vrational? #(1 2))
                 (vrational? #(+inf.0 2))
                 (vrational? #(#f 1))]
}

@;----------------------------------------------------------------------------------------------------
@subsection[#:tag "math.intervals"]{Intervals and Interval Functions}

@defstruct[ivl ([min real?] [max real?])]{
Represents a closed interval.

An interval with two real-valued endpoints always contains the endpoints in order:
@interaction[#:eval plot-eval (ivl 0 1) (ivl 1 0)]

@;{
If either endpoint is @racket[+nan.0], both are, and the interval represents the empty interval:
@interaction[#:eval plot-eval (ivl +nan.0 0) (ivl 0 +nan.0)]
}

An interval can have infinite endpoints:
@interaction[#:eval plot-eval (ivl -inf.0 0) (ivl 0 +inf.0) (ivl -inf.0 +inf.0)]

Functions that return rectangle renderers, such as @racket[rectangles] and @racket[discrete-histogram3d], accept vectors of @racket[ivl]s as arguments.
The @racket[ivl] struct type is also provided by @racketmodname[plot] so users of such renderers do not have to require @racketmodname[plot/utils].
}

@defproc[(rational-ivl? [i any/c]) boolean?]{
Returns @racket[#t] if @racket[i] is an interval and each of its endpoints is @racket[rational?].
@examples[#:eval plot-eval
                 (map rational-ivl? (list (ivl -1 1) (ivl -inf.0 2) 'bob))]
}

@;{
@defthing[empty-ivl ivl?]{
The empty interval.
}

@defproc[(ivl-meet [i ivl?] ...) ivl?]{
Returns the intersection of the given intervals.
@examples[#:eval plot-eval
                 (ivl-meet)
                 (ivl-meet (ivl 0 1) (ivl 2 3))
                 (ivl-meet (ivl 0 2) (ivl 1 3))
                 (ivl-meet empty-ivl (ivl 0 1))]
}

@defproc[(ivl-join [i ivl?] ...) ivl?]{
Returns the smallest interval that contains all the points in the given intervals.
@examples[#:eval plot-eval
                 (ivl-join)
                 (ivl-join (ivl 0 1) (ivl 2 3))
                 (ivl-join (ivl 0 2) (ivl 1 3))
                 (ivl-join empty-ivl (ivl 0 1))]
Think of it as returning an interval union, but with any gaps filled.
}

@defproc[(ivl-center [i ivl?]) (or/c real? #f)]{
@examples[#:eval plot-eval
                 (ivl-center (ivl -1 1))
                 (ivl-center empty-ivl)
                 (ivl-center (ivl -inf.0 +inf.0))]
}

@defproc[(ivl-contains? [i ivl?] [x real?]) boolean?]{
@examples[#:eval plot-eval
                 (ivl-contains? (ivl -1 1) 0)
                 (ivl-contains? (ivl -1 1) 2)
                 (ivl-contains? (ivl -inf.0 +inf.0) 0)
                 (ivl-contains? empty-ivl 0)]
}

@defproc[(ivl-empty? [i ivl?]) boolean?]{
@examples[#:eval plot-eval
                 (ivl-empty? empty-ivl)
                 (ivl-empty? (ivl 0 0))]
}

@defproc[(ivl-length [i ivl?]) (or/c real? #f)]{
@examples[#:eval plot-eval
                 (ivl-length empty-ivl)
                 (ivl-length (ivl 0 0))
                 (ivl-length (ivl -1 1))
                 (ivl-length (ivl -inf.0 +inf.0))]
}

@defproc[(ivl-inexact->exact [i ivl?]) ivl?]
@defproc[(ivl-rational? [i ivl?]) boolean?]
@defproc[(ivl-singular? [i ivl?]) boolean?]
@defproc[(ivl-translate [i ivl?] [d real?]) ivl?]
@defproc[(ivl-zero-length? [i ivl?]) boolean?]
}

@defproc[(bounds->intervals [xs (listof real?)]) (listof ivl?)]{
Given a list of points, returns intervals between each pair.

Use this to construct inputs for @(racket rectangles) and @(racket rectangles3d).
@examples[#:eval plot-eval (bounds->intervals (linear-seq 0 1 5))]
}

@defproc[(clamp-real [x real?] [i ivl?]) real?]{
}

@;----------------------------------------------------------------------------------------------------
@;{
@subsection[#:tag "math.rectangles"]{Rectangles and Rectangle Functions}

@margin-note*{The @racket[rect-meet] and @racket[rect-join] functions define a @link["http://en.wikipedia.org/wiki/Lattice_%28order%29"]{pointed lattice} over rectangles.
                  This fact may seem esoteric, but it allows @(plot-name) to combine multiple renderers with different rectangular bounds in a way that is intuitive and mathematically sound.}
@defproc[(rect-meet [i (vectorof ivl?)] ...) (vectorof ivl?)]{
}

@defproc[(rect-join [i (vectorof ivl?)] ...) (vectorof ivl?)]{
}

@defproc[(empty-rect [n exact-nonnegative-integer?]) (vectorof ivl?)]
@defproc[(rect-area [r (vectorof ivl?)]) (or/c real? #f)]
@defproc[(rect-center [r (vectorof ivl?)]) (vectorof real?)]
@defproc[(rect-contains? [r (vectorof ivl?)] [v (vectorof real?)]) boolean?]
@defproc[(rect-empty? [r (vectorof ivl?)]) boolean?]
@defproc[(rect-inexact->exact [r (vectorof ivl?)]) (vectorof ivl?)]
@defproc[(rect-known? [r (vectorof ivl?)]) boolean?]
@defproc[(rect-rational? [r (vectorof ivl?)]) boolean?]
@defproc[(rect-singular? [r (vectorof ivl?)]) boolean?]
@defproc[(rect-translate [r (vectorof ivl?)] [v (vectorof real?)]) (vectorof ivl?)]
@defproc[(rect-zero-area? [r (vectorof ivl?)]) boolean?]
@defproc[(rational-rect? [r any/c]) boolean?]
@defproc[(bounding-rect [vs (listof (vectorof ivl?))]) (vectorof ivl?)]
}

@;====================================================================================================
@;{
@section{Marching Squares and Cubes}

@defproc[(heights->lines [xa real?] [xb real?] [ya real?] [yb real?]
                         [z real?] [z1 real?] [z2 real?] [z3 real?] [z4 real?])
         (listof (list/c (vector/c real? real? real?)
                         (vector/c real? real? real?)))]

@defproc[(heights->polys [xa real?] [xb real?] [ya real?] [yb real?]
                         [za real?] [zb real?]
                         [z1 real?] [z2 real?] [z3 real?] [z4 real?])
         (listof (listof (vector/c real? real? real?)))]

@defproc[(heights->cube-polys [xa real?] [xb real?] [ya real?] [yb real?] [za real?] [zb real?]
                              [d real?]
                              [d1 real?] [d2 real?] [d3 real?] [d4 real?]
                              [d5 real?] [d6 real?] [d7 real?] [d8 real?])
         (listof (listof (vector/c real? real? real?)))]{
winding warning
}
}

@;====================================================================================================
@section{Dates and Times}

@defproc[(datetime->real [x (or/c plot-time? date? date*? sql-date? sql-time? sql-timestamp?)]) real?]{
Converts various date/time representations into UTC seconds, respecting time zone offsets.

For dates, the value returned is the number of seconds since @italic{a system-dependent UTC epoch}.
See @racket[date-ticks] for more information.

To plot a time series using dates pulled from an SQL database, simply set the relevant axis ticks (probably @racket[plot-x-ticks]) to @racket[date-ticks], and convert the dates to seconds using @racket[datetime->real] before passing them to @racket[lines].
To keep time zone offsets from influencing the plot, set them to @racket[0] first.
}

@defstruct[plot-time ([second (and/c (>=/c 0) (</c 60))]
                      [minute (integer-in 0 59)]
                      [hour (integer-in 0 23)]
                      [day exact-integer?])]{
A time representation that accounts for days, negative times (using negative days), and fractional seconds.

@(plot-name) (specifically @racket[time-ticks]) uses @racket[plot-time] internally to format times, but because renderer-producing functions require only real values,
user code should not need it. It is provided just in case.
}

@deftogether[(@defproc[(plot-time->seconds [t plot-time?]) real?]
              @defproc[(seconds->plot-time [s real?]) plot-time?])]{
Convert @racket[plot-time]s to real seconds, and vice-versa.
@examples[#:eval plot-eval
                 (define (plot-time+ t1 t2)
                   (seconds->plot-time (+ (plot-time->seconds t1)
                                          (plot-time->seconds t2))))
                 (plot-time+ (plot-time 32 0 12 1)
                             (plot-time 32 0 14 1))]
}
