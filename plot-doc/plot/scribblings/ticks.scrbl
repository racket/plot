#lang scribble/manual

@(require "common.rkt" (for-label racket/date db))

@title[#:tag "ticks and transforms"]{Axis Transforms and Ticks}

@declare-exporting[plot]
@defmodule*/no-declare[(plot) #:link-target? #f]

@section[#:tag "transforms"]{Axis Transforms}

The @italic{x}, @italic{y} and @italic{z} axes for any plot can be independently transformed by parameterizing the plot on different @racket[plot-x-transform], @racket[plot-y-transform] and @racket[plot-z-transform] values.
For example, to plot the @italic{x} axis with a log transform:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform])
                      (plot (function sin 1 100)))]
Most @racket[log-transform]ed plots use different ticks than the default, uniformly spaced ticks, however.
To put log ticks on the @italic{x} axis, set the @racket[plot-x-ticks] parameter:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform]
                                   [plot-x-ticks      (log-ticks)])
                      (plot (function sin 1 100)))]
See @secref["ticks"] for more details on parameterizing a plot's axis ticks.

@margin-note*{
To sample nonlinearly, the @italic{inverse} of a transform is applied to linearly sampled points. See @racket[make-axis-transform] and @racket[nonlinear-seq].}
Renderers cooperate with the current transforms by sampling nonlinearly. For example,
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform])
                      (plot3d (surface3d + 0.01 1 0.01 1)))]
Notice that the surface is sampled uniformly in appearance even though the @italic{x}-axis ticks are not spaced uniformly.

Transforms are applied to the primitive shapes that comprise a plot:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform])
                      (plot3d (surface3d + 0.01 1 0.01 1 #:samples 3)))]
Here, the renderer returned by @racket[surface3d] does not have to bend the polygons it draws; @racket[plot3d] does this automatically (by recursive subdivision).

@deftogether[(@defparam[plot-x-transform transform axis-transform/c #:value id-transform]
              @defparam[plot-y-transform transform axis-transform/c #:value id-transform]
              @defparam[plot-z-transform transform axis-transform/c #:value id-transform])]{
Independent, per-axis, monotone, nonlinear transforms. @(plot-name) comes with some typical (and some atypical) axis transforms, documented immediately below.
}

@defthing[id-transform axis-transform/c]{
The identity axis transform, the default transform for all axes.
}

@defthing[log-transform axis-transform/c]{
A log transform. Use this to generate plots with log-scale axes. Any such axis must have positive bounds.

The beginning of the @secref["ticks and transforms"] section has a working example. An example of exceeding the bounds is
@interaction[#:eval plot-eval
                    (eval:alts
                     (parameterize ([plot-x-transform  log-transform])
                       (plot (function (λ (x) x) -1 1)))
                     (eval:result "" "" "log-transform: expects type <positive real> as 1st argument, given: -1; other arguments were: 1"))]
See @racket[axis-transform-bound] and @racket[axis-transform-append] for ways to get around an axis transform's bounds limitations.
}

@defproc[(stretch-transform [a real?] [b real?] [scale (>/c 0)]) axis-transform/c]{
Returns an axis transform that stretches a finite interval.

The following example uses a @racket[stretch-transform] to draw attention to the interval [-1,1] in an illustration of the limit of @italic{sin(x)/x} as @italic{x} approaches zero (a critical part of proving the derivative of @italic{sin(x)}):
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (stretch-transform -1 1 20)]
                                   [plot-x-ticks  (ticks-add (plot-x-ticks) '(-1 1))])
                      (plot (list (y-axis -1 #:ticks? #f) (y-axis 1 #:ticks? #f)
                                  (function (λ (x) (/ (sin x) x)) -14 14
                                            #:width 2 #:color 4 #:label "y = sin(x)/x")
                                  (point-label (vector 0 1) "y → 1 as x → 0"
                                               #:anchor 'bottom-right))
                            #:y-max 1.2))]
}

@defproc[(collapse-transform [a real?] [b real?]) axis-transform/c]{
Returns an axis transform that collapses a finite interval to its midpoint.
For example, to remove part of the long, boring asymptotic approach of @italic{atan(x)} toward π/2:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (collapse-transform 50 150)])
                      (plot (function atan 10 200 #:label "y = atan(x)")
                            #:legend-anchor 'center))]
In this case, there were already ticks at the collapsed interval's endpoints.
If there had not been, it would have been necessary to use @racket[ticks-add] to let viewers know precisely the interval that was collapsed.
(See @racket[stretch-transform] for an example.)
}

@defthing[cbrt-transform axis-transform/c]{
A ``cube-root'' transform, mostly used for testing.
Unlike the log transform, it is defined on the entire real line, making it better for testing the appearance of plots with nonlinearly transformed axes.
}

@defproc[(hand-drawn-transform [freq (>/c 0)]) axis-transform/c]{
An @italic{extremely important} test case, which makes sure that @(plot-name) can use any monotone, invertible function as an axis transform.
The @(racket freq) parameter controls the ``shakiness'' of the transform. At high values, it makes plots look like Peanuts cartoons.
@examples[#:eval plot-eval
                 (parameterize ([plot-x-transform  (hand-drawn-transform 200)]
                                [plot-y-transform  (hand-drawn-transform 200)])
                   (plot (function sqr -1 1)))
                 (parameterize ([plot-x-transform  (hand-drawn-transform 50)]
                                [plot-y-transform  (hand-drawn-transform 50)]
                                [plot-z-transform  (hand-drawn-transform 50)])
                   (plot3d (contour-intervals3d (λ (x y) (- (sqr x) (sqr y)))
                                                -1 1 -1 1 #:samples 9)))]
}

@defthing[axis-transform/c contract? #:value (-> real? real? invertible-function? invertible-function?)]{
The contract for axis transforms.

The easiest ways to construct novel axis transforms are to use the axis transform combinators @racket[axis-transform-append], @racket[axis-transform-bound] and @racket[axis-transform-compose], or to apply @racket[make-axis-transform] to an @racket[invertible-function].
}

@defproc[(axis-transform-append [t1 axis-transform/c] [t2 axis-transform/c] [mid real?]) axis-transform/c]{
Returns an axis transform that transforms values less than @racket[mid] like @racket[t1], and transforms values greater than @racket[mid] like @racket[t2].
(Whether it transforms @racket[mid] like @racket[t1] or @racket[t2] is immaterial, as a transformed @racket[mid] is equal to @racket[mid] either way.)
@examples[#:eval plot-eval
                 (parameterize ([plot-x-transform  (axis-transform-append
                                                    (stretch-transform -2 -1 10)
                                                    (stretch-transform 1 2 10)
                                                    0)])
                   (plot (function (λ (x) x) -3 3)))]
}

@defproc[(axis-transform-bound [t axis-transform/c] [a real?] [b real?]) axis-transform/c]{
Returns an axis transform that transforms values like @racket[t] does in the interval [@racket[a],@racket[b]], but like the identity transform outside of it.
For example, to bound @racket[log-transform] to an interval in which it is well-defined,
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (axis-transform-bound
                                                       log-transform 0.01 +inf.0)])
                      (plot (function (λ (x) x) -4 8 #:label "y = x")))]
}

@defproc[(axis-transform-compose [t1 axis-transform/c] [t2 axis-transform/c]) axis-transform/c]{
Composes two axis transforms.
For example, to collapse part of a @racket[log-transform]ed axis, try something like
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (axis-transform-compose
                                                       log-transform
                                                       (collapse-transform 2 4))])
                      (plot (function (λ (x) x) 1 5)))]
Argument order matters, but predicting the effects of exchanging arguments can be difficult.
Fortunately, the effects are usually slight.
}

@defproc[(make-axis-transform [fun invertible-function?]) axis-transform/c]{
Given a monotone @racket[invertible-function], returns an axis transform.
Monotonicity is necessary, but cannot be enforced.
The inverse is used to take samples uniformly along transformed axes (see @racket[nonlinear-seq]).

@examples[#:eval plot-eval
                 (parameterize ([plot-y-transform  (make-axis-transform
                                                    (invertible-function sqrt sqr))])
                   (plot (function (λ (x) x) 0 5)))]

An axis transform created by @racket[make-axis-transform] (or by any of the above combinators) does not transform the endpoints of an axis's bounds, to within floating-point error.
For example,
@interaction[#:eval plot-eval
                    (match-let ([(invertible-function f g)
                                 (apply-axis-transform log-transform 1 3)])
                      (define xs '(1 2 3))
                      (define new-xs (map f xs))
                      (define old-xs (map g new-xs))
                      (values new-xs old-xs))]

Technically, @racket[fun] does not need to be truly invertible.
Given @racket[fun] = @racket[(invertible-function f g)], it is enough for @racket[f] to be a @hyperlink["http://en.wikipedia.org/wiki/Inverse_function#Left_and_right_inverses"]{left inverse} of @racket[g];
that is, always @racket[(f (g x)) = x] but not necessarily @racket[(g (f x)) = x].
If @racket[f] and @racket[g] had to be strict inverses of each other, there could be no @racket[collapse-transform].
}

@defproc[(apply-axis-transform [t axis-transform/c] [x-min real?] [x-max real?]) invertible-function?]{
Returns an invertible function that transforms axis points within the given axis bounds.
This convenience function is used internally to transform points before rendering, but is provided for completeness.
}

@section[#:tag "ticks"]{Axis Ticks}

Each plot axis has two indepedent sets of ticks: the @italic{near} ticks and the @italic{far} ticks.

@deftogether[(@defparam[plot-x-ticks ticks ticks? #:value (linear-ticks)]
              @defparam[plot-x-far-ticks ticks ticks? #:value (ticks-mimic plot-x-ticks)]
              @defparam[plot-y-ticks ticks ticks? #:value (linear-ticks)]
              @defparam[plot-y-far-ticks ticks ticks? #:value (ticks-mimic plot-y-ticks)]
              @defparam[plot-z-ticks ticks ticks? #:value (linear-ticks)]
              @defparam[plot-z-far-ticks ticks ticks? #:value (ticks-mimic plot-z-ticks)])]{
@examples[#:eval plot-eval
                 (parameterize ([plot-x-label      "Near x axis"]
                                [plot-y-label      "Near y axis"]
                                [plot-z-label      "Near z axis"]
                                [plot-x-ticks      (date-ticks)]
                                [plot-y-ticks      (time-ticks)]
                                [plot-z-ticks      (fraction-ticks)]
                                [plot-x-far-label  "Far x axis"]
                                [plot-y-far-label  "Far y axis"]
                                [plot-z-far-label  "Far z axis"]
                                [plot-x-far-ticks  (linear-ticks)]
                                [plot-y-far-ticks  (currency-ticks)]
                                [plot-z-far-ticks  (log-ticks #:base 2)])
                   (plot3d (lines3d '(#(1 1 1) #(40000000 4 4)) #:style 'transparent)
                           #:angle 45 #:altitude 50
                           #:title "Axis Names and Tick Locations"))]
At any @racket[#:angle], the far @italic{x} and @italic{y} ticks are behind the plot, and the far @italic{z} ticks are on the right.
Far ticks are drawn, but not labeled, if they are identical to their corresponding near ticks.
They are always identical by default.

@deftech{Major ticks} are longer than @deftech{minor ticks}. Major tick labels are always drawn unless collapsed with a nearby tick.
Minor tick labels are never drawn.

Renderers produced  by @racket[contours] and @racket[contour-intervals] use the value of @racket[plot-z-ticks] to place and label contour lines.
For example, compare plots of the same function renderered using both @racket[contour-intervals] and @racket[contour-intervals3d]:
@interaction[#:eval plot-eval
                    (parameterize ([plot-z-ticks  (currency-ticks)])
                      (define (saddle x y) (- (sqr x) (sqr y)))
                      (values
                       (plot (contour-intervals saddle -1 1 -1 1 #:label "z")
                             #:legend-anchor 'center)
                       (plot3d (contour-intervals3d saddle -1 1 -1 1 #:label "z")
                               #:legend-anchor 'center)))]
}

@defproc[(contour-ticks [z-ticks ticks?] [z-min real?] [z-max real?]
                        [levels (or/c 'auto exact-positive-integer? (listof real?))]
                        [intervals? boolean?])
         (listof tick?)]{
Returns the ticks used for contour values.
This is used internally by renderers returned from @racket[contours], @racket[contour-intervals], @racket[contours3d], @racket[contour-intervals3d], and @racket[isosurfaces3d], but is provided for completeness.

When @racket[levels] is @racket['auto], the returned values do not correspond @italic{exactly} with the values of ticks returned by @racket[z-ticks]: they might be missing the endpoint values. For example,
@interaction[#:eval plot-eval
                 (map pre-tick-value 
                      (filter pre-tick-major? (ticks-generate (plot-z-ticks) 0 1)))
                 (map pre-tick-value
                      (contour-ticks (plot-z-ticks) 0 1 'auto #f))]
}

@defparam[plot-d-ticks ticks ticks? #:value (linear-ticks)]{
The ticks used for default isosurface values in @racket[isosurfaces3d].
}

@defparam[plot-r-ticks ticks ticks? #:value (linear-ticks)]{
The ticks used for radius lines in @racket[polar-axes].
}

@defstruct[ticks ([layout ticks-layout/c] [format ticks-format/c])]{
A @racket[ticks] for a near or far axis consists of a @racket[layout] function, which determines the number of ticks and where they will be placed, and a @racket[format] function, which determines the ticks' labels.
}

@defproc[(ticks-generate [ticks ticks?] [min real?] [max real?]) (listof tick?)]{
Generates the @racket[tick] values for the range [@racket[min], @racket[max]], with layout and format specified by @racket[ticks].

@examples[#:eval plot-eval (ticks-generate (plot-x-ticks) 1/3 2/3)]
}

@defparam[ticks-default-number number exact-positive-integer? #:value 4]{
Most tick layout functions (and thus their corresponding @racket[ticks]-constructing functions) have a @racket[#:number] keyword argument with default @racket[(ticks-default-number)].
What the number means depends on the tick layout function.
Most use it for an average number of major ticks.

It is unlikely to mean the exact number of major ticks.
Without adjusting the number of ticks, layout functions usually cannot find uniformly spaced ticks that will have simple labels after formatting.
For example, the following plot shows the actual number of major ticks for the interval [0,@italic{x}] when the requested number of ticks is 8, as generated by @racket[linear-ticks-layout]:
@interaction[#:eval plot-eval
                    (plot (function (λ (x)
                                      (count pre-tick-major?
                                             ((linear-ticks-layout #:number 8) 0 x)))
                                    0.1 10)
                          #:x-label "Interval [0,x]" #:y-label "Number of ticks")]
}

@subsection{Linear Ticks}

@deftogether[(@defproc[(linear-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)]
                                            [#:base base (and/c exact-integer? (>=/c 2)) 10]
                                            [#:divisors divisors (listof exact-positive-integer?) '(1 2 4 5)])
                       ticks-layout/c]
              @defproc[(linear-ticks-format) ticks-format/c]
              @defproc[(linear-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                       [#:base base (and/c exact-integer? (>=/c 2)) 10]
                       [#:divisors divisors (listof exact-positive-integer?) '(1 2 4 5)])
                       ticks?])]{
The layout function, format function, and combined @racket[ticks] for uniformly spaced ticks.

To lay out ticks, @racket[linear-ticks-layout] finds the power of @racket[base] closest to the axis interval size, chooses a simple first tick, and then chooses a skip length using @racket[divisors] that maximizes the number of ticks without exceeding @racket[number].
@margin-note*{For strategic use of non-default arguments, see @racket[bit/byte-ticks], @racket[currency-ticks], and @racket[fraction-ticks].}
The default arguments correspond to the standard 1-2-5-in-base-10 rule used almost everywhere in plot tick layout.

To format ticks, @racket[linear-ticks-format] uses @racket[real->plot-label], and uses @racket[digits-for-range] to determine the maximum number of fractional digits in the decimal expansion.
}

@subsection{Log Ticks}

@deftogether[(@defproc[(log-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)]
                                         [#:base base (and/c exact-integer? (>=/c 2)) 10])
                       ticks-layout/c]
              @defproc[(log-ticks-format [#:base base (and/c exact-integer? (>=/c 2)) 10])
                       ticks-format/c]
              @defproc[(log-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                                  [#:base base (and/c exact-integer? (>=/c 2)) 10])
                       ticks?])]{
The layout function, format function, and combined @racket[ticks] for exponentially spaced major ticks.
(The minor ticks between are uniformly spaced.)
Use these ticks for @racket[log-transform]ed axes, because when exponentially spaced tick positions are @racket[log-transform]ed, they become uniformly spaced.

The @racket[#:base] keyword argument is the logarithm base.
See @racket[plot-z-far-ticks] for an example of use.
}

@subsection{Date Ticks}

@deftogether[(@defproc[(date-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)])
                       ticks-layout/c]
              @defproc[(date-ticks-format [#:formats formats (listof string?) (date-ticks-formats)])
                       ticks-format/c]
              @defproc[(date-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                                   [#:formats formats (listof string?) (date-ticks-formats)])
                       ticks?])]{
The layout function, format function, and combined @racket[ticks] for uniformly spaced ticks with date labels.

These axis ticks regard values as being in seconds since @italic{a system-dependent Universal Coordinated Time (UTC) epoch}.
(For example, the Unix and Mac OS X epoch is January 1, 1970 UTC, and the Windows epoch is January 1, 1601 UTC.)
Use @racket[date->seconds] to convert local dates to seconds, or @racket[datetime->real] to convert dates to UTC seconds in a way that accounts for time zone offsets.

Actually, @racket[date-ticks-layout] does not always space ticks @italic{quite} uniformly.
For example, it rounds ticks that are spaced about one month apart or more to the nearest month.
Generally, @racket[date-ticks-layout] tries to place ticks at minute, hour, day, week, month and year boundaries, as well as common multiples such as 90 days or 6 months.

To try to avoid displaying overlapping labels, @racket[date-ticks-format] chooses date formats from @racket[formats] for which labels will contain no redundant information.

All the format specifiers given in @racketmodname[srfi/19] (which are derived from Unix's @tt{date} command), except those that represent time zones, are allowed in date format strings.
}

@defparam[date-ticks-formats formats (listof string?) #:value 24h-descending-date-ticks-formats]{
The default date formats.
}

@deftogether[(
@defthing[24h-descending-date-ticks-formats (listof string?)
  #:value
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
    "~fs")]
@defthing[12h-descending-date-ticks-formats (listof string?)
  #:value
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
    "~fs")])]

@subsection{Time Ticks}

@deftogether[(@defproc[(time-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)])
                       ticks-layout/c]
              @defproc[(time-ticks-format [#:formats formats (listof string?) (time-ticks-formats)])
                       ticks-format/c]
              @defproc[(time-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                                   [#:formats formats (listof string?) (time-ticks-formats)])
                       ticks?])]{
The layout function, format function, and combined @racket[ticks] for uniformly spaced ticks with time labels.

These axis ticks regard values as being in seconds.
Use @racket[datetime->real] to convert @racket[sql-time] or @racket[plot-time] values to seconds.

Generally, @racket[time-ticks-layout] tries to place ticks at minute, hour and day boundaries, as well as common multiples such as 12 hours or 30 days.

To try to avoid displaying overlapping labels, @racket[time-ticks-format] chooses a date format from @racket[formats] for which labels will contain no redundant information.

All the time-related format specifiers given in @racketmodname[srfi/19] (which are derived from Unix's @tt{date} command) are allowed in time format strings.
}

@defparam[time-ticks-formats formats (listof string?) #:value 24h-descending-time-ticks-formats]{
The default time formats.
}

@deftogether[(
@defthing[24h-descending-time-ticks-formats (listof string?)
  #:value
  '("~dd ~H:~M:~f"
    "~dd ~H:~M"
    "~dd ~Hh"
    "~dd"
    "~H:~M:~f"
    "~H:~M"
    "~Hh"
    "~M:~fs"
    "~Mm"
    "~fs")]
@defthing[12h-descending-time-ticks-formats (listof string?)
  #:value
  '("~dd ~I:~M:~f ~p"
    "~dd ~I:~M ~p"
    "~dd ~I ~p"
    "~dd"
    "~I:~M:~f ~p"
    "~I:~M ~p"
    "~I ~p"
    "~M:~fs"
    "~Mm"
    "~fs")])]

@subsection{Currency Ticks}

@deftogether[(@defproc[(currency-ticks-format [#:kind kind (or/c string? symbol?) 'USD]
                                              [#:scales scales (listof string?) (currency-ticks-scales)]
                                              [#:formats formats (list/c string? string? string?) (currency-ticks-formats)])
                       ticks-format/c]
              @defproc[(currency-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                                       [#:kind kind (or/c string? symbol?) 'USD]
                                       [#:scales scales (listof string?) (currency-ticks-scales)]
                                       [#:formats formats (list/c string? string? string?) (currency-ticks-formats)])
                       ticks?])]{
The format function and combined @racket[ticks] for uniformly spaced ticks with currency labels;
@racket[currency-ticks] uses @racket[linear-ticks-layout] for layout.

The @racket[#:kind] keyword argument is either a string containing the currency symbol, or a currency code such as @racket['USD], @racket['GBP] or @racket['EUR].
The @racket[currency-ticks-format] function can map most ISO 4217 currency codes to their corresponding currency symbol.

The @racket[#:scales] keyword argument is a list of suffixes for each 10@superscript{3} scale, such as @racket["K"] (US thousand, or kilo), @racket["bn"] (UK short-scale billion) or @racket["Md"] (EU long-scale milliard). Off-scale amounts are given power-of-ten suffixes such as ``×10@superscript{21}.''

The @racket[#:formats] keyword argument is a list of three format strings, representing the formats of positive, negative, and zero amounts, respectively. The format specifiers are:
@itemlist[@item{@racket["~$"]: replaced by the currency symbol}
          @item{@racket["~w"]: replaced by the whole part of the amount}
          @item{@racket["~f"]: replaced by the fractional part, with 2 or more decimal digits}
          @item{@racket["~s"]: replaced by the scale suffix}
          @item{@racket["~~"]: replaced by ``~''}]
}

@deftogether[(@defparam[currency-ticks-scales scales (listof string?) #:value us-currency-scales]
              @defparam[currency-ticks-formats formats (list/c string? string? string?) #:value us-currency-formats])]{
The default currency scales and formats.

For example, a @(plot-name) user in France would probably begin programs with
@racketblock[(require plot)
             (currency-ticks-scales eu-currency-scales)
             (currency-ticks-formats eu-currency-formats)]
and use @racket[(currency-ticks #:kind 'EUR)] for local currency or @racket[(currency-ticks #:kind 'JPY)] for Japanese Yen.

Cultural sensitivity notwithstanding, when writing for a local audience, it is generally considered proper to use local currency scales and formats for foreign currencies, but use the foreign currency symbol.
}

@defthing[us-currency-scales (listof string?) #:value '("" "K" "M" "B" "T")]{
Short-scale suffix abbreviations as commonly used in the United States, Canada, and some other English-speaking countries. These stand for ``kilo,'' ``million,'' ``billion,'' and ``trillion.''
}

@defthing[uk-currency-scales (listof string?) #:value '("" "k" "m" "bn" "tr")]{
Short-scale suffix abbreviations as commonly used in the United Kingdom since switching to the short scale in 1974, and as currently recommended by the Daily Telegraph and Times style guides.
}

@defthing[eu-currency-scales (listof string?) #:value '("" "K" "M" "Md" "B")]{
European Union long-scale suffix abbreviations, which stand for ``kilo,'' ``million,'' ``milliard,'' and ``billion.''

The abbreviations actually used vary with geography, even within countries, but these seem to be common.
Further long-scale suffix abbreviations such as for ``billiard'' are ommitted due to lack of even weak consensus.
}

@defthing[us-currency-formats (list/c string? string? string?) #:value '("~$~w.~f~s" "(~$~w.~f~s)" "~$0")]{
Common currency formats used in the United States.
}

@defthing[uk-currency-formats (list/c string? string? string?) #:value '("~$~w.~f~s" "-~$~w.~f~s" "~$0")]{
Common currency formats used in the United Kingdom.
Note that it sensibly uses a negative sign to denote negative amounts.
}

@defthing[eu-currency-formats (list/c string? string? string?) #:value '("~w,~f ~s~$" "-~w,~f ~s~$" "0 ~$")]{
A guess at common currency formats for the European Union.
Like scale suffixes, actual formats vary with geography, but currency formats can even vary with audience or tone.
}

@subsection{Other Ticks}

@deftogether[(@defthing[no-ticks-layout ticks-layout/c]
              @defthing[no-ticks-format ticks-format/c]
              @defthing[no-ticks ticks? #:value (ticks no-ticks-layout no-ticks-format)])]{
The layout function, format function, and combined @racket[ticks] for no ticks whatsoever.
@examples[#:eval plot-eval
                 (parameterize ([plot-x-ticks  no-ticks]
                                [plot-y-ticks  no-ticks]
                                [plot-x-label  #f]
                                [plot-y-label  #f])
                   (plot (list (polar-axes) (polar (λ (θ) 1/3)))))]
}

@deftogether[(@defproc[(bit/byte-ticks-format [#:size size (or/c 'byte 'bit) 'byte]
                                              [#:kind kind (or/c 'CS 'SI) 'CS])
                       ticks-format/c]
              @defproc[(bit/byte-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                                       [#:size size (or/c 'byte 'bit) 'byte]
                                       [#:kind kind (or/c 'CS 'SI) 'CS])
                       ticks?])]{
The format function and and combined @racket[ticks] for bit or byte values.

The @racket[#:kind] keyword argument indicates either International System of Units (@racket['SI]) suffixes, as used to communicate hard drive capacities, or Computer Science (@racket['CS]) suffixes, as used to communicate memory capacities.

For layout, @racket[bit/byte-ticks] uses @racket[linear-ticks-layout] with
@itemize[
         @item{If @racket[kind] is @racket['SI], base @racket[10] and divisors @racket['(1 2 4 5)].}
         @item{If @racket[kind] is @racket['CS], base @racket[2] and divisors @racket['(1 2)].}
         ]
}

@deftogether[(@defproc[(fraction-ticks-format [#:base base (and/c exact-integer? (>=/c 2)) 10]
                                              [#:divisors divisors (listof exact-positive-integer?) '(1 2 3 4 5)])
                       ticks-format/c]
              @defproc[(fraction-ticks [#:base base (and/c exact-integer? (>=/c 2)) 10]
                                       [#:divisors divisors (listof exact-positive-integer?) '(1 2 3 4 5)])
                       ticks?])]{
The format function and and combined @racket[ticks] for fraction-formatted values.
For layout, @racket[fraction-ticks] uses @racket[linear-ticks-layout], passing it the given @racket[divisors].
}

@subsection{Tick Combinators}

@defproc[(ticks-mimic [thunk (-> ticks?)]) ticks?]{
Returns a @racket[ticks] that mimics the given @racket[ticks] returned by @racket[thunk].
Used in default values for @racket[plot-x-far-ticks], @racket[plot-y-far-ticks] and @racket[plot-z-far-ticks] to ensure that, unless one of these parameters is changed, the far tick labels are not drawn.
}

@defproc[(ticks-add [t ticks?] [xs (listof real?)] [major? boolean? #t]) ticks?]{
Returns a new @racket[ticks] that acts like @racket[t], except that it puts additional ticks at positions @racket[xs]. If @racket[major?] is true, the ticks at positions @racket[xs] are all @tech{major ticks}; otherwise, they are minor ticks.
}

@defproc[(ticks-scale [t ticks?] [fun invertible-function?]) ticks?]{
Returns a new @racket[ticks] that acts like @racket[t], but for an axis transformed by @racket[fun].
Unlike with typical @secref["transforms"], @racket[fun] is allowed to transform axis endpoints.
(See @racket[make-axis-transform] for an explanation about transforming endpoints.)

Use @racket[ticks-scale] to plot values at multiple scales simultaneously, with one scale on the near axis and one scale on the far axis.
The following example plots degrees Celsius on the left and degrees Farenheit on the right:
@interaction[#:eval plot-eval
                    (parameterize
                        ([plot-x-ticks      (time-ticks)]
                         [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                         (linear-scale 9/5 32))]
                         [plot-y-label      "Temperature (\u00b0C)"]
                         [plot-y-far-label  "Temperature (\u00b0F)"])
                      (define data
                        (list #(0 0) #(15 0.6) #(30 9.5) #(45 10.0) #(60 16.6)
                              #(75 41.6) #(90 42.7) #(105 65.5) #(120 78.9)
                              #(135 78.9) #(150 131.1) #(165 151.1) #(180 176.2)))
                      (plot (list
                             (function (λ (x) (/ (sqr x) 180)) 0 180
                                       #:style 'long-dash #:color 3 #:label "Trend")
                             (lines data #:color 2 #:width 2)
                             (points data #:color 1 #:line-width 2 #:label "Measured"))
                            #:y-min -25 #:x-label "Time"))]
}

@subsection{Tick Data Types and Contracts}

@defstruct[pre-tick ([value real?] [major? boolean?])]{
Represents a tick that has not yet been labeled.
}

@defstruct[(tick pre-tick) ([label string?])]{
Represents a tick with a label.
}

@defthing[ticks-layout/c contract? #:value (-> real? real? (listof pre-tick?))]{
The contract for tick layout functions. Note that a layout function returns @racket[pre-tick]s, or unlabeled ticks.
}

@defthing[ticks-format/c contract? #:value (-> real? real? (listof pre-tick?) (listof string?))]{
The contract for tick format functions. A format function receives axis bounds so it can determine how many decimal digits to display (usually by applying @racket[digits-for-range] to the bounds).
}

@section[#:tag "invertible"]{Invertible Functions}

@defstruct[invertible-function ([f (-> real? real?)] [g (-> real? real?)])]{
Represents an invertible function. Used for @secref["transforms"] and by @racket[ticks-scale].

The function itself is @racket[f], and its inverse is @racket[g].
Because @racket[real?]s can be inexact, this invariant must be approximate and therefore cannot be enforced.
(For example, @racket[(exp (log 10))] = @racket[10.000000000000002].)
The obligation to maintain it rests on whomever constructs one.
}

@defthing[id-function invertible-function? #:value (invertible-function (λ (x) x) (λ (x) x))]{
The identity function as an @racket[invertible-function].
}

@defproc[(invertible-compose [f1 invertible-function?] [f2 invertible-function?])
         invertible-function?]{
Returns the composition of two invertible functions.
}

@defproc[(invertible-inverse [h invertible-function?]) invertible-function?]{
Returns the inverse of an invertible function.
}

@defproc[(linear-scale [m rational?] [b rational? 0]) invertible-function?]{
Returns a one-dimensional linear scaling function, as an @racket[invertible-function].
This function constructs the most common arguments to @racket[ticks-scale].
}
