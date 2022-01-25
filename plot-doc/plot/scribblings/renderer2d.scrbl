#lang scribble/manual

@(require "common.rkt")

@title[#:tag "renderer2d"]{2D Renderers}

@declare-exporting[plot]
@defmodule*/no-declare[(plot) #:link-target? #f]

@section[#:tag "renderer2d-function-arguments"]{2D Renderer Function Arguments}

Functions that return 2D renderers always have these kinds of arguments:
@itemlist[
          @item{Required (and possibly optional) arguments representing the graph to plot.}
          @item{Optional keyword arguments for overriding calculated bounds, with the default value @(racket #f).}
          @item{Optional keyword arguments that determine the appearance of the plot.}
          @item{The optional keyword argument @(racket #:label), which specifies the name of the renderer in the legend.}]

We will take @(racket function), perhaps the most commonly used renderer-producing function, as an example.

@bold{Graph arguments.} The first argument to @(racket function) is the required @(racket f), the function to plot.
It is followed by two optional arguments @(racket x-min) and @(racket x-max), which specify the renderer's @italic{x} bounds.
(If not given, the @italic{x} bounds will be the plot area @italic{x} bounds, as requested by another renderer or specified to @(racket plot) using @(racket #:x-min) and @(racket #:x-max).)

These three arguments define the @deftech{graph} of the function @(racket f), a possibly infinite set of pairs of points @(racket x),@(racket (f x)).
An infinite graph cannot be plotted directly, so the renderer must approximately plot the points in it.
The renderer returned by @(racket function) does this by drawing lines connected end-to-end.

@bold{Overriding bounds arguments.} Next in @(racket function)'s argument list are the keyword arguments @(racket #:y-min) and @(racket #:y-max), which override the renderer's calculated @italic{y} bounds if given.

@bold{Appearance arguments.} The next keyword argument is @(racket #:samples), which determines the quality of the renderer's approximate plot (higher is better).
Following @(racket #:samples) are @(racket #:color), @(racket #:width), @(racket #:style) and @(racket #:alpha), which determine the color, width, style and opacity of the lines comprising the plot.

In general, the keyword arguments that determine the appearance of plots follow consistent naming conventions.
The most common keywords are @(racket #:color) (for fill and line colors), @(racket #:width) (for line widths), @(racket #:style) (for fill and line styles) and @(racket #:alpha).
When a function needs both a fill color and a line color, the fill color is given using @(racket #:color), and the line color is given using @(racket #:line-color) (or some variation, such as @(racket #:line1-color)). Styles follow the same rule.

Every appearance keyword argument defaults to the value of a parameter.
This allows whole families of plots to be altered with little work.
For example, setting @(racket (line-color 3)) causes every subsequent renderer that draws connected lines to draw its lines in blue.

@bold{Label argument.} Lastly, there is @(racket #:label). If given, the
@(racket function) renderer will generate a label entry that @(racket plot)
puts in the legend.  The label argument can be a string or a @(racket pict).
For most use cases, the string will be sufficient, especially since it allows
using Unicode characters, and thus some mathematical notation.  For more
complex cases, a @(racket pict) can be used, whic allows arbitrary text and
graphics to be used as label entries.

@history[#:changed "7.9" "Added support for pictures for #:label"]

Not every renderer-producing function has a @(racket #:label) argument; for example, @(racket error-bars).


@section{2D Point Renderers}

@defproc[(points [vs  (sequence/c (sequence/c #:min-count 2 real?))]
                 [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                 [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                 [#:sym sym point-sym/c (point-sym)]
                 [#:color color plot-color/c (point-color)]
                 [#:fill-color fill-color (or/c plot-color/c 'auto) 'auto]
                 [#:x-jitter x-jitter (>=/c 0) (point-x-jitter)]
                 [#:y-jitter y-jitter (>=/c 0) (point-y-jitter)]
                 [#:size size (>=/c 0) (point-size)]
                 [#:line-width line-width (>=/c 0) (point-line-width)]
                 [#:alpha alpha (real-in 0 1) (point-alpha)]
                 [#:label label (or/c string? pict? #f) #f]
                 ) renderer2d?]{
Returns a @tech{renderer} that draws points. Use it, for example, to draw 2D scatter plots.

The renderer sets its bounds to the smallest rectangle that contains the points.
Still, it is often necessary to override these bounds, especially with randomized data. For example,
@interaction[#:eval plot-eval
                    (parameterize ([plot-width    150]
                                   [plot-height   150]
                                   [plot-x-label  #f]
                                   [plot-y-label  #f])
                      (define xs (build-list 20 (λ _ (random))))
                      (define ys (build-list 20 (λ _ (random))))
                      (list (plot (points (map vector xs ys)))
                            (plot (points (map vector xs ys)
                                          #:x-min 0 #:x-max 1
                                          #:y-min 0 #:y-max 1))))]
Readers of the first plot could only guess that the random points were generated in [0,1] × [0,1].

The @racket[#:sym] argument may be any integer, a Unicode character or string, or a symbol in
@racket[known-point-symbols].
Use an integer when you need different points but don't care exactly what they are.

When @racket[x-jitter] is non-zero, all points are translated by a random amount
at most @racket[x-jitter] from their original position along the x-axis.
A non-zero @racket[y-jitter] similarly translates points along the y-axis.
Jitter is added in both directions so total spread is twice the value given.
To be precise, each point @racket[p] is moved to a random location inside a rectangle centered at
@racket[p] with width at most twice @racket[x-jitter] and height at most twice @racket[y-jitter]
subject to the constraint that new points lie within
[@racket[x-min], @racket[x-max]] and [@racket[y-min], @racket[y-max]]
if these bounds are non-@racket[#f].

@interaction[#:eval plot-eval
                    (plot
                      (points (for/list ([_i (in-range 1000)])
                                (list 0 0))
                              #:alpha 0.4
                              #:x-jitter 1
                              #:y-jitter 1
                              #:sym 'fullcircle1
                              #:color "blue")
                      #:x-min -2 #:x-max 2 #:y-min -2 #:y-max 2)]

Randomly moving data points is almost always a bad idea, but jittering in a controlled manner can
sometimes be useful.
For example:
@margin-note{More examples of jittering:
  @hyperlink["http://kieranhealy.org/blog/archives/2015/02/03/another-look-at-the-california-vaccination-data/"]{Another Look at the California Vaccination Data}
  and
  @hyperlink["https://pavelfatin.com/typing-with-pleasure/"]{Typing with Pleasure}}

@itemlist[
  @item{To highlight the size of a dense (or @hyperlink["https://en.wiktionary.org/wiki/overplotting"]{overplotted}) sample.}
  @item{To see the distribution of 1-dimensional data; as a substitute for box or violin plots.}
  @item{To anonymize spatial data, showing i.e. an office's neighborhood but hiding its address.}
]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(vector-field
          [f (or/c (-> real? real? (sequence/c real?))
                   (-> (vector/c real? real?) (sequence/c real?)))]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:samples samples exact-positive-integer? (vector-field-samples)]
          [#:scale scale (or/c real? (one-of/c 'auto 'normalized)) (vector-field-scale)]
          [#:color color plot-color/c (vector-field-color)]
          [#:line-width line-width (>=/c 0) (vector-field-line-width)]
          [#:line-style line-style plot-pen-style/c (vector-field-line-style)]
          [#:alpha alpha (real-in 0 1) (vector-field-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Returns a renderer that draws a vector field.

If @(racket scale) is a real number, arrow lengths are multiplied by @(racket scale).
If @(racket 'auto), the scale is calculated in a way that keeps arrows from overlapping.
If @(racket 'normalized), each arrow is made the same length: the maximum length that would have been allowed by @(racket 'auto).

The shape of the arrow-head can be controlled with @racket[arrow-head-size-or-scale] and @racket[arrow-head-angle].

An example of automatic scaling:
@interaction[#:eval plot-eval
                    (plot (vector-field (λ (x y) (vector (+ x y) (- x y)))
                                        -2 2 -2 2))]

@history[#:changed "7.9" "Added support for pictures for #:label and controlling the arrowhead"]
}

@defproc[(error-bars
          [bars (sequence/c (sequence/c #:min-count 3 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:color color plot-color/c (error-bar-color)]
          [#:line-width line-width (>=/c 0) (error-bar-line-width)]
          [#:line-style line-style plot-pen-style/c (error-bar-line-style)]
          [#:width width (>=/c 0) (error-bar-width)]
          [#:alpha alpha (real-in 0 1) (error-bar-alpha)]
          [#:invert? invert? boolean? #f]
          ) renderer2d?]{
Returns a renderer that draws error bars.
The first and second element in each vector in @(racket bars) comprise the coordinate; the third is the height.
@interaction[#:eval plot-eval
                    (plot (list (function sqr 1 7)
                                (error-bars (list (vector 2 4 12)
                                                  (vector 4 16 20)
                                                  (vector 6 36 10)))))]

If @racket[invert?] is @racket[#t], the x and y coordinates are inverted, and the bars are drawn horizontally
rather than vertically. This is intended for use with the corresponding option of @racket[discrete-histogram]
and @racket[stacked-histogram].

@history[#:changed "1.1" @elem{Added the @racket[#:invert?] option.}]}

@defproc[(candlesticks
          [candles (sequence/c (sequence/c #:min-count 5 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:up-color up-color plot-color/c (candlestick-up-color)]
          [#:down-color down-color plot-color/c (candlestick-down-color)]
          [#:line-width line-width (>=/c 0) (candlestick-line-width)]
          [#:line-style line-style plot-pen-style/c (candlestick-line-style)]
          [#:width width (>=/c 0) (candlestick-width)]
          [#:alpha alpha (real-in 0 1) (candlestick-alpha)]
          ) renderer2d?]{
Returns a renderer that draws candlesticks. This is most common when plotting historical prices for financial 
instruments. The first element in each vector of @(racket candles) comprises the x-axis coordinate; the second, third, 
fourth, and fifth elements in each vector comprise the open, high, low, and close, respectively, of the y-axis coordinates.
@interaction[#:eval plot-eval
                    (plot (list (candlesticks (list (vector 2 4 12 4 8)
                                                    (vector 4 16 20 8 12)
                                                    (vector 6 24 36 10 24)))))]
}

@defproc[(color-field
          [f (or/c (-> real? real? plot-color/c)
                   (-> (vector/c real? real?) plot-color/c))]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:samples samples exact-positive-integer? (color-field-samples)]
          [#:alpha alpha (real-in 0 1) (color-field-alpha)]
          ) renderer2d?]{
Returns a renderer that draws rectangles filled with a color based on the center point.

@interaction[#:eval plot-eval
                    (plot (color-field (λ (x y) (if (< (+ (sqr x) (sqr y)) 1) (random 10) 'black))
                                        -2 2 -2 2))]
@history[#:added "7.9"]
}

@section{2D Line Renderers}

@defproc[(function [f (real? . -> . real?)]
                   [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
                   [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                   [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                   [#:color color plot-color/c (line-color)]
                   [#:width width (>=/c 0) (line-width)]
                   [#:style style plot-pen-style/c (line-style)]
                   [#:alpha alpha (real-in 0 1) (line-alpha)]
                   [#:label label (or/c string? pict? #f) #f]
                   ) renderer2d?]{
Returns a renderer that plots a function of @italic{x}. For example, a parabola:
@interaction[#:eval plot-eval (plot (function sqr -2 2))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(inverse [f (real? . -> . real?)]
                  [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
                  [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                  [#:color color plot-color/c (line-color)]
                  [#:width width (>=/c 0) (line-width)]
                  [#:style style plot-pen-style/c (line-style)]
                  [#:alpha alpha (real-in 0 1) (line-alpha)]
                  [#:label label (or/c string? pict? #f) #f]
                  ) renderer2d?]{
Like @(racket function), but regards @(racket f) as a function of @italic{y}.
For example, a parabola, an inverse parabola, and the reflection line:
@interaction[#:eval plot-eval
                    (plot (list (axes)
                                (function sqr -2 2 #:label "y = x²")
                                (function (λ (x) x) #:color 0 #:style 'dot #:label "y = x")
                                (inverse sqr -2 2 #:color 3 #:label "x = y²")))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(lines [vs  (sequence/c (sequence/c #:min-count 2 real?))]
                [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                [#:color color plot-color/c (line-color)]
                [#:width width (>=/c 0) (line-width)]
                [#:style style plot-pen-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? pict? #f) #f]
                ) renderer2d?]{

  Returns a renderer that draws lines connecting the points in the input
  sequence @(racket vs).
  
  This is directly useful for plotting a time series, such as a random walk:
  
@interaction[#:eval plot-eval
                    (plot (lines
                           (reverse
                            (for/fold ([lst (list (vector 0 0))]) ([i  (in-range 1 200)])
                              (match-define (vector x y) (first lst))
                              (cons (vector i (+ y (* 1/100 (- (random) 1/2)))) lst)))
                           #:color 6 #:label "Random walk"))]

The @(racket parametric) and @(racket polar) functions are defined using @(racket lines).

  If any of the points in @(racket vs) is @(racket +nan.0), no line segment
  will be drawn at that position.  This can be used to draw several
  independent data sets with one @(racket lines) renderer, improving rendering
  performence if the datasets contain a large number of points.

  @history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(parametric [f (real? . -> . (sequence/c real?))]
                     [t-min rational?] [t-max rational?]
                     [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                     [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                     [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                     [#:color color plot-color/c (line-color)]
                     [#:width width (>=/c 0) (line-width)]
                     [#:style style plot-pen-style/c (line-style)]
                     [#:alpha alpha (real-in 0 1) (line-alpha)]
                     [#:label label (or/c string? pict? #f) #f]
                     ) renderer2d?]{
Returns a renderer that plots vector-valued functions of time.
For example, the circle as a function of time can be plotted using
@interaction[#:eval plot-eval
             (plot (parametric (λ (t) (vector (cos t) (sin t))) 0 (* 2 pi)))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(polar [f (real? . -> . real?)]
                [θ-min real? 0] [θ-max real? (* 2 pi)]
                [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                [#:color color plot-color/c (line-color)]
                [#:width width (>=/c 0) (line-width)]
                [#:style style plot-pen-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? pict? #f) #f]
                ) renderer2d?]{
Returns a renderer that plots functions from angle to radius.
Note that the angle parameters @(racket θ-min) and @(racket θ-max) default to @(racket 0) and @(racket (* 2 pi)).

For example, drawing a full circle:
@interaction[#:eval plot-eval (plot (polar (λ (θ) 1)))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(density [xs (sequence/c real?)]
                  [bw-adjust (>/c 0) 1]
                  [ws (or/c (sequence/c (>=/c 0)) #f) #f]
                  [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                  [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                  [#:color color plot-color/c (line-color)]
                  [#:width width (>=/c 0) (line-width)]
                  [#:style style plot-pen-style/c (line-style)]
                  [#:alpha alpha (real-in 0 1) (line-alpha)]
                  [#:label label (or/c string? pict? #f) #f]
                  ) renderer2d?]{
Returns a renderer that plots an estimated density for the given points, which are optionally weighted by @racket[ws].
The bandwidth for the kernel is calculated as @(racket (* bw-adjust 1.06 sd (expt n -0.2))), where @(racket sd) is the standard deviation of the data and @(racket n) is the number of points.
Currently, the only supported kernel is the Gaussian.

For example, to plot an estimated density of the triangle distribution:
@interaction[#:eval plot-eval
                    (plot (list (function (λ (x) (cond [(or (x . < . -1) (x . > . 1))  0]
                                                       [(x . < . 0)   (+ 1 x)]
                                                       [(x . >= . 0)  (- 1 x)]))
                                          -1.5 1.5 #:label "Density")
                                (density (build-list
                                          2000 (λ (n) (- (+ (random) (random)) 1)))
                                         #:color 0 #:width 2 #:style 'dot
                                         #:label "Est. density")))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}


@defproc[(arrows [vs  (or/c (listof (sequence/c #:min-count 2 real?))
                            (vectorof (vector/c (sequence/c #:min-count 2 real?)
                                                (sequence/c #:min-count 2 real?))))]
                 [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                 [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                 [#:color color plot-color/c (arrows-color)]
                 [#:width width (>=/c 0) (arrows-line-width)]
                 [#:style style plot-pen-style/c (arrows-line-style)]
                 [#:alpha alpha (real-in 0 1) (arrows-alpha)]
                 [#:arrow-head-size-or-scale size (or/c (list/c '= (>=/c 0)) (>=/c 0)) (arrow-head-size-or-scale)]
                 [#:arrow-head-angle angle (>=/c 0) (arrow-head-angle)]
                 [#:label label (or/c string? pict? #f) #f]
                 ) renderer2d?]{
Returns a renderer which draws arrows. Arrows can be specified either as sequences of 2D points,
in this case they will be drawn as connected arrows between each two adjacent points,
or they can be specified as an origin point and a rectangular magnitude vector, in which case each arrow
is drawn individually. See example below.

In @racket[vs] list and vector are interchangeable. Arrow-heads are only drawn when the endpoint is inside the drawing area.
 @interaction[#:eval plot-eval
              (plot (list
                     (arrows
                      `((0 0) (2 1) (3 3) (0 0))
                      #:arrow-head-size-or-scale '(= 20)
                      #:arrow-head-angle .2
                      #:color 6 #:label "a+b+c=0")
                     (arrows
                      `(((2 0) (0 1)) ((3 0) (-1 1)))
                      #:arrow-head-size-or-scale .2
                      #:color 2 #:label "d")))]
@history[#:added "7.9"]
}

@defproc[(hrule [y real?]
                [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
                [#:color color plot-color/c (line-color)]
                [#:width width (>=/c 0) (line-width)]
                [#:style style plot-pen-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? pict? #f) #f]
                ) renderer2d?]{
Draws a horizontal line at @italic{y}.
By default, the line spans the entire plot area width.

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(vrule [x real?]
                [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
                [#:color color plot-color/c (line-color)]
                [#:width width (>=/c 0) (line-width)]
                [#:style style plot-pen-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? pict? #f) #f]
                ) renderer2d?]{
Draws a vertical line at @italic{x}.
By default, the line spans the entire plot area height.

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@section{2D Interval Renderers}

These renderers each correspond with a line renderer, and graph the area between two lines.

@defproc[(function-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Corresponds with @(racket function).

@interaction[#:eval plot-eval (plot (function-interval (λ (x) 0) (λ (x) (exp (* -1/2 (sqr x))))
                                                       -4 4 #:line1-style 'transparent))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(inverse-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Corresponds with @(racket inverse).

@interaction[#:eval plot-eval (plot (inverse-interval sin (λ (x) 0) (- pi) pi
                                                      #:line2-style 'long-dash))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(lines-interval
          [v1s (sequence/c (sequence/c #:min-count 2 real?))]
          [v2s (sequence/c (sequence/c #:min-count 2 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Corresponds with @(racket lines).

@interaction[#:eval plot-eval
                    (plot (list
                           (tick-grid)
                           (lines-interval (list #(0 0) #(1 1/2)) (list #(0 1) #(1 3/2))
                                           #:color 4 #:line1-color 4 #:line2-color 4
                                           #:label "Parallelogram")))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(parametric-interval
          [f1 (real? . -> . (sequence/c real?))]
          [f2 (real? . -> . (sequence/c real?))]
          [t-min rational?] [t-max rational?]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Corresponds with @(racket parametric).

@interaction[#:eval plot-eval
                    (define (f1 t) (vector (* 2 (cos (* 4/5 t)))
                                           (* 2 (sin (* 4/5 t)))))
                    (define (f2 t) (vector (* 1/2 (cos t))
                                           (* 1/2 (sin t))))
                    (plot (parametric-interval f1 f2 (- pi) pi))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(polar-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [θ-min rational? 0] [θ-max rational? (* 2 pi)]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Corresponds with @(racket polar).

@interaction[#:eval plot-eval
                    (define (f1 θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))
                    (define (f2 θ) (+ 1 (* 1/4 (cos (* 10 θ)))))
                    (plot (list (polar-axes #:number 10)
                                (polar-interval f1 f2 #:label "[f1,f2]")))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@section{2D Contour (Isoline) Renderers}

@defproc[(isoline
          [f (real? real? . -> . real?)] [z real?]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Returns a renderer that plots a contour line, or a line of constant value (height).
A circle of radius @(racket r), for example, is the line of constant value @(racket r) for the distance function:
@interaction[#:eval plot-eval (plot (isoline (λ (x y) (sqrt (+ (sqr x) (sqr y)))) 1.5
                                             -2 2 -2 2 #:label "z"))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}
In this case, @(racket r) = @(racket 1.5).

This function would have been named @racket[contour], except the name was already used by a deprecated function.
It may be renamed in the future, with @racket[isoline] as an alias.

@defproc[(contours
          [f (real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:widths widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:styles styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof real?)) (contour-alphas)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Returns a renderer that plots contour lines, or lines of constant value (height).

When @(racket levels) is @(racket 'auto), the number of contour lines and their values are chosen the same way as axis tick positions; i.e. they are chosen to be simple.
When @(racket levels) is a number, @(racket contours) chooses that number of values, evenly spaced, within the output range of @(racket f).
When @(racket levels) is a list, @(racket contours) plots contours at the values in @(racket levels).

For example, a saddle:
@interaction[#:eval plot-eval (plot (contours (λ (x y) (- (sqr x) (sqr y)))
                                              -2 2 -2 2 #:label "z"))]

The appearance keyword arguments assign a color, width, style and opacity @italic{to each contour line}.
Each can be given as a list or as a function from a list of output values of @(racket f) to a list of appearance values.
In both cases, when there are more contour lines than list elements, the colors, widths, styles and alphas in the list repeat.

For example,
@interaction[#:eval plot-eval (plot (contours (λ (x y) (- (sqr x) (sqr y)))
                                              -2 2 -2 2 #:levels 4
                                              #:colors '("blue" "red")
                                              #:widths '(4 1)
                                              #:styles '(solid dot)))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(contour-intervals
          [f (real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (contour-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof ivl?)) (contour-interval-colors)]
          [#:styles styles (plot-brush-styles/c (listof ivl?)) (contour-interval-styles)]
          [#:contour-colors contour-colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:contour-widths contour-widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:contour-styles contour-styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof ivl?)) (contour-interval-alphas)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Returns a renderer that fills the area between contour lines, and additionally draws contour lines.

For example, the canonical saddle, with its gradient field superimposed:
@interaction[#:eval plot-eval
                    (plot (list (contour-intervals (λ (x y) (- (sqr x) (sqr y)))
                                                   -2 2 -2 2 #:label "z")
                                (vector-field (λ (x y) (vector (* 2 x) (* -2 y)))
                                              #:color "black" #:label "Gradient")))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@section{2D Rectangle Renderers}

@defproc[(rectangles
          [rects  (sequence/c (sequence/c #:min-count 2 ivl?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Returns a renderer that draws rectangles.

The rectangles are given as a sequence of sequences of intervals---each inner
sequence defines the bounds of a rectangle.  Any of the bounds can be
@racket[-inf.0] or @racket[+inf.0], in which case the rectangle extents to the
edge of the plot area in the respective direction.

For example,
@interaction[#:eval plot-eval (plot (rectangles (list (vector (ivl -1 0) (ivl -1 1))
                                                      (vector (ivl 0 2) (ivl 1 2)))))]

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(area-histogram
          [f (real? . -> . real?)]
          [bin-bounds (sequence/c real?)]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          ) renderer2d?]{
Returns a renderer that draws a histogram approximating the area under a curve.
The @(racket #:samples) argument determines the accuracy of the calculated areas.
@interaction[#:eval plot-eval
                    (require (only-in plot/utils linear-seq))
                    (define (f x) (exp (* -1/2 (sqr x))))
                    (plot (list (area-histogram f (linear-seq -4 4 10))
                                (function f -4 4)))]
}

@defproc[(discrete-histogram
          [cat-vals (sequence/c (or/c (vector/c any/c (or/c real? ivl? #f))
                                      (list/c any/c (or/c real? ivl? #f))))]
          [#:x-min x-min (or/c rational? #f) 0] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:skip skip (>=/c 0) (discrete-histogram-skip)]
          [#:invert? invert? boolean? (discrete-histogram-invert?)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? pict? #f) #f]
          [#:add-ticks? add-ticks? boolean? #t]
          [#:far-ticks? far-ticks? boolean? #f]
          ) renderer2d?]{
Returns a renderer that draws a discrete histogram.

@examples[#:eval plot-eval
                 (plot (discrete-histogram (list #(A 1) #(B 2) #(B 3)
                                                 (vector 'C (ivl 0.5 1.5)))))]

Use @racket[#:invert? #t] to draw horizontal bars. See @racket[stacked-histogram] for an example.

Each bar takes up exactly one plot unit, and is drawn with @racket[(* 1/2 gap)] empty space on each side.
Bar number @racket[i] is drawn at @racket[(+ x-min (* i skip))].
Thus, the first bar (@racket[i] = @racket[0]) is drawn in the interval between @racket[x-min] (default @racket[0]) and @racket[(+ x-min 1)].

To plot two histograms side-by-side, pass the appropriate @racket[x-min] value to the second renderer. For example,
@interaction[#:eval plot-eval
                    (plot (list (discrete-histogram (list #(a 1) #(b 2) #(c 3) #(d 2)
                                                          #(e 4) #(f 2.5) #(g 1))
                                                    #:label "Numbers per letter")
                                (discrete-histogram (list #(1 1) #(4 2) #(3 1.5))
                                                    #:x-min 8
                                                    #:label "Numbers per number"
                                                    #:color 2 #:line-color 2)))]
Here, the first histogram has @racket[7] bars, so the second is drawn starting at @racket[x-min] = @racket[8].

To interleave histograms, such as when plotting benchmark results, use a @racket[skip] value larger than or equal to the number of histograms, and give each histogram a different @racket[x-min].
For example,
@interaction[#:eval plot-eval
                    (plot (list (discrete-histogram
                                 '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5))
                                 #:skip 2.5 #:x-min 0
                                 #:label "AMD")
                                (discrete-histogram
                                 '(#(Eggs 1.4) #(Bacon 2.3) #(Pancakes 3.1))
                                 #:skip 2.5 #:x-min 1
                                 #:label "Intel" #:color 2 #:line-color 2))
                          #:x-label "Breakfast Food" #:y-label "Cooking Time (minutes)"
                          #:title "Cooking Times For Breakfast Food, Per Processor")]
When interleaving many histograms, consider setting the @racket[discrete-histogram-skip] parameter to change @racket[skip]'s default value.

@history[#:changed "7.9" "Added support for pictures for #:label"]
}

@defproc[(stacked-histogram
          [cat-vals (sequence/c (or/c (vector/c any/c (sequence/c real?))
                                      (list/c any/c (sequence/c real?))))]
          [#:x-min x-min (or/c rational? #f) 0] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:skip skip (>=/c 0) (discrete-histogram-skip)]
          [#:invert? invert? boolean? (discrete-histogram-invert?)]
          [#:colors colors (plot-colors/c nat/c) (stacked-histogram-colors)]
          [#:styles styles (plot-brush-styles/c nat/c) (stacked-histogram-styles)]
          [#:line-colors line-colors (plot-colors/c nat/c) (stacked-histogram-line-colors)]
          [#:line-widths line-widths (pen-widths/c nat/c) (stacked-histogram-line-widths)]
          [#:line-styles line-styles (plot-pen-styles/c nat/c) (stacked-histogram-line-styles)]
          [#:alphas alphas (alphas/c nat/c) (stacked-histogram-alphas)]
          [#:labels labels (labels/c nat/c) '(#f)]
          [#:add-ticks? add-ticks? boolean? #t]
          [#:far-ticks? far-ticks? boolean? #f]
          ) (listof renderer2d?)]{
Returns a list of renderers that draw parts of a stacked histogram.
The heights of each bar section are given as a list.
@examples[#:eval plot-eval
                 (plot (stacked-histogram (list #(a (1 1 1)) #(b (1.5 3))
                                                #(c ()) #(d (1/2)))
                                          #:invert? #t
                                          #:labels '("Red" #f "Blue"))
                       #:legend-anchor 'top-right)]
}

@section{Violin and Box Plot Renderers}

@defproc[(violin
          [vs (sequence/c real?)]
          [#:x x real? 0]
          [#:width width (>=/c 0) 1]
          [#:bandwidth bandwidth (or/c real? #f) #f]
          [#:invert? invert? boolean? #f]
          [#:label label (or/c string? pict? #f) #f]
          [#:add-ticks? add-ticks? boolean? #t]
          [#:far-ticks? far-ticks? boolean? #f]
          [#:y-min y-min (or/c rational? #f) #f]
          [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line-color line1-color plot-color/c (interval-line1-color)]
          [#:line-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          ) renderer2d?]{

Draws a @hyperlink["https://en.wikipedia.org/wiki/Violin_plot"]{violin} plot
from the list of real values @racket[vs].  The plot is centered at @racket[x]
and the @racket[width] parameter is used as a scaling factor to control the
width of the violin.

The default kernel density bandwidth is determined by
@racket[silverman-bandwidth].

When @racket[invert?] is @racket[#f], the violin plot is drawn vertically,
when it is @racket[#t], the x and y coordinates are inverted, and the violin
is drawn horizontally.

@racket[label] defines the plot label, it is the value shown in the plot
legend as well as on the X axis under the violin plot (or Y axis if the plot
is inverted).  The label is shown on the X axis on only if @racket[add-ticks?]
is @racket[#t], and, if @racket[far-ticks?] is @racket[#t] the label is placed
on the far axis.

@racket[y-min] and @racket[y-max] define the vertical range to draw the
violin, by default, the entire violin is drawn.

@racket[samples] defines the number of samples used by the function renderer
while drawing the violin outline.

See @secref["renderer2d-function-arguments"] for the meaning of the other
arguments.

@interaction[#:eval plot-eval
             (parameterize ([plot-pen-color-map 'tab20]
                            [plot-brush-color-map 'tab20]
                            [plot-x-label #f]
                            [plot-y-label #f])
               (define (rnorm sample-count mean stddev)
                 (sample (normal-dist mean stddev) sample-count))
               (define a (rnorm 50 10 5))
               (define b (append (rnorm 50 13 1) (rnorm 50 18 1)))
               (define c (rnorm 20 25 4))
               (define d (rnorm 10 12 2))
               (plot
                (for/list ([data (list a b c d)]
                           [label (list "a" "b" "c" "d")]
                           [index (in-naturals)])
                  (violin data
                          #:label label
                          #:invert? #t
                          #:x index
                          #:width 10/8
                          #:color (+ (* index 2) 1)
                          #:line-color (* index 2)))
                #:legend-anchor 'no-legend))]

@history[#:added "8.5"]
}

@defproc[(box-and-whisker
          [vs (sequence/c real?)]
          [#:weights ws (sequence/c real?) #f]
          [#:x x real? 0]
          [#:width width (>=/c 0) 1]
          [#:iqr-scale iqr-scale (>=/c 0) 1.5]
          [#:invert? invert? boolean? #f]
          [#:label label (or/c string? pict? #f) #f]
          [#:add-ticks? add-ticks? boolean? #t]
          [#:far-ticks? far-ticks? boolean? #f]
          [#:box-color box-color plot-color/c (rectangle-color)]
          [#:box-style box-style plot-brush-style/c (rectangle-style)]
          [#:box-line-color box-line-color plot-color/c (rectangle-line-color)]
          [#:box-line-width box-line-width (>=/c 0) (rectangle-line-width)]
          [#:box-line-style box-line-style plot-pen-style/c (rectangle-line-style)]
          [#:box-alpha box-alpha (real-in 0 1) (rectangle-alpha)]
          [#:show-outliers? show-outliers? boolean? #t]
          [#:outlier-color outlier-color plot-color/c (point-color)]
          [#:outlier-sym outlier-sym point-sym/c (point-sym)]
          [#:outlier-fill-color outlier-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:outlier-size outlier-size (>=/c 0) (point-size)]
          [#:outlier-line-width outlier-line-width (>=/c 0) (point-line-width)]
          [#:outlier-alpha outlier-alpha (real-in 0 1) (point-alpha)]
          [#:show-whiskers? show-whiskers? boolean? #t]
          [#:whisker-color whisker-color plot-color/c (line-color)]
          [#:whisker-width whisker-width (>=/c 0) (line-width)]
          [#:whisker-style whisker-style plot-pen-style/c (line-style)]
          [#:whisker-alpha whisker-alpha (real-in 0 1) (line-alpha)]
          [#:show-median? show-median? boolean? #t]
          [#:median-color median-color plot-color/c (line-color)]
          [#:median-width median-width (>=/c 0) (line-width)]
          [#:median-style median-style plot-pen-style/c (line-style)]
          [#:median-alpha median-alpha (real-in 0 1) (line-alpha)]
          ) renderer2d?]{

Draws a @hyperlink["https://en.wikipedia.org/wiki/Box_plot"]{box and whisker}
plot from the list of real values @racket[vs], possibly weighted by the values
in @racket[ws].  The plot is centered at @racket[x] and the @racket[width]
parameter is used as a scaling factor to control the width of the box.

The @racket[iqr-scale] controls the scaling factor for the inter-quantile
range, which decides how far the whiskers extent and which points are
considered outliers.

When @racket[invert?] is @racket[#f], the box plot is drawn vertically, when
it is @racket[#t], the x and y coordinates are inverted, and the box plot is
drawn horizontally.

@racket[label] defines the plot label, it is the value shown in the plot
legend as well as on the X axis under the box plot (or Y axis if the plot is
inverted).  The label is shown on the X axis on only if @racket[add-ticks?] is
@racket[#t], and, if @racket[far-ticks?] is @racket[#t] the label is placed on
the far axis.

See @secref["renderer2d-function-arguments"] for the meaning of the other
arguments.

@interaction[#:eval plot-eval
             (parameterize ([plot-pen-color-map 'tab20]
                            [plot-brush-color-map 'tab20]
                            [plot-x-label #f]
                            [plot-y-label #f])
               (define (rnorm sample-count mean stddev)
                 (sample (normal-dist mean stddev) sample-count))
               (define a (rnorm 50 10 5))
               (define b (append (rnorm 50 13 1) (rnorm 50 18 1)))
               (define c (rnorm 20 25 4))
               (define d (rnorm 10 12 2))
               (plot
                (for/list ([data (list a b c d)]
                           [label (list "a" "b" "c" "d")]
                           [index (in-naturals)])
                  (box-and-whisker data
                                   #:label label
                                   #:invert? #f
                                   #:x index
                                   #:width 6/8
                                   #:box-color (+ (* index 2) 1)
                                   #:box-line-color (* index 2)
                                   #:whisker-color (* index 2)
                                   #:median-color "red"))
                #:legend-anchor 'no-legend))]

@history[#:added "8.5"]
}

@section{2D Plot Decoration Renderers}

@defproc[(x-axis [y real? 0]
                 [#:ticks? ticks? boolean? (x-axis-ticks?)]
                 [#:labels? labels? boolean? (x-axis-labels?)]
                 [#:far? far? boolean? (x-axis-far?)]
                 [#:alpha alpha (real-in 0 1) (x-axis-alpha)])
         renderer2d?]{
Returns a renderer that draws an @italic{x} axis.
}

@defproc[(y-axis [x real? 0]
                 [#:ticks? ticks? boolean? (y-axis-ticks?)]
                 [#:labels? labels? boolean? (y-axis-labels?)]
                 [#:far? far? boolean? (y-axis-far?)]
                 [#:alpha alpha (real-in 0 1) (y-axis-alpha)])
         renderer2d?]{
Returns a renderer that draws a @italic{y} axis.
}

@defproc[(axes [x real? 0] [y real? 0]
               [#:x-ticks? x-ticks? boolean? (x-axis-ticks?)]
               [#:y-ticks? y-ticks? boolean? (y-axis-ticks?)]
               [#:x-labels? x-labels? boolean? (x-axis-labels?)]
               [#:y-labels? y-labels? boolean? (y-axis-labels?)]
               [#:x-alpha x-alpha (real-in 0 1) (x-axis-alpha)]
               [#:y-alpha y-alpha (real-in 0 1) (y-axis-alpha)])
         (listof renderer2d?)]{
Returns a list containing an @(racket x-axis) renderer and a @(racket y-axis) renderer. See @(racket inverse) for an example.
}

@defproc[(polar-axes [#:number num exact-nonnegative-integer? (polar-axes-number)]
                     [#:ticks? ticks? boolean? (polar-axes-ticks?)]
                     [#:labels? labels? boolean? (polar-axes-labels?)]
                     [#:alpha alpha (real-in 0 1) (polar-axes-alpha)])
         renderer2d?]{
Returns a renderer that draws polar axes. See @(racket polar-interval) for an example.
}

@defproc[(x-tick-lines) renderer2d?]{
Returns a renderer that draws vertical lines from the lower @italic{x}-axis ticks to the upper.
}

@defproc[(y-tick-lines) renderer2d?]{
Returns a renderer that draws horizontal lines from the left @italic{y}-axis ticks to the right.
}

@defproc[(tick-grid) (listof renderer2d?)]{
Returns a list containing an @(racket x-tick-lines) renderer and a @(racket y-tick-lines) renderer.
See @(racket lines-interval) for an example.
}

@defproc[(point-label
          [v (sequence/c real?)] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:face face (or/c string? #f) (plot-font-face)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a labeled point.
If @(racket label) is @(racket #f), the point is labeled with its position.

@interaction[#:eval plot-eval
                    (plot (list (function sqr 0 2)
                                (point-label (vector 1 1))))]

The remaining labeled-point functions are defined in terms of this one.
}

@defproc[(point-pict
          [v (sequence/c real?)]
          [pict pict?]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a point with a pict as the label.

@interaction[#:eval plot-eval
                    (require pict)
                    (plot (list (function sqr 0 2)
                                (point-pict (vector 1 1) (standard-fish 40 15))))]

The remaining labeled-pict functions are defined in terms of this one.
}

@defproc[(function-label
          [f  (real? . -> . real?)] [x real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:face face (or/c string? #f) (plot-font-face)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a labeled point on a function's graph.

@interaction[#:eval plot-eval
                    (plot (list (function sin (- pi) pi)
                                (function-label sin (* 1/6 pi) "(1/6 π, 1/2)"
                                                #:anchor 'right)))]
}

@defproc[(function-pict
          [f  (real? . -> . real?)] [x real?] [pict pict?]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a point with a pict as the label on a function's graph.
}

@defproc[(inverse-label
          [f  (real? . -> . real?)] [y real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:face face (or/c string? #f) (plot-font-face)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a labeled point on a function's inverted graph.
}

@defproc[(inverse-pict
          [f  (real? . -> . real?)] [y real?] [pict pict?]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a point with a pict as the label on a function's inverted graph.
}


@defproc[(parametric-label
          [f (real? . -> . (sequence/c real?))] [t real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:face face (or/c string? #f) (plot-font-face)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a labeled point on a parametric function's graph.
}

@defproc[(parametric-pict
          [f (real? . -> . (sequence/c real?))] [t real?] [pict pict?]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a point with a pict as the label on a parametric function's graph.
}


@defproc[(polar-label
          [f (real? . -> . real?)] [θ real?] [label (or/c string? #f) #f]
          [#:color color plot-color/c (plot-foreground)]
          [#:size size (>=/c 0) (plot-font-size)]
          [#:face face (or/c string? #f) (plot-font-face)]
          [#:family family font-family/c (plot-font-family)]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:angle angle real? (label-angle)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a labeled point on a polar function's graph.
}


@defproc[(polar-pict
          [f (real? . -> . real?)] [θ real?] [pict pict?]
          [#:anchor anchor anchor/c (label-anchor)]
          [#:point-color point-color plot-color/c (point-color)]
          [#:point-fill-color point-fill-color (or/c plot-color/c 'auto) 'auto]
          [#:point-size point-size (>=/c 0) (label-point-size)]
          [#:point-line-width point-line-width (>=/c 0) (point-line-width)]
          [#:point-sym point-sym point-sym/c 'fullcircle]
          [#:alpha alpha (real-in 0 1) (label-alpha)]
          ) renderer2d?]{
Returns a renderer that draws a point with a pict as the label on a polar function's graph.
}

@section[#:tag "2d-plot-snip-interactive-overlays"]{Interactive Overlays for 2D plots}

@defmodule[plot/snip]

A plot @racket[snip%] object returned by @racket[plot-snip] can be set up to
provide interactive overlays.  This feature can be used, for example, to show
the current value of the plot function at the mouse cursor.

If the code below is evaluated in DrRacket, the resulting plot will show a
vertical line tracking the mouse and the current plot position is shown on a
label.  This is achieved by adding a mouse callback to the plot snip returned
by @racket[plot-snip].  When the mouse callback is invoked, it will add a
@racket[vrule] at the current X position and a @racket[point-label] at the
current value of the plotted function.

@racketblock[
(require plot)
(define snip (plot-snip (function sin) #:x-min -5 #:x-max 5))
(define (mouse-callback snip event x y)
   (if (and x y)
       (send snip set-overlay-renderers
             (list (vrule x)
                   (point-label (vector x (sin x)))))
       (send snip set-overlay-renderers #f)))
(send snip set-mouse-event-callback mouse-callback)
snip]

Here are a few hints for adding common interactive elements to racket plots:

@itemlist[

@item{The @racket[hrule] and @racket[vrule] renderers can be used to draw
horizontal and vertical lines that track the mouse position}

@item{The @racket[rectangles] renderer can be used to highlight a region on
the plot.  For example, to highlight a vertical region between @racket[_xmin]
and @racket[_xmax], you can use:

@racketblock[
(rectangles (list (vector (ivl _xmin _xmax) (ivl -inf.0 +inf.0)))
            #:alpha 0.2)]
}

@item{A @racket[point-label] renderer can be used to add a point with a
string label to the plot.  To add only the label, use @racket['none] as the
value for the @racket[#:point-sym] argument.}

@item{A @racket[point-pict] renderer can be used to add a point with an
attached @racketmodname[pict] instead of a string label.  This can be used to
draw fancy labels (for example with rounded corners), or any other type of
graphics element.}

@item{A @racket[points] renderer can be used to mark specific locations on
the plot, without specifying a label for them}
]

@defclass[2d-plot-snip% snip% ()]{

An instance of this class is returned by @racket[plot-snip].

@defmethod[(set-mouse-event-callback [callback (or/c plot-mouse-event-callback/c #f)]) any/c]{

Set a callback function to be invoked with mouse events from the snip.  The
callback is invoked with the actual snip object, the @racket[mouse-event%] and
the X, Y position of the mouse in plot coordinates (i.e., the coordinate system
used by the renderers in the plot).  The X and Y values are
@racket[#f] when the mouse is outside the plot area (for example, when the
mouse is over the axis area).

When a callback is installed, the default zoom functionality of the plot snips
is disabled.  This can be restored by calling
@racket[set-mouse-event-callback] with a @racket[#f] argument.

}

@defmethod[(set-overlay-renderers [renderers (or/c (treeof renderer2d?) #f)]) any/c]{

Set a collection of renderers to be drawn
on top of the existing plot.  This can be any combination of 2D renderers, but
it will not be able to modify the axes or the dimensions of the plot area.
Only one set of overlay renderers can be installed; calling this method a
second time will replace the previous overlays.  Specifying @racket[#f] as the
renderers will cause overlays to be disabled.

}
}

@defthing[plot-mouse-event-callback/c contract? #:value (-> (is-a?/c snip%)
                                                            (is-a?/c mouse-event%)
                                                            (or/c real? #f)
                                                            (or/c real? #f)
                                                            any/c)]{
A contract for callback functions passed to @racket[set-mouse-event-callback].
}
