#lang scribble/manual

@(require "common.rkt")

@title[#:tag "params"]{Plot and Renderer Parameters}

@declare-exporting[plot]
@defmodule*/no-declare[(plot) #:link-target? #f]

@section{Compatibility}

@defparam[plot-deprecation-warnings? warnings boolean? #:value #f]{
When @(racket #t), prints a deprecation warning to @(racket current-error-port) on the first use of @(racket mix), @(racket line), @(racket contour), @(racket shade), @(racket surface), or a keyword argument of @(racket plot) or @(racket plot3d) that exists solely for backward compatibility.
}

@section{Output}

@defparam[plot-new-window? new-window? boolean? #:value #f]{
When @(racket #t), @(racket plot) and @(racket plot3d) open a new window for each plot instead of returning an @(racket image-snip%).

Users of command-line Racket, which cannot display image snips, should enter
@racketblock[(plot-new-window? #t)]
before using @(racket plot) or @(racket plot3d).
}

@deftogether[((defparam plot-width width exact-positive-integer? #:value 400)
              (defparam plot-height height exact-positive-integer? #:value 400))]{
The width and height of a plot, in logical drawing units (e.g. pixels for bitmap plots).
Used for default arguments of plotting procedures such as @racket[plot] and @racket[plot3d].
}

@defparam[plot-jpeg-quality quality (integer-in 0 100) #:value 100]{
The quality of JPEG images written by @(racket plot-file) and @(racket plot3d-file). See @(method bitmap% save-file).
}

@defparam[plot-ps/pdf-interactive? interactive? boolean? #:value #f]{
If @(racket #t), @(racket plot-file) and @(racket plot3d-file) open a dialog when writing PostScript or PDF files. See @(racket post-script-dc%) and @(racket pdf-dc%).
}

@section{General Appearance}

@deftogether[((defparam plot-title title (or/c string? #f) #:value #f)
              (defparam plot-x-label label (or/c string? #f) #:value "x axis")
              (defparam plot-y-label label (or/c string? #f) #:value "y axis")
              (defparam plot-z-label label (or/c string? #f) #:value #f))]{
Title and near axis labels.
A @(racket #f) value means the label is not drawn and takes no space.
A @(racket "") value effectively means the label is not drawn, but it takes space.
Used as default keyword arguments of plotting procedures such as @racket[plot] and @racket[plot3d].
}

@deftogether[((defparam plot-x-far-label label (or/c string? #f) #:value #f)
              (defparam plot-y-far-label label (or/c string? #f) #:value #f)
              (defparam plot-z-far-label label (or/c string? #f) #:value #f))]{
Far axis labels.
A @(racket #f) value means the label is not drawn and takes no space.
A @(racket "") value effectively means the label is not drawn, but it takes space.
See @racket[plot-x-ticks] for a discussion of near and far axes.
}

@defparam[plot3d-samples n (and/c exact-integer? (>=/c 2)) #:value 41]{
Number of samples taken of functions plotted by 3D renderers, per-axis.
Used as the default @racket[#:samples] argument of @racket[surface3d], @racket[polar3d],
@racket[isoline3d], @racket[contours3d], @racket[contour-intervals3d], @racket[isosurface3d]
and @racket[isosurfaces3d].
}

@deftogether[((defparam plot3d-angle angle real? #:value 30)
              (defparam plot3d-altitude altitude real? #:value 60))]{
The angle and altitude of the camera in rendering 3D plots, in degrees.
Used as default keyword arguments of plotting procedures such as @racket[plot3d].
}

@deftogether[((defparam plot3d-ambient-light amt (real-in 0 1) #:value 2/3)
              (defparam plot3d-diffuse-light? diffuse? boolean? #:value #t)
              (defparam plot3d-specular-light? specular? boolean? #:value #t))]{
Amount of ambient light, and whether 3D plots are rendered with diffuse and specular reflectance.
}

@deftogether[((defparam plot-foreground color plot-color/c #:value 0)
              (defparam plot-background color plot-color/c #:value 0))]{
The plot foreground and background color.
That both are @(racket 0) by default is not a mistake: for foreground colors, @(racket 0) is interpreted as black; for background colors, @(racket 0) is interpreted as white.
See @(racket ->pen-color) and @(racket ->brush-color) for details on how @(plot-name) interprets integer colors.
}

@deftogether[((defparam plot-foreground-alpha alpha (real-in 0 1) #:value 1)
              (defparam plot-background-alpha alpha (real-in 0 1) #:value 1))]{
The opacity of the background and foreground colors.
}

@deftogether[((defparam plot-font-size size (>=/c 0) #:value 11)
              (defparam plot-font-face face (or/c string? #f) #:value #f)
              (defparam plot-font-family family font-family/c #:value 'roman))]{
The font size (in drawing units), face, and family of the title, axis labels, tick labels, and other labels.
}

@deftogether[((defparam plot-legend-anchor anchor anchor/c #:value 'top-left)
              (defparam plot-legend-box-alpha alpha (real-in 0 1) #:value 2/3))]{
The placement of the legend and the opacity of its background.
}

@defparam[plot-tick-size size (>=/c 0) #:value 10]{
The length of tick lines, in drawing units.
}

@deftogether[((defparam plot-x-tick-label-anchor anchor anchor/c #:value 'top)
              (defparam plot-y-tick-label-anchor anchor anchor/c #:value 'right)
              (defparam plot-x-far-tick-label-anchor anchor anchor/c #:value 'bottom)
              (defparam plot-y-far-tick-label-anchor anchor anchor/c #:value 'left)
              (defparam plot-x-tick-label-angle angle real? #:value 0)
              (defparam plot-y-tick-label-angle angle real? #:value 0)
              (defparam plot-x-far-tick-label-angle angle real? #:value 0)
              (defparam plot-y-far-tick-label-angle angle real? #:value 0))]{
Anchor and angles for axis tick labels (2D only).
Angles are in degrees.
The anchor refers to the part of the label attached to the end of the tick line.

Set these when labels would otherwise overlap; for example, in histograms with long category names.
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-tick-label-anchor  'top-right]
                                   [plot-x-tick-label-angle   30])
                      (plot (discrete-histogram '(#(really-long-category-name-1 2)
                                                  #(long-category-name-2 1.75)
                                                  #(long-category-name-3 2.5)))))]
}

@deftogether[((defparam plot-x-axis? draw? boolean? #:value #t)
              (defparam plot-y-axis? draw? boolean? #:value #t)
              (defparam plot-z-axis? draw? boolean? #:value #t)
              (defparam plot-x-far-axis? draw? boolean? #:value #t)
              (defparam plot-y-far-axis? draw? boolean? #:value #t)
              (defparam plot-z-far-axis? draw? boolean? #:value #t))]{
When any of these is @racket[#f], the corresponding axis is not drawn.

Use these along with @racket[x-axis] and @racket[y-axis] renderers if you want axes that intersect the origin or some other point.
}

@defparam[plot-animating? animating? boolean? #:value #f]{
When @(racket #t), certain renderers draw simplified plots to speed up drawing.
@(plot-name) sets it to @(racket #t), for example, when a user is clicking and dragging a 3D plot to rotate it.
}

@defproc[(animated-samples [samples (and/c exact-integer? (>=/c 2))])
         (and/c exact-integer? (>=/c 2))]{
Given a number of samples, returns the number of samples to use.
This returns @racket[samples] when @racket[plot-animating?] is @racket[#f].
}

@defparam[plot-decorations? draw? boolean? #:value #t]{
When @(racket #f), axes, axis labels, ticks, tick labels, and the title are not drawn.
}

@section{Lines}

@defparam[line-samples n (and/c exact-integer? (>=/c 2)) #:value 500]{
The number of points to sample when approximating a line.
Used as a default keyword argument in @racket[function], @racket[inverse], @racket[parametric],
@racket[polar], @racket[density], @racket[function-interval], @racket[inverse-interval],
@racket[parametric-interval], @racket[polar-interval], @racket[area-histogram] and
@racket[parametric3d].
}

@deftogether[((defparam line-color color plot-color/c #:value 1)
              (defparam line-width width (>=/c 0) #:value 1)
              (defparam line-style style plot-pen-style/c #:value 'solid)
              (defparam line-alpha alpha (real-in 0 1) #:value 1))]{
The pen color, pen width, pen style and opacity of lines in plots.
Used as default keyword arguments of @racket[function], @racket[inverse], @racket[lines],
@racket[parametric], @racket[polar], @racket[density], @racket[isoline], @racket[lines3d],
@racket[parametric3d] and @racket[isoline3d].
}

@section{Intervals}

@deftogether[((defparam interval-color color plot-color/c #:value 3)
              (defparam interval-style style plot-brush-style/c #:value 'solid)
              (defparam interval-line1-color color plot-color/c #:value 3)
              (defparam interval-line1-width width (>=/c 0) #:value 1)
              (defparam interval-line1-style style plot-pen-style/c #:value 'solid)
              (defparam interval-line2-color color plot-color/c #:value 3)
              (defparam interval-line2-width width (>=/c 0) #:value 1)
              (defparam interval-line2-style style plot-pen-style/c #:value 'solid)
              (defparam interval-alpha alpha (real-in 0 1) #:value 3/4))]{
The brush color/style, lower line pen color/width/style, upper line pen color/width/style, and opacity of interval plots.
Used as default keyword arguments of @racket[function-interval], @racket[inverse-interval],
@racket[lines-interval], @racket[parametric-interval] and @racket[polar-interval].
}

@section{Points and Point Labels}

@deftogether[((defparam point-sym sym point-sym/c #:value 'circle)
              (defparam point-size size (>=/c 0) #:value 6)
              (defparam point-alpha alpha (real-in 0 1) #:value 1))]{
The symbol, and its size and opacity, used in point plots.
Used as default keyword arguments of @racket[points] and @racket[points3d].
}

@deftogether[((defparam point-x-jitter x-jitter (>=/c 0) #:value 0)
              (defparam point-y-jitter y-jitter (>=/c 0) #:value 0)
              (defparam point-z-jitter z-jitter (>=/c 0) #:value 0))]{
When any of @racket[x-jitter], @racket[y-jitter], or @racket[z-jitter] are non-zero,
 @racket[points] and @racket[points3d] will produce points randomly translated from their
 original position along the x, y, or z axis, respectively.
For instance, if each parameter is set to 1, then @racket[points '(0 0)] will produce a random point
 in a square of area 1 centered at @racket['(0 0)].
Likewise @racket[points3d] will make a random point within a unit cube centered at @racket['(0 0 0)].
}

@deftogether[((defparam point-color color plot-color/c #:value 0)
              (defparam point-line-width width (>=/c 0) #:value 1))]{
The color and line width of symbols used in point plots and labeled points.
Used as default keyword arguments of @racket[points] and @racket[points3d], as well as
in @racket[point-label], @racket[function-label], @racket[inverse-label],
@racket[parametric-label], @racket[polar-label] and @racket[point-label3d].
}

@deftogether[((defparam label-anchor anchor anchor/c #:value 'left)
              (defparam label-angle angle real? #:value 0)
              (defparam label-alpha alpha (real-in 0 1) #:value 1)
              (defparam label-point-size size (>=/c 0) #:value 4))]{
Point label anchor, angle, and opacity, and the size of points next to labels.
Used as default keyword arguments of @racket[point-label], @racket[function-label],
@racket[inverse-label], @racket[parametric-label], @racket[polar-label] and @racket[point-label3d].
}

@section{Vector Fields}

@deftogether[((defparam vector-field-samples n exact-positive-integer? #:value 20)
              (defparam vector-field3d-samples n exact-positive-integer? #:value 9))]{
The default number of samples @racket[vector-field] and @racket[vector-field3d] take, respectively.
}

@deftogether[((defparam vector-field-color color plot-color/c #:value 1)
              (defparam vector-field-line-width width (>=/c 0) #:value 2/3)
              (defparam vector-field-line-style style plot-pen-style/c #:value 'solid)
              (defparam vector-field-scale scale (or/c real? (one-of/c 'auto 'normalized)) #:value 'auto)
              (defparam vector-field-alpha alpha (real-in 0 1) #:value 1))]{
The default pen color, pen width, pen style, scaling factor, and opacity used by
@racket[vector-field] and @racket[vector-field3d].
}

@section{Error Bars}

@deftogether[((defparam error-bar-width width (>=/c 0) #:value 6)
              (defparam error-bar-color color plot-color/c #:value 0)
              (defparam error-bar-line-width pen-width (>=/c 0) #:value 1)
              (defparam error-bar-line-style pen-style plot-pen-style/c #:value 'solid)
              (defparam error-bar-alpha alpha (real-in 0 1) #:value 2/3))]{
The default width, pen color/width/style, and opacity used by @racket[error-bars].
}

@section{Contours and Contour Intervals}

@deftogether[(
@defproc[(default-contour-colors [zs (listof real?)])
         (listof plot-color/c)
         #:value (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
                             (length zs))]
@defproc[(default-contour-fill-colors [z-ivls (listof ivl?)])
         (listof plot-color/c)
         #:value (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
                             (length z-ivls))])]{
The default values of the parameters @racket[contour-colors] and @racket[contour-interval-colors], respectively.
}

@defparam[contour-samples n (and/c exact-integer? (>=/c 2)) #:value 51]{
The number of samples taken in 2D contour plots.
Used as a defaut keyword argument in @racket[isoline], @racket[contours] and @racket[contour-intervals].
}

@deftogether[((defparam contour-levels levels (or/c 'auto exact-positive-integer? (listof real?)) #:value 'auto)
              (defparam contour-colors colors (plot-colors/c (listof real?)) #:value default-contour-colors)
              (defparam contour-widths widths (pen-widths/c (listof real?)) #:value '(1))
              (defparam contour-styles styles (plot-pen-styles/c (listof real?)) #:value '(solid long-dash)))]{
The number, pen colors, pen widths, and pen styles of @bold{lines} in contour plots.
Used as default keyword arguments of @racket[contours], @racket[contour-intervals], @racket[contours3d],
and @racket[contour-intervals3d].
}

@defparam[contour-alphas alphas (alphas/c (listof real?)) #:value '(1)]{
The opacities of @bold{lines} in contour plots.
Used as a default keyword argument in @racket[contours] and @racket[contours3d].
}

@deftogether[((defparam contour-interval-colors colors (plot-colors/c (listof ivl?)) #:value default-contour-fill-colors)
              (defparam contour-interval-styles styles (plot-brush-styles/c (listof ivl?)) #:value '(solid))
              (defparam contour-interval-alphas alphas (alphas/c (listof ivl?)) #:value '(1)))]{
The brush colors, brush styles, and opacities of @bold{intervals} in contour plots.
Used as default keyword arguments of @racket[contour-intervals] and @racket[contour-intervals3d].
}

@section{Contour Surfaces}

@deftogether[((defparam contour-interval-line-colors colors (plot-colors/c (listof ivl?)) #:value '(0))
              (defparam contour-interval-line-widths widths (pen-widths/c (listof ivl?)) #:value '(1/3))
              (defparam contour-interval-line-styles styles (plot-pen-styles/c (listof ivl?)) #:value '(solid)))]{
The pen colors, widths, and styles of the sampling grid, where it intersects contour intervals.
Used as default keyword arguments of @racket[contour-intervals3d].
}

@section{Rectangles}

@deftogether[((defparam rectangle-color color plot-color/c #:value 3)
              (defparam rectangle-style style plot-brush-style/c #:value 'solid)
              (defparam rectangle-line-color pen-color plot-color/c #:value 3)
              (defparam rectangle-line-width pen-width (>=/c 0) #:value 1)
              (defparam rectangle3d-line-width pen-width (>=/c 0) #:value 1/3)
              (defparam rectangle-line-style pen-style plot-pen-style/c #:value 'solid)
              (defparam rectangle-alpha alpha (real-in 0 1) #:value 1))]{
The brush color/style of faces, pen color/width/style of edges, and opacity of recangles.
Used as default keyword arguments of @racket[rectangles], @racket[area-histogram],
@racket[discrete-histogram], @racket[rectangles3d] and @racket[discrete-histogram3d].

The default pen width of 3D rectangle edges is narrower for aesthetic reasons.
}

@defparam[discrete-histogram-gap gap (real-in 0 1) #:value 1/8]{
The gap between histogram bars, as a percentage of bar width.
Used as a default keyword argument of @racket[discrete-histogram], @racket[stacked-histogram],
@racket[discrete-histogram3d] and @racket[stacked-histogram3d].
}

@deftogether[((defparam discrete-histogram-skip skip (>=/c 0) #:value 1)
              (defparam discrete-histogram-invert? invert? boolean? #:value #f))]{
Distance on the @italic{x} axis between histogram bars, and whether to draw histograms horizontally.
Used as default keyword arguments of @racket[discrete-histogram] and @racket[stacked-histogram].
}

@deftogether[((defparam stacked-histogram-colors colors (plot-colors/c nat/c) #:value (λ (n) (build-list n add1)))
              (defparam stacked-histogram-styles styles (plot-brush-styles/c nat/c) #:value '(solid))
              (defparam stacked-histogram-line-colors pen-colors (plot-colors/c nat/c) #:value (stacked-histogram-colors))
              (defparam stacked-histogram-line-widths pen-widths (pen-widths/c nat/c) #:value '(1))
              (defparam stacked-histogram-line-styles pen-styles (plot-pen-styles/c nat/c) #:value '(solid))
              (defparam stacked-histogram-alphas alphas (alphas/c nat/c) #:value '(1)))]{
Stacked histogram brush colors/styles, pen colors/widths/styles, and opacities.
Used as default keyword arguments of @racket[stacked-histogram] and @racket[stacked-histogram3d].
}

@section{Non-Border Axes}

@deftogether[((defparam x-axis-ticks? ticks? boolean? #:value #t)
              (defparam y-axis-ticks? ticks? boolean? #:value #t)
              ;(defparam z-axis-ticks? ticks? boolean? #:value #t)
              (defparam x-axis-labels? labels? boolean? #:value #f)
              (defparam y-axis-labels? labels? boolean? #:value #f)
              ;(defparam z-axis-labels? labels? boolean? #:value #f)
              (defparam x-axis-far? far? boolean? #:value #f)
              (defparam y-axis-far? far? boolean? #:value #f)
              ;(defparam z-axis-far? far? boolean? #:value #f)
              (defparam x-axis-alpha alpha (real-in 0 1) #:value 1)
              (defparam y-axis-alpha alpha (real-in 0 1) #:value 1)
              ;(defparam z-axis-alpha alpha (real-in 0 1) #:value 1)
              )]{
Default values for keyword arguments of @racket[x-axis], @racket[y-axis] and @racket[axes].
}

@deftogether[((defparam polar-axes-number n exact-nonnegative-integer? #:value 12)
              (defparam polar-axes-ticks? ticks? boolean? #:value #t)
              (defparam polar-axes-labels? labels? boolean? #:value #t)
              (defparam polar-axes-alpha alpha (real-in 0 1) #:value 1/2))]{
Number of polar axes, whether radius ticks (i.e. lines) are drawn, whether labels are drawn, and opacity.
Used as default keyword arguments of @racket[polar-axes].
}

@section{Surfaces}

@deftogether[((defparam surface-color color plot-color/c #:value 0)
              (defparam surface-style style plot-brush-style/c #:value 'solid)
              (defparam surface-line-color pen-color plot-color/c #:value 0)
              (defparam surface-line-width pen-width (>=/c 0) #:value 1/3)
              (defparam surface-line-style pen-style plot-pen-style/c #:value 'solid)
              (defparam surface-alpha alpha (real-in 0 1) #:value 1))]{
Surface brush color/style, pen color/width/style of the sampling grid where it intersects the surface, and opacity.
Used as default keyword arguments of @racket[surface3d], @racket[polar3d] and @racket[isosurface3d].
}

@deftogether[(
@defproc[(default-isosurface-colors [zs (listof real?)])
         (listof plot-color/c)
         #:value (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
                             (length zs))]
@defproc[(default-isosurface-line-colors [zs (listof real?)])
         (listof plot-color/c)
         #:value (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
                             (length zs))])]{
The default values of the parameters @racket[isosurface-colors] and @racket[isosurface-line-colors], respectively.
}

@deftogether[((defparam isosurface-levels levels (or/c 'auto exact-positive-integer? (listof real?)) #:value 'auto)
              (defparam isosurface-colors colors (plot-colors/c (listof real?)) #:value default-isosurface-colors)
              (defparam isosurface-styles styles (plot-brush-styles/c (listof real?)) #:value '(solid))
              (defparam isosurface-line-colors pen-colors (plot-colors/c (listof real?)) #:value default-isosurface-line-colors)
              (defparam isosurface-line-widths pen-widths (pen-widths/c (listof real?)) #:value '(1/3))
              (defparam isosurface-line-styles pen-styles (plot-pen-styles/c (listof real?)) #:value '(solid))
              (defparam isosurface-alphas alphas (alphas/c (listof real?)) #:value '(1/2)))]{
The number, brush colors/styles, pen colors/widths/styles, grid color/widths/styles, and opacities of
nested isosurfaces.
Used as default keyword arguments of @racket[isosurfaces3d].
}
