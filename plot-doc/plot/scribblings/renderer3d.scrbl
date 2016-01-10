#lang scribble/manual

@(require "common.rkt")

@title[#:tag "renderer3d"]{3D Renderers}

@declare-exporting[plot]
@defmodule*/no-declare[(plot) #:link-target? #f]

@section{3D Renderer Function Arguments}

As with functions that return 2D renderers, functions that return 3D renderers always have these kinds of arguments:
@itemlist[
          @item{Required (and possibly optional) arguments representing the graph to plot.}
          @item{Optional keyword arguments for overriding calculated bounds, with the default value @(racket #f).}
          @item{Optional keyword arguments that determine the appearance of the plot.}
          @item{The optional keyword argument @(racket #:label), which specifies the name of the renderer in the legend.}]

See @secref["renderer2d-function-arguments"] for a detailed example.

@section{3D Point Renderers}

@defproc[(points3d
          [vs  (sequence/c (sequence/c #:min-count 3 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:sym sym point-sym/c (point-sym)]
          [#:color color plot-color/c (point-color)]
          [#:fill-color fill-color (or/c plot-color/c 'auto) 'auto]
          [#:x-jitter x-jitter (>=/c 0) (point-x-jitter)]
          [#:y-jitter y-jitter (>=/c 0) (point-y-jitter)]
          [#:z-jitter z-jitter (>=/c 0) (point-z-jitter)]
          [#:size size (>=/c 0) (point-size)]
          [#:line-width line-width (>=/c 0) (point-line-width)]
          [#:alpha alpha (real-in 0 1) (point-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that draws points in 3D space.

For example, a scatter plot of points sampled uniformly from the surface of a sphere:
@interaction[#:eval plot-eval
                    (define (runif) (- (* 2 (random)) 1))
                    (define (rnormish) (+ (runif) (runif) (runif) (runif)))
                    
                    (define xs0 (build-list 1000 (λ _ (rnormish))))
                    (define ys0 (build-list 1000 (λ _ (rnormish))))
                    (define zs0 (build-list 1000 (λ _ (rnormish))))
                    (define mags (map (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z))))
                                      xs0 ys0 zs0))
                    (define xs (map / xs0 mags))
                    (define ys (map / ys0 mags))
                    (define zs (map / zs0 mags))
                    
                    (plot3d (points3d (map vector xs ys zs) #:sym 'dot)
                            #:altitude 25)]

When @racket[x-jitter], @racket[y-jitter], or @racket[z-jitter] is non-zero,
each point @racket[p] is translated to a random location inside a box centered at @racket[p] with
width @racket[x-jitter], height @racket[y-jitter], and depth @racket[z-jitter].
The new points will lie within [@racket[x-min], @racket[x-max]] etc. if these bounds are
non-@racket[#f].

Note that adding random noise to data, via jittering or otherwise, is usually a bad idea.
See the documentation for @racket[points] for examples where jittering may be appropriate.
}

@defproc[(vector-field3d
          [f (or/c (real? real? real? . -> . (sequence/c real?))
                   ((vector/c real? real? real?) . -> . (sequence/c real?)))]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [z-min (or/c rational? #f) #f] [z-max (or/c rational? #f) #f]
          [#:samples samples exact-positive-integer? (vector-field3d-samples)]
          [#:scale scale (or/c real? (one-of/c 'auto 'normalized)) (vector-field-scale)]
          [#:color color plot-color/c (vector-field-color)]
          [#:line-width line-width (>=/c 0) (vector-field-line-width)]
          [#:line-style line-style plot-pen-style/c (vector-field-line-style)]
          [#:alpha alpha (real-in 0 1) (vector-field-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that draws a vector field in 3D space.
The arguments are interpreted identically to the corresponding arguments to @racket[vector-field].
@examples[#:eval plot-eval
                 (plot3d (vector-field3d (λ (x y z) (vector x z y))
                                         -2 2 -2 2 -2 2))]
}

@section{3D Line Renderers}

@defproc[(lines3d
          [vs  (sequence/c (sequence/c #:min-count 3 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that draws connected lines.
The @racket[parametric3d] function is defined in terms of this one.
}

@defproc[(parametric3d
          [f (real? . -> . (sequence/c real?))]
          [t-min rational?] [t-max rational?]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots a vector-valued function of time. For example,
@interaction[#:eval plot-eval
                    (require (only-in plot/utils 3d-polar->3d-cartesian))
                    (plot3d (parametric3d (λ (t) (3d-polar->3d-cartesian (* t 80) t 1))
                                          (- pi) pi #:samples 3000 #:alpha 0.5)
                            #:altitude 25)]
}

@section{3D Surface Renderers}

@defproc[(surface3d
          [f (real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:color color plot-color/c (surface-color)]
          [#:style style plot-brush-style/c (surface-style)]
          [#:line-color line-color plot-color/c (surface-line-color)]
          [#:line-width line-width (>=/c 0) (surface-line-width)]
          [#:line-style line-style plot-pen-style/c (surface-line-style)]
          [#:alpha alpha (real-in 0 1) (surface-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots a two-input, one-output function. For example,
@interaction[#:eval plot-eval (plot3d (list (surface3d (λ (x y) (+ (sqr x) (sqr y))) -1 1 -1 1
                                                       #:label "z = x^2 + y^2")
                                            (surface3d (λ (x y) (- (+ (sqr x) (sqr y)))) -1 1 -1 1
                                                       #:color 4 #:line-color 4
                                                       #:label "z = -x^2 - y^2")))]
}

@defproc[(polar3d
          [f (real? real? . -> . real?)]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:color color plot-color/c (surface-color)]
          [#:style style plot-brush-style/c (surface-style)]
          [#:line-color line-color plot-color/c (surface-line-color)]
          [#:line-width line-width (>=/c 0) (surface-line-width)]
          [#:line-style line-style plot-pen-style/c (surface-line-style)]
          [#:alpha alpha (real-in 0 1) (surface-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots a function from latitude and longitude to radius.

Currently, latitudes range from @(racket 0) to @(racket (* 2 pi)), and longitudes from @(racket (* -1/2 pi)) to @(racket (* 1/2 pi)).
These intervals may become optional arguments to @racket[polar3d] in the future.

A sphere is the graph of a polar function of constant radius:
@interaction[#:eval plot-eval (plot3d (polar3d (λ (θ ρ) 1)) #:altitude 25)]

Combining polar function renderers allows faking latitudes or longitudes in larger ranges, to get, for example, a seashell plot:
@interaction[#:eval plot-eval
                    (parameterize ([plot-decorations?  #f]
                                   [plot3d-samples     75])
                      (define (f1 θ ρ) (+ 1 (/ θ 2 pi) (* 1/8 (sin (* 8 ρ)))))
                      (define (f2 θ ρ) (+ 0 (/ θ 2 pi) (* 1/8 (sin (* 8 ρ)))))
                      
                      (plot3d (list (polar3d f1 #:color "navajowhite"
                                             #:line-style 'transparent #:alpha 2/3)
                                    (polar3d f2 #:color "navajowhite"
                                             #:line-style 'transparent #:alpha 2/3))))]
}

@section{3D Contour (Isoline) Renderers}

@defproc[(isoline3d
          [f (real? real? . -> . real?)] [z real?]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots a single contour line on the surface of a function.

The appearance keyword arguments are interpreted identically to the appearance keyword arguments to @(racket isoline).

This function is not terribly useful by itself, but can be when combined with others:
@interaction[#:eval plot-eval
                    (define (saddle x y) (- (sqr x) (sqr y)))
                    (plot3d (list (surface3d saddle -1 1 -1 1)
                                  (isoline3d saddle 1/4 #:width 2 #:style 'long-dash)))]
}

@defproc[(contours3d
          [f (real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:widths widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:styles styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof real?)) (contour-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots contour lines on the surface of a function.

The appearance keyword arguments are interpreted identically to the appearance keyword arguments to @(racket contours).
In particular, when @(racket levels) is @(racket 'auto), contour values correspond precisely to @italic{z} axis ticks.

For example,
@interaction[#:eval plot-eval (plot3d (contours3d (λ (x y) (+ (sqr x) (sqr y))) -1.1 1.1 -1.1 1.1
                                                  #:label "z = x^2 + y^2"))]
}

@defproc[(contour-intervals3d
          [f (real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:colors colors (plot-colors/c (listof ivl?)) (contour-interval-colors)]
          [#:styles styles (plot-brush-styles/c (listof ivl?)) (contour-interval-styles)]
          [#:line-colors line-colors (plot-colors/c (listof ivl?)) (contour-interval-line-colors)]
          [#:line-widths line-widths (pen-widths/c (listof ivl?)) (contour-interval-line-widths)]
          [#:line-styles line-styles (plot-pen-styles/c (listof ivl?)) (contour-interval-line-styles)]
          [#:contour-colors contour-colors (plot-colors/c (listof real?)) (contour-colors)]
          [#:contour-widths contour-widths (pen-widths/c (listof real?)) (contour-widths)]
          [#:contour-styles contour-styles (plot-pen-styles/c (listof real?)) (contour-styles)]
          [#:alphas alphas (alphas/c (listof ivl?)) (contour-interval-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots contour intervals and contour lines on the surface of a function.
The appearance keyword arguments are interpreted identically to the appearance keyword arguments to @(racket contour-intervals).

For example,
@interaction[#:eval plot-eval (plot3d (contour-intervals3d (λ (x y) (+ (sqr x) (sqr y)))
                                                           -1.1 1.1 -1.1 1.1
                                                           #:label "z = x^2 + y^2"))]
}

@section{3D Isosurface Renderers}

@defproc[(isosurface3d
          [f (real? real? real? . -> . real?)] [d rational?]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [z-min (or/c rational? #f) #f] [z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:color color plot-color/c (surface-color)]
          [#:style style plot-brush-style/c (surface-style)]
          [#:line-color line-color plot-color/c (surface-line-color)]
          [#:line-width line-width (>=/c 0) (surface-line-width)]
          [#:line-style line-style plot-pen-style/c (surface-line-style)]
          [#:alpha alpha (real-in 0 1) (surface-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots the surface of constant output value of the function @(racket f). The argument @(racket d) is the constant value.

For example, a sphere is all the points in which the Euclidean distance function returns the sphere's radius:                                                           
@interaction[#:eval plot-eval (plot3d (isosurface3d
                                       (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z)))) 1
                                       -1 1 -1 1 -1 1)
                                      #:altitude 25)]
}

@defproc[(isosurfaces3d
          [f (real? real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [z-min (or/c rational? #f) #f] [z-max (or/c rational? #f) #f]
          [#:d-min d-min (or/c rational? #f) #f] [#:d-max d-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (isosurface-levels)]
          [#:colors colors (plot-colors/c (listof real?)) (isosurface-colors)]
          [#:styles styles (plot-brush-styles/c (listof real?)) (isosurface-styles)]
          [#:line-colors line-colors (plot-colors/c (listof real?)) (isosurface-line-colors)]
          [#:line-widths line-widths (pen-widths/c (listof real?)) (isosurface-line-widths)]
          [#:line-styles line-styles (plot-pen-styles/c (listof real?)) (isosurface-line-styles)]
          [#:alphas alphas (alphas/c (listof real?)) (isosurface-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that plots multiple isosurfaces. The appearance keyword arguments are interpreted similarly to those of @(racket contours).

Use this to visualize functions from three inputs to one output; for example:
@interaction[#:eval plot-eval
                    (define (saddle x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z)))))
                    (plot3d (isosurfaces3d saddle #:d-min -1 #:d-max 1 #:label "")
                            #:x-min -2 #:x-max 2
                            #:y-min -2 #:y-max 2
                            #:z-min -2 #:z-max 2)]

If it helps, think of the output of @(racket f) as a density or charge.
}

@section{3D Rectangle Renderers}

@defproc[(rectangles3d
          [rects  (sequence/c (sequence/c #:min-count 3 ivl?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle3d-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?]{
Returns a renderer that draws rectangles.

This can be used to draw histograms; for example,
@interaction[#:eval plot-eval
                    (require (only-in plot/utils bounds->intervals linear-seq))
                    
                    (define (norm2 x y) (exp (* -1/2 (+ (sqr (- x 5)) (sqr y)))))
                    (define x-ivls (bounds->intervals (linear-seq 2 8 16)))
                    (define y-ivls (bounds->intervals (linear-seq -5 5 16)))
                    (define x-mids (linear-seq 2 8 15 #:start? #f #:end? #f))
                    (define y-mids (linear-seq -5 5 15 #:start? #f #:end? #f))
                    
                    (plot3d (rectangles3d (append*
                                           (for/list ([y-ivl  (in-list y-ivls)]
                                                      [y  (in-list y-mids)])
                                             (for/list ([x-ivl  (in-list x-ivls)]
                                                        [x  (in-list x-mids)])
                                               (define z (norm2 x y))
                                               (vector x-ivl y-ivl (ivl 0 z)))))
                                          #:alpha 3/4
                                          #:label "Appx. 2D Normal"))]
}

@defproc[(discrete-histogram3d
          [cat-vals (sequence/c (or/c (vector/c any/c any/c (or/c real? ivl? #f))
                                      (list/c any/c any/c (or/c real? ivl? #f))))]
          [#:x-min x-min (or/c rational? #f) 0] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) 0] [#:z-max z-max (or/c rational? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle3d-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          [#:add-x-ticks? add-x-ticks? boolean? #t]
          [#:add-y-ticks? add-y-ticks? boolean? #t]
          [#:x-far-ticks? x-far-ticks? boolean? #f]
          [#:y-far-ticks? y-far-ticks? boolean? #f]
          ) renderer3d?]{
Returns a renderer that draws discrete histograms on a two-valued domain.

Missing pairs are not drawn; for example,
@interaction[#:eval plot-eval
                    (plot3d (discrete-histogram3d '(#(a a 1) #(a b 2) #(b b 3))
                                                  #:label "Missing (b,a)"
                                                  #:color 4 #:line-color 4))]
}

@defproc[(stacked-histogram3d
          [cat-vals (sequence/c (or/c (vector/c any/c any/c (sequence/c real?))
                                      (list/c any/c any/c (sequence/c real?))))]
          [#:x-min x-min (or/c rational? #f) 0] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) 0] [#:z-max z-max (or/c rational? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:colors colors (plot-colors/c nat/c) (stacked-histogram-colors)]
          [#:styles styles (plot-brush-styles/c nat/c) (stacked-histogram-styles)]
          [#:line-colors line-colors (plot-colors/c nat/c) (stacked-histogram-line-colors)]
          [#:line-widths line-widths (pen-widths/c nat/c) (stacked-histogram-line-widths)]
          [#:line-styles line-styles (plot-pen-styles/c nat/c) (stacked-histogram-line-styles)]
          [#:alphas alphas (alphas/c nat/c) (stacked-histogram-alphas)]
          [#:labels labels (labels/c nat/c) '(#f)]
          [#:add-x-ticks? add-x-ticks? boolean? #t]
          [#:add-y-ticks? add-y-ticks? boolean? #t]
          [#:x-far-ticks? x-far-ticks? boolean? #f]
          [#:y-far-ticks? y-far-ticks? boolean? #f]
          ) (listof renderer3d?)]{
Returns a renderer that draws a stacked histogram.
Think of it as a version of @racket[discrete-histogram] that allows multiple values to be specified for each pair of categories.
@examples[#:eval plot-eval
                 (define data '(#(a a (1 1 1)) #(a b (1.5 3)) #(b b ()) #(b a (1/2))))
                 (plot3d (stacked-histogram3d data #:labels '("Red" #f "Blue")
                                              #:alphas '(2/3 1 2/3)))]
}

@defproc[(point-label3d
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
          ) renderer3d?]{
Returns a renderer that draws a labeled point.
If @(racket label) is @(racket #f), the point is labeled with its position.
Analogous to @racket[point-label].
}
