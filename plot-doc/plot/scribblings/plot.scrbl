#lang scribble/manual

@(require "common.rkt")

@title[#:tag "top"]{Plot: Graph Plotting}
@author{@(author+email "Neil Toronto" (author-email))}

@defmodule[plot]

The Plot library provides a flexible interface for producing nearly any kind of plot.
It includes many common kinds of plots already, such as scatter plots, line plots, contour plots, histograms, and 3D surfaces and isosurfaces.
Thanks to Racket's excellent multiple-backend drawing library, Plot can render plots as interactive snips in DrRacket, as picts in slideshows, as PNG, PDF, PS and SVG files, or on any device context.

For plotting without a GUI, see @racketmodname[plot/no-gui].
For plotting in REPL-like environments outside of DrRacket, including Scribble manuals, see @racketmodname[plot/pict] and @racketmodname[plot/bitmap].

@table-of-contents[]

@include-section["intro.scrbl"]

@include-section["plotting.scrbl"]

@include-section["renderer2d.scrbl"]

@include-section["renderer3d.scrbl"]

@include-section["nonrenderer.scrbl"]

@include-section["ticks.scrbl"]

@include-section["utils.scrbl"]

@include-section["params.scrbl"]

@include-section["contracts.scrbl"]

@include-section["porting.scrbl"]

@include-section["compat.scrbl"]

@close-plot-eval[]

@; Needs a timeout for testing:
@(module* test racket/base
   (require (submod ".."))
   (module config info
     (define timeout 180)))
