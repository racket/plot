#lang scribble/manual

@title[#:tag "typed-compat"]{Legacy Typed Interface}

@defmodule*/no-declare[(plot/typed)]
@defmodule*/no-declare[(plot/typed/utils plot/typed/no-gui plot/typed/bitmap plot/typed/pict)]

@bold{Do not use these modules in new programs.}
They are likely to disappear in a (distant) future release.
Use @racketmodname[plot], @racketmodname[plot/utils], @racketmodname[plot/no-gui], @racketmodname[plot/bitmap] and @racketmodname[plot/pict] instead.

Plot versions 6.1.1 and earlier were written in untyped Racket, and exposed a typed interface through these modules.
Now that Plot is written in Typed Racket, a separate typed interface is no longer necessary.
However, the above modules are still available for backwards compatibility.
