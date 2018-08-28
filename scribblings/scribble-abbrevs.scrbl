#lang scribble/manual
@require[
  scribble/example
  (for-label
    racket/base
    scribble-abbrevs
    scribble/core
    scribble/manual
    racket/contract
    (only-in pict pict?)
    (only-in pict/code codeblock-pict))]

@(define my-eval (make-base-eval '(require scribble-abbrevs)))

@; -----------------------------------------------------------------------------
@title{Scribble Abbrevs}
@author{Ben Greenman}

@defmodule[scribble-abbrevs]{
  Helpers for making Scribble documents.
  The @racketmodname[scribble-abbrevs] module provides all the bindings documented on this page.
}


@section{General Scribble Utilities}

@defmodule[scribble-abbrevs/scribble]{
  General utilities, for any Scribble document.
}

@defproc[(add-commas [r real?]) string?]{
  @index["~n"]{Similar} to @racket[number->string], but adds commas to numbers with more than three digits.

  @examples[#:eval my-eval
   (add-commas 42)
   (add-commas 42000)]
}

@defproc[(authors [s string?] ...) (or/c string? element?)]{
  Renders a sequence of author names (with Oxford comma).

  @examples[#:eval my-eval
    @authors{Charles Darwin}
    @authors["Thomas Jefferson" "Alexander Hamilton"]
    @authors["Ed" "Ed" "Eddy"]]
}

@deftogether[(
  @defproc[(authors* [s* (listof string?)]) element?]
  @defproc[(oxfordize [s* (listof string?)]) element?])]{
  Same as @racket[(apply authors s*)].
}
@margin-note{Always use the Oxford comma. Remember
@hyperlink["https://www.nytimes.com/2017/03/16/us/oxford-comma-lawsuit.html"]{the
Maine truck drivers}!
(@hyperlink["https://www.nytimes.com/2018/02/09/us/oxford-comma-maine.html"]{settlement})}

@defproc[(sf [x pre-content?]) element?]{
  Renders the given content in @racket{sfstyle} (serif-style).
}

@defproc[(parag [s string?] ...) element?]{
  Renders the given strings as a paragraph title.
}

@defproc[(format-url [url string?]) element?]{
  Renders a clickable URL.
  Strips prefixes like @racket{www.} and @racket{http://}.
}

@defproc[(integer->word [i exact-nonnegative-integer?] [#:title? title? boolean? #false]) string?]{
  Returns the English word for the given integer.

  @examples[#:eval my-eval
   @integer->word[0]
   @integer->word[42]
   @integer->word[-8675309]
  ]

  The current implementation fails unless @racket[(abs i)] is less than 1 quadrillion.
}

@defproc[(Integer->word [i exact-nonnegative-integer?]) string?]{
  Alias for @racket[(integer->word i #:title? #true)].
}

@; -----------------------------------------------------------------------------

@deftogether[(
@defidform[x-axes]
@defidform[y-axes]
@defidform[x-axis]
@defidform[y-axis]
@defidform[etal])]{
  Each identifier @racket[_id] renders like the string @racket{id}, except
   that it might be prettier (avoid bad line breaks, bad spacing).
}


@; -----------------------------------------------------------------------------
@section{LaTeX Renderer Utilities}

@defmodule[scribble-abbrevs/latex]{
  Utilities for Scribble code that generates LaTeX output.
}

@defidform[appendix]{
  Typesets @litchar{\appendix} in a @racket[paragraph] with the @racket['pretitle] style.
  In LaTeX, this marks the current "section" as the start of an appendix.
}

@defproc[(sc [x pre-content?]) element?]{
  Renders the given content in small caps style.
}

@defproc[(exact [s string?] ...) element?]{
  Renders the given strings as-is in the output.

  @examples[#:eval my-eval
  @exact|{\frac{2}{3}}|]
}

@defproc[($ [s string?] ...) element?]{
  Same as @racket[(exact "$" s ... "$")].
}

@defproc[(definition [term string?] [def-elem* pre-content?] ...) paragraph?]{
  Renders an un-numbered definition for a technical term.

  @examples[#:eval my-eval
  @definition["rose"]{A rose is a rose is a rose is a rose.}]

  This usually looks good to me.
}

@deftogether[(
  @defproc[(Section-ref [tag string?]) element?]
  @defproc[(section-ref [tag string?]) element?])]{

  @deprecated[#:what "function" @racket[secref]]

  Renders the section number for @racket[tag] prefixed with the word @racket{Section}
   (respectively, @racket{section}).

  These functions assume that the following LaTeX command appears somewhere
   between the definition of Scribble's @tt{SecRef} (see @secref["builtin-latex" #:doc '(lib "scribblings/scribble/scribble.scrbl")])
   and the first occurrence of @racket[section-ref]:

  @verbatim{
    \renewcommand{\SecRef}[2]{#1}
  }
}


@; -----------------------------------------------------------------------------
@section{Documentation Renderer Utilities}

@defmodule[scribble-abbrevs/manual]{
  Utilities for Scribble code that generates Racket documentation.
}

@defproc[(tech/guide [pre-content pre-content?] ...) element?]{
  Similar to @racket[tech], but links to @other-manual['(lib "scribblings/guide/guide.scrbl")].
}

@defproc[(tech/reference [pre-content pre-content?] ...) element?]{
  Similar to @racket[tech], but links to @other-manual['(lib "scribblings/reference/reference.scrbl")].
}

@defproc[(racketfile [filename path-string?]) element?]{
  Typesets the contents of the given file as if its contents were wrapped in a @racket[racketblock].
}


@; -----------------------------------------------------------------------------
@section{Pict Utilities}

@defmodule[scribble-abbrevs/pict]{
}

@;@defform[(lined-racketblock datum ...)]{
@;  Similar to @racket[racketblock], but with line numbers.
@;}

@defproc[(codeblock-pict/numbers [code string?]
                                 [#:title title #f]
                                 [#:start start 1]
                                 [#:keep-lang-line? keep-lang #t]) pict?]{
  Similar to @racket[codeblock], but with line numbers.
}

@defproc[(add-border [p pict?]
                     [#:margin-top margin-top 2]
                     [#:margin-bottom margin-bottom 2]
                     [#:margin-left margin-left 2]
                     [#:margin-right margin-right 2]) pict?]{
  Adds a thin black border around the given pict.
}

