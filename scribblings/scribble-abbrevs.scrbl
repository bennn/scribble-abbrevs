#lang scribble/manual
@require[scribble/example (for-label racket/base scribble-abbrevs scribble/core scribble/manual racket/contract)]

@(define my-eval (make-base-eval '(require scribble-abbrevs)))

@; -----------------------------------------------------------------------------
@title{Scribble Abbrevs}
@author{Ben Greenman}

@defmodule[scribble-abbrevs]{
  Helpers for writing Scribble papers --- that is, Scribble code that
   generates LaTeX output.
}

I think the functions here are useful enough to be online and documented.

@defproc[(add-commas [r real?]) string?]{
  @index["~n"]{Similar} to @racket[number->string], but adds commas to numbers with more than three digits.

  @examples[#:eval my-eval
   (add-commas 42)
   (add-commas 42000)]
}

@defidform[appendix]{
  Typesets @litchar{\appendix} in a @racket[paragraph] with the @racket['pretitle] style.
  In LaTeX, this marks the current "section" as the start of an appendix.
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

@defproc[(sf [x pre-content?]) element?]{
  Renders the given content in @racket{sfstyle} (serif-style).
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

@defproc[(parag [s string?] ...) element?]{
  Renders the given strings as a paragraph title.
}

@defproc[(definition [term string?] [def-elem* pre-content?] ...) paragraph?]{
  Renders an un-numbered definition for a technical term.

  @examples[#:eval my-eval
  @definition["rose"]{A rose is a rose is a rose is a rose.}]

  This usually looks good to me.
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

