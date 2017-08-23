#lang racket/base

(provide
  authors
  authors*
  sf
  sc
  exact
  etal
  $
  parag
  definition
  defn
  Section-ref
  section-ref
  x-axes
  y-axes
  x-axis
  y-axis
  format-url
  Integer->word
  integer->word
)

(require
  (only-in racket/list
    add-between)
  (only-in racket/format
    ~a)
  (only-in racket/string
    string-join)
  scribble-abbrevs/private/integer-word
  scribble/base
  scribble/core
  scribble/manual)

;; =============================================================================

(define (authors . a*)
  (authors* a*))

(define (authors* a*)
  (cond
   [(null? a*)
    (raise-argument-error 'authors "at least one argument" a*)]
   [(null? (cdr a*))
    (car a*)]
   [(null? (cddr a*))
    (list (car a*) " and " (cadr a*))]
   [else
    (add-between a* ", " #:before-last ", and ")]))

(define (sf x)
  (elem #:style "sfstyle" x))

(define (sc x #:sup [sup #f])
  (exact "\\textsc{\\small " x "}"
    (if sup (format "$^~a$" sup) "")))

(define (exact . items)
  (make-element (make-style "relax" '(exact-chars))
                items))

(define etal
  (exact "et~al."))

(define ($ . items)
  (apply exact (list "$" items "$")))

(define (parag . x)
  (apply elem #:style "paragraph" x))

(define (definition term . defn*)
  (make-paragraph plain
    (list
      (exact "\\vspace{1ex}\n")
      (bold "Definition")
      (cons (element #f (list " (" (emph term) ") ")) defn*)
      (exact "\\vspace{1ex}\n"))))

(define (defn term)
  term)

(define (Section-ref s)
  (elem "Section" ~ (secref s)))

(define (section-ref s)
  (elem "section" ~ (secref s)))

(define (axes q)
  (elem (emph q) "-axes"))

(define x-axes
  (axes "x"))

(define y-axes
  (axes "y"))

(define (axis q)
  (elem (emph q) "-axis"))

(define x-axis
  (axis "x"))

(define y-axis
  (axis "y"))

(define (format-url str)
  (hyperlink str
    (url
      (remove-prefix "www."
        (remove-prefix "http[^:]*://" str)))))

(define (remove-prefix rx str)
  (define m (regexp-match (string-append "^" rx "(.*)$") str))
  (if m (cadr m) str))

(define (Integer->word i)
  (integer->word i #:title? #t))

(define (integer->word i #:title? [title? #f])
  (define word* (integer->word* i))
  (define word (string-join word* "-"))
  (if title?
    (string-titlecase word)
    word))
