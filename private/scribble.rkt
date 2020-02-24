#lang racket/base

(provide
  add-commas
  authors
  authors*
  oxfordize
  defn
  parag
  etal
  sf
  x-axes
  y-axes
  x-axis
  y-axis
  format-url
  Integer->word
  integer->word
  natural->roman-numeral
  natural->roman-symbol*
  roman-symbol?
)

(require
  (only-in racket/list
    add-between)
  (only-in racket/format
    ~a)
  (only-in racket/string
    string-join
    string-split)
  scribble-abbrevs/private/integer-word
  scribble-abbrevs/private/roman-numeral
  scribble/base
  scribble/core
  scribble/manual)

;; =============================================================================

(define (add-commas n)
  (unless (real? n)
    (raise-argument-error 'add-commas "real?" n))
  (define str (number->string n))
  (define str* (string-split str "."))
  (string-append (add-commas/integer (car str*))
                 (if (or (null? (cdr str*)) (< (string-length str) 4))
                   ""
                   (string-append "." (cadr str*)))))

(define (add-commas/integer str)
  (define L (string-length str))
  (string-join
    (let loop ([i L]
               [acc '()])
      (let ([i-3 (- i 3)])
        (cond
         [(<= i-3 0)
          (cons (substring str 0 i) acc)]
         [else
          (loop i-3 (cons "," (cons (substring str i-3 i) acc)))]))) ""))

(define (authors . a*)
  (authors* a*))

(define (authors* a*)
  (oxfordize a*))

(define (oxfordize a*)
  (cond
   [(null? a*)
    (raise-argument-error 'authors "at least one argument" a*)]
   [(null? (cdr a*))
    (car a*)]
   [(null? (cddr a*))
    (list (car a*) " and " (cadr a*))]
   [else
    (add-between a* ", " #:before-last ", and ")]))

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

(define (natural->roman-numeral n)
  (define r* (natural->roman-symbol* n))
  (apply string-append (map symbol->string r*)))

(define (defn term)
  term)

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

(define (parag . x)
  (apply elem #:style "paragraph" x))

(define (format-url str)
  (hyperlink str
    (make-element 'url
      (remove-prefix "www."
        (remove-prefix "http[^:]*://" str)))))

(define (sf x)
  (elem #:style "sfstyle" x))

(define etal
  (elem "et" ~ "al."))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs)

  (test-case "add-commas"
    (check-apply* add-commas
     [1
      => "1"]
     [10
      => "10"]
     [100
      => "100"]
     [1000
      => "1,000"]
     [999999
      => "999,999"]
     [12
      => "12"]
     [1234.56789
      => "1,234.56789"]
     [123456789
      => "123,456,789"]
     [12456789
      => "12,456,789"]
     (0 => "0")
     (1 => "1")
     (12 => "12")
     (123 => "123")
     (1234 => "1,234")
     (12345 => "12,345")
     (123456 => "123,456")
     (1234567 => "1,234,567")
     (12345678 => "12,345,678")
     ))
)
