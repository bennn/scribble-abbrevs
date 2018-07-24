#lang racket/base

(provide
  $
  appendix
  definition
  exact
  Section-ref
  section-ref
  Sections-ref
  sections-ref
  sc)

(require
  scribble/base
  scribble/core
  scribble/manual)

;; -----------------------------------------------------------------------------

(define (sc x #:sup [sup #f])
  (exact "\\textsc{\\small " x "}"
    (if sup (format "$^~a$" sup) "")))

(define (exact . items)
  (make-element (make-style "relax" '(exact-chars))
                items))

(define appendix
  (make-paragraph (make-style 'pretitle '())
    (make-element (make-style "appendix" '(exact-chars)) '())))

(define ($ . items)
  (apply exact (list "$" items "$")))

(define (definition term . defn*)
  (make-paragraph plain
    (list
      (exact "\\vspace{1ex}\n")
      (bold "Definition")
      (cons (element #f (list " (" (emph term) ") ")) defn*)
      (exact "\\vspace{1ex}\n"))))

(define (X-ref x s)
  (elem x ~ (secref s)))

;; TODO these should work like figure-ref

(define (Section-ref s)
  (X-ref "Section" s))

(define (Sections-ref s)
  (X-ref "Sections" s))

(define (section-ref s)
  (X-ref "section" s))

(define (sections-ref s)
  (X-ref "sections" s))

