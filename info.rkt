#lang info
(define collection "scribble-abbrevs")
(define deps '("base" "scribble-lib" "reprovide-lang"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "rackunit-abbrevs"))
(define pkg-desc "Scribble helpers")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("docs/scribble-abbrevs.scrbl" () (tool-library))))
