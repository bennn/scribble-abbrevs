#lang info
(define collection "scribble-abbrevs")
(define deps '("base" "scribble-lib" "reprovide-lang-lib" "pict-lib" "draw-lib"))
(define build-deps '("scribble-lib" "scribble-doc" "racket-doc" "rackunit-abbrevs" "rackunit-lib" "rackunit-abbrevs" "pict-doc"))
(define pkg-desc "Scribble helpers")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/scribble-abbrevs.scrbl" () (tool-library))))
