#lang racket/base

(provide
  racketfile
  tech/guide
  tech/reference)

(require
  scribble/manual
  (for-syntax
    racket/base
    syntax/parse
    (only-in syntax/location syntax-source-directory)
    (only-in racket/file file->list)))

;; -----------------------------------------------------------------------------

(define-syntax (racketfile stx)
  (syntax-parse stx
   [(_ file-name:str)
    #:with (str* ...)
           (file->list (let* ([fn (syntax-e #'file-name)]
                              [dir (syntax-source-directory stx)])
                         (cond
                          [(complete-path? fn)
                           fn]
                          [dir
                           (build-path dir fn)]
                          [else
                           (raise-argument-error 'racketfile "cannot find source for '~a'" fn)]))
                       (lambda (p)
                         (let ([v (read-line p)])
                           (if (eof-object? v) v (string-append v "\n")))))
    (with-syntax ((ctx (syntax/loc stx #'file-name)))
      (syntax/loc stx
        (typeset-code
          #:context ctx
          (quote str*) ...)))]))

(define (tech/guide . text)
  (keyword-apply tech '(#:doc) '((lib "scribblings/guide/guide.scrbl")) text))

(define (tech/reference . text)
  (keyword-apply tech '(#:doc) '((lib "scribblings/reference/reference.scrbl")) text))
