#lang racket/base

(require racket/contract)
(provide
  lined-racketblock
  ;; @lined-racketblock[CODE ...]
  ;; Like `racketblock`, but adds line numbers

  (contract-out
    [add-border
     (->* [pict?] [#:margin-top real?
                  #:margin-bottom real?
                  #:margin-left real?
                  #:margin-right real?]
          pict?)]
    ;; Draw a frame around the given pict.
    ;; The default for each margin is 2.

    [codeblock-pict/numbers
      (->* [string?] [#:title (or/c #f string?)
                      #:start exact-nonnegative-integer?
                      #:keep-lang-line? boolean?]
           pict?)]
    ;; Like `codeblock-pict`, but adds line numbers to the right of the code.
    ;; - `#:title` puts a label at the top-right of the block (if supplied)
    ;; - `#:start` the first line number
    ;; - `#:keep-lang-line?` if `#false`, do not render the first line of code
))

(require racket/string
         (only-in scribble/manual
                  racketblock0
                  RACKETBLOCK0)
         racket/draw
         pict
         pict/code
         (for-syntax syntax/parse
                     syntax/srcloc
                     racket/math
                     racket/base
                     racket/list))

(define (codeblock-pict/numbers
         code
         #:title [title #f]
         #:start [start 1]
         #:keep-lang-line? [keep-lang #t])
  (parameterize ((current-code-font "inconsolata"))
    (define line-count
      (length (string-split code "\n" #:trim? #f #:repeat? #f)))
    (define prog (codeblock-pict code #:keep-lang-line? keep-lang))
    (define lang-line-offset (if keep-lang 0 -1))
    (define code-pict
      (ht-append 5
       (numbers start (+ start line-count lang-line-offset))
       prog))
    (if title
      (vr-append 4
                 (add-border ((current-code-tt) title))
                 code-pict)
      code-pict)))

(define (numbers start end)
  (apply vr-append
         (blank (/ (current-code-line-sep) 1.5))
         (for/list ([i (in-range start end)])
           (define the-num ((current-code-tt) (number->string i)))
           (cbl-superimpose
            (ghost the-num)
            (scale the-num 0.8)))))

(define (add-border p
                    #:margin-top [margin-top 2]
                    #:margin-bottom [margin-bottom 2]
                    #:margin-left [margin-left 2]
                    #:margin-right [margin-right 2])
  (frame
    (vl-append (blank 0 margin-top)
               (ht-append (blank margin-left 0) p (blank margin-right 0))
               (blank 0 margin-bottom))))

(define-syntax (lined-racketblock0 stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:escape e-id:id) #:defaults ([e-id #'unsyntax])) . body)
     (define lines (collect-lines #'body))
     (define first (apply min lines))
     (define last (apply max lines))
     (define length (add1 (- last first)))
     (define digits (add1 (exact-floor (/ (log length) (log 10)))))
     #`(tabular
        (list
         (list
          " "
          " "
          " "
          (RACKETBLOCK0
           #,(update-source-location
              #'(UNSYNTAX (hspace 1))
              ;#'(UNSYNTAX (verbatim "~"))
              #:line 1
              #:column 0)
           #,@(for/list ([i (in-range length)])
                (define i-digits (add1 (exact-floor (/ (log (add1 i)) (log 10)))))
                (define spaces (- digits i-digits))
                (update-source-location
                 #`(UNSYNTAX (smaller (tt #,(number->string (add1 i)))))
                 #:line (add1 i)
                 #:column (- digits i-digits))))
          " "
          " "
          " "
          " "
          (racketblock0 #:escape e-id . body))))]))

(define-syntax (lined-racketblock stx)
  (syntax-parse stx
    [(_ . body)
     #'(nested #:style 'inset (lined-racketblock0 . body))]))

(define-for-syntax (collect-lines stx)
  (define stx* (if (syntax? stx) (syntax-e stx) stx))
  (cond
    [(or (list? stx) (vector? stx) (hash? stx))
     (flatten (for/list ([s stx*])
                (collect-lines s)))]
    [(pair? stx*)
     (append (collect-lines (car stx*))
             (collect-lines (cdr stx*)))]
    [else
     (list (syntax-line stx))]))
