#lang racket/base

(require racket/contract)
(provide
  (contract-out
    [roman-symbol?
      (-> any/c boolean?)]
    [natural->roman-symbol*
      (-> exact-positive-integer? (and/c (not/c null?) (listof roman-symbol?)))]
    [roman-numeral->natural
      (-> (or/c string? (listof roman-symbol?)) exact-nonnegative-integer?)]))

(require
  (only-in racket/list make-list)
  racket/math)

;; -----------------------------------------------------------------------------

(define roman-symbol*
  (vector-immutable 'I 'V 'X 'L 'C 'D 'M))

(define (roman-symbol? x)
  (and (symbol? x)
       (for/or ((s (in-vector roman-symbol*)))
         (eq? x s))))

(define (roman-index->natural i)
  (cond
    [(zero? i)
     1]
    [else
     (define k (- (exact-ceiling (/ i 2)) 1))
     (* (expt 10 k) (if (even? i) 10 5))]))

(define (roman-symbol->natural s)
  (define i
    (for/first ((k (in-range (vector-length roman-symbol*)))
                #:when (eq? s (vector-ref roman-symbol* k)))
      k))
  (unless i
    (raise-argument-error 'roman-symbol->natural "roman-symbol?" s))
  (roman-index->natural i))

(define max-roman-natural
  (roman-index->natural (- (vector-length roman-symbol*) 1)))

(define (natural->roman-symbol* n)
  (let loop ([i (- (vector-length roman-symbol*) 1)]
             [n n]
             [acc '()])
    (define curr-sym (vector-ref roman-symbol* i))
    (define curr-val (roman-index->natural i))
    (cond
      [(<= curr-val n)
       (define-values [first-n rest-n] (quotient/remainder n curr-val))
       (define prefix* (make-list first-n curr-sym))
       (loop i rest-n (append prefix* acc))]
      [(and (< 0 i) (<= (get-subtract-val curr-sym) n))
       (define-values [sub-val sub-sym] (get-subtract curr-sym))
       (loop (- i 1) (- n sub-val) (list* curr-sym sub-sym acc))]
      [(zero? i)
       (reverse acc)]
      [else
       (loop (- i 1) n acc)])))

(define (get-subtract-val n)
  (define-values [val _] (get-subtract n))
  val)

(define (get-subtract-sym n)
  (define-values [_ sym] (get-subtract n))
  sym)

(define (get-subtract n)
  (case n
    ((M) (values 900 'C))
    ((D) (values 400 'C))
    ((C) (values 90 'X))
    ((L) (values 40 'X))
    ((X) (values 9 'I))
    ((V) (values 4 'I))
    (else #f)))

(define (roman-numeral->natural x)
  (cond
    [((listof roman-symbol?) x)
     (roman-symbol*->natural x)]
    [(and (string? x) (string->roman-symbol* x))
     => roman-symbol*->natural]
    [else
     (raise-argument-error 'roman-numeral->natural "roman numeral?" x)]))

(define (string->roman-symbol* pre-str)
  (define str (string-upcase pre-str))
  (define sym*
    (for/list ((c (in-string str)))
      (char->roman-symbol c)))
  (cond
    [(andmap values sym*)
     sym*]
    [(or (string=? str "NULLA") (string=? str "N"))
     '()]
    [else
     #f]))

(define (char->roman-symbol c)
  (define sym (string->symbol (string c)))
  (and (roman-symbol? sym) sym))

(define (roman-symbol*->natural r*)
  (for/fold ((acc 0)
             (prev #f)
             #:result (+ acc (or prev 0)))
            ((r (in-list r*)))
    (define n (roman-symbol->natural r))
    (if prev
      (if (< prev n)
        (values (+ acc (- n prev)) #f)
        (values (+ acc prev) n))
      (values acc n))))

;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  (test-case "roman-symbol->natural"
    (check-equal? (roman-symbol->natural 'I) 1)
    (check-equal? (roman-symbol->natural 'V) 5)
    (check-equal? (roman-symbol->natural 'X) 10)
    (check-equal? (roman-symbol->natural 'L) 50)
    (check-equal? (roman-symbol->natural 'C) 100)
    (check-equal? (roman-symbol->natural 'D) 500)
    (check-equal? (roman-symbol->natural 'M) 1000))

  (test-case "natural->roman-symbol*:doc"
    (check-equal? (natural->roman-symbol* 0) '())
    (check-equal? (natural->roman-symbol* 2) '(I I))
    (check-equal? (natural->roman-symbol* 4) '(I V))
    (check-equal? (natural->roman-symbol* 54) '(L I V))
    (check-equal? (natural->roman-symbol* 101) '(C I)))

  (test-case "natural->roman-symbol*:wikipedia"
    (check-equal? (natural->roman-symbol* 39) '(X X X I X))
    (check-equal? (natural->roman-symbol* 246) '(C C X L V I))
    (check-equal? (natural->roman-symbol* 789) '(D C C L X X X I X))
    (check-equal? (natural->roman-symbol* 2421) '(M M C D X X I))
    (check-equal? (natural->roman-symbol* 160) '(C L X))
    (check-equal? (natural->roman-symbol* 207) '(C C V I I))
    (check-equal? (natural->roman-symbol* 1009) '(M I X))
    (check-equal? (natural->roman-symbol* 1066) '(M L X V I))
    (check-equal? (natural->roman-symbol* 1776) '(M D C C L X X V I))
    (check-equal? (natural->roman-symbol* 1954) '(M C M L I V))
    (check-equal? (natural->roman-symbol* 2014) '(M M X I V))
    (check-equal? (natural->roman-symbol* 2020) '(M M X X)))

  (test-case "natural->roman-symbol*:misc"
    (check-equal? (natural->roman-symbol* 1555) '(M D L V))
    (check-equal? (natural->roman-symbol* 2000) '(M M))
    (check-equal? (natural->roman-symbol* 1900) '(M C M))
    (check-equal? (natural->roman-symbol* 1950) '(M C M L))
    (check-equal? (natural->roman-symbol* 1500) '(M D)))

  (test-case "roman-numeral->natural"
    (check-equal? (roman-numeral->natural "nulla") 0)
    (check-equal? (roman-numeral->natural "V") 5)
    (check-equal? (roman-numeral->natural '(V)) 5)
    (check-equal? (roman-numeral->natural "IIII") 4)
    (check-equal? (roman-numeral->natural "XLIX") 49)
    (check-equal? (roman-numeral->natural '()) 0)
    (check-equal? (roman-numeral->natural '(I I)) 2)
    (check-equal? (roman-numeral->natural '(I V)) 4)
    (check-equal? (roman-numeral->natural '(L I V)) 54)
    (check-equal? (roman-numeral->natural '(C I)) 101)
    (check-equal? (roman-numeral->natural '(X X X I X)) 39)
    (check-equal? (roman-numeral->natural '(C C X L V I)) 246)
    (check-equal? (roman-numeral->natural '(D C C L X X X I X)) 789)
    (check-equal? (roman-numeral->natural '(M M C D X X I)) 2421)
    (check-equal? (roman-numeral->natural '(C L X)) 160)
    (check-equal? (roman-numeral->natural '(C C V I I)) 207)
    (check-equal? (roman-numeral->natural '(M I X)) 1009)
    (check-equal? (roman-numeral->natural '(M L X V I)) 1066)
    (check-equal? (roman-numeral->natural '(M D C C L X X V I)) 1776)
    (check-equal? (roman-numeral->natural '(M C M L I V)) 1954)
    (check-equal? (roman-numeral->natural '(M M X I V)) 2014)
    (check-equal? (roman-numeral->natural '(M M X X)) 2020))

)
