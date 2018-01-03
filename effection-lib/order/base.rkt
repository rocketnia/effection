#lang parendown racket/base

(require #/only-in racket/contract/base
  -> ->i and/c any/c chaperone-contract? contract? contract-projection
  flat-contract? struct/c)
(require #/only-in racket/contract/combinator
  contract-first-order-passes? make-contract)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define-generics)

(require #/only-in lathe dissect expect mat next nextlet w-)

(require #/only-in effection/maybe/base just nothing maybe/c)

(require "../private/util.rkt")


(provide #/struct-out ordering-lt)
(provide #/struct-out ordering-eq)
(provide #/struct-out ordering-gt)
(provide ordering-private? dex-result? cline-result?)
(provide make-ordering-private-lt make-ordering-private-gt)

(provide name?)

(provide cline?)
(provide cline-containing)
(provide in-cline? compare-by-cline)

(provide dex?)
(provide in-dex? name-of compare-by-dex)

(provide #/struct-out dexable)
(provide valid-dexable? dexableof)
(provide dexables-autodex name-of-dexable)

(provide dex-dex dex-cline dex-name dex-by-cline)
(provide
  cline-by-dex
  cline-give-up
  cline-default
  cline-by-own-method
  cline-fix)



(struct-easy "an ordering-lt" (ordering-lt) #:equal)
(struct-easy "an ordering-private-lt" (ordering-private-lt) #:equal)
(struct-easy "an ordering-eq" (ordering-eq) #:equal)
(struct-easy "an ordering-private-gt" (ordering-private-gt) #:equal)
(struct-easy "an ordering-gt" (ordering-gt) #:equal)

(define/contract (ordering-private? x)
  (-> any/c boolean?)
  (or (ordering-private-lt? x) (ordering-private-gt? x)))

(define/contract (dex-result? x)
  (-> any/c boolean?)
  (or (ordering-private? x) (ordering-eq? x)))

(define/contract (cline-result? x)
  (-> any/c boolean?)
  (or (dex-result? x) (ordering-lt? x) (ordering-gt? x)))

; NOTE: We make these procedures because if we provided them as bare
; values, we would encourage people to write code that appeared to
; keep the ordering private but actually exposed it to a simple `eq?`
; check. Of course, Effection-unsafe Racket code can still compare
; these values by writing them to streams and observing the data
; that's written this way, but at least that's harder to do by
; accident.
(define/contract (make-ordering-private-lt)
  (-> ordering-private?)
  (ordering-private-lt))
(define/contract (make-ordering-private-gt)
  (-> ordering-private?)
  (ordering-private-gt))


; Internally, we represent name values as data made of symbols, empty
; lists, and cons cells, and for sorting purposes, we consider them to
; ascend in that order.
(struct-easy "a name" (name-internal rep))

(define/contract (name? x)
  (-> any/c boolean?)
  (name-internal? x))

(define/contract (names-autodex a b)
  (-> name? name? dex-result?)
  (dissect a (name-internal a)
  #/dissect b (name-internal b)
  #/nextlet a a b b
    
    ; Handle the cons cells.
    (mat a (cons a-first a-rest)
      (mat b (cons b-first b-rest)
        (w- first-result (next a-first b-first)
        #/expect first-result (ordering-eq) first-result
        #/next a-rest b-rest)
      #/ordering-private-gt)
    #/mat b (cons b-first b-rest) (ordering-private-lt)
    
    ; Handle the empty lists.
    #/mat a (list)
      (mat b (list) (ordering-eq)
      #/ordering-private-gt)
    #/mat b (list) (ordering-private-lt)
    
    ; Handle the symbols.
    #/if (symbol<? a b)
      (ordering-private-lt)
    #/if (symbol<? b a)
      (ordering-private-gt)
      (ordering-eq))))


(define-generics cline-internals
  (cline-internals-tag cline-internals)
  (cline-internals-autoname cline-internals)
  (cline-internals-autodex cline-internals other)
  (cline-internals-in? cline-internals x)
  (cline-internals-name-of cline-internals x)
  (cline-internals-call cline-internals a b))

(struct-easy "a cline-encapsulated" (cline-encapsulated internals))

(define/contract (cline? x)
  (-> any/c boolean?)
  (cline-encapsulated? x))

(define/contract (cline-containing x)
  (-> any/c flat-contract?)
  (and/c cline? #/lambda (cline) (in-cline? cline x)))

(define/contract (autoname-cline cline)
  (-> cline? name?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-autoname internals))

(define/contract (in-cline? cline x)
  (-> cline? any/c boolean?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-in? internals x))

(define/contract (name-of-by-cline cline x)
  (-> cline? any/c #/maybe/c name?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-name-of internals x))

(define/contract (compare-by-cline cline a b)
  (-> cline? any/c any/c #/maybe/c cline-result?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-call internals a b))


(define-generics dex-internals
  (dex-internals-tag dex-internals)
  (dex-internals-autoname dex-internals)
  (dex-internals-autodex dex-internals other)
  (dex-internals-in? dex-internals x)
  (dex-internals-name-of dex-internals x)
  (dex-internals-call dex-internals a b))

(struct-easy "a dex-encapsulated" (dex-encapsulated internals))

(define/contract (dex? x)
  (-> any/c boolean?)
  (dex-encapsulated? x))

(define/contract (autoname-dex dex)
  (-> dex? name?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-autoname internals))

(define/contract (in-dex? dex x)
  (-> dex? any/c boolean?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-in? internals x))

(define/contract (name-of dex x)
  (-> dex? any/c #/maybe/c name?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-name-of internals x))

(define/contract (compare-by-dex dex a b)
  (-> dex? any/c any/c #/maybe/c dex-result?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-call internals a b))


(struct-easy "a dexable" (dexable dex value))

(define/contract (valid-dexable? x)
  (-> any/c boolean?)
  (expect x (dexable dex value) #f
  #/and (dex? dex) (in-dex? dex value)))

(define/contract (dexableof c)
  (-> contract? contract?)
  (and/c valid-dexable?
  #/if (chaperone-contract? c)
    (struct/c dexable any/c c)
    (make-contract
      
      #:name 'dexableof
      
      #:first-order
      (lambda (x)
        (contract-first-order-passes?
          (struct/c dexable any/c #/lambda (x)
            (contract-first-order-passes? c x))
          x))
      
      #:projection
      (lambda (b)
        (w- c-projection ((contract-projection c) b)
        #/lambda (x)
          (dissect x (dexable dex x)
          #/dexable dex #/c-projection x))))))

; TODO: Rename this to `compare-dexables`.
(define/contract (dexables-autodex a b)
  (-> valid-dexable? valid-dexable? #/maybe/c dex-result?)
  (dissect a (dexable a-dex a)
  #/dissect b (dexable b-dex b)
  #/expect (compare-by-dex dex-dex a-dex b-dex) (just #/ordering-eq)
    (nothing)
  #/compare-by-dex a-dex a b))

(define/contract (name-of-dexable x)
  (-> valid-dexable? name?)
  (dissect x (dexable dex x)
  #/dissect (name-of dex x) (just result)
    result))



(struct-easy "a dex-internals-dex" (dex-internals-dex)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-dex)
    
    (define (dex-internals-autoname this)
      'dex-dex)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (dex? x))
    
    (define (dex-internals-name-of this x)
      (if (dex? x)
        (just #/autoname-dex x)
        (nothing)))
    
    (define (dex-internals-call this a b)
      (expect a (dex-encapsulated a) (nothing)
      #/expect b (dex-encapsulated b) (nothing)
      #/w- a-tag (dex-internals-tag a)
      #/w- b-tag (dex-internals-tag b)
      #/if (symbol<? a-tag b-tag) (just #/ordering-lt)
      #/if (symbol<? b-tag a-tag) (just #/ordering-gt)
      #/dex-internals-autodex a b))
  ])

(define/contract dex-dex dex? #/dex-encapsulated #/dex-internals-dex)


(struct-easy "a dex-internals-cline" (dex-internals-cline)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-cline)
    
    (define (dex-internals-autoname this)
      'dex-cline)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (cline? x))
    
    (define (dex-internals-name-of this x)
      (if (cline? x)
        (just #/autoname-cline x)
        (nothing)))
    
    (define (dex-internals-call this a b)
      (expect a (cline-encapsulated a) (nothing)
      #/expect b (cline-encapsulated b) (nothing)
      #/w- a-tag (cline-internals-tag a)
      #/w- b-tag (cline-internals-tag b)
      #/if (symbol<? a-tag b-tag) (just #/ordering-lt)
      #/if (symbol<? b-tag a-tag) (just #/ordering-gt)
      #/cline-internals-autodex a b))
  ])

(define/contract dex-cline dex?
  (dex-encapsulated #/dex-internals-cline))


(struct-easy "a dex-internals-name" (dex-internals-name)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-name)
    
    (define (dex-internals-autoname this)
      'dex-name)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (name? x))
    
    (define (dex-internals-name-of this x)
      (expect x (name-internal rep) (nothing)
      #/list #/list 'name rep))
    
    (define (dex-internals-call this a b)
      (if (and (name? a) (name? b))
        (just #/names-autodex a b)
        (nothing)))
  ])

(define/contract dex-name dex?
  (dex-encapsulated #/dex-internals-name))


(struct-easy "a dex-internals-by-cline" (dex-internals-by-cline cline)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (dex-internals-tag this)
      'dex-by-cline)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-by-cline cline)
      #/list 'dex-by-cline #/autoname-cline cline))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-by-cline a)
      #/dissect other (dex-internals-by-cline b)
      #/compare-by-dex dex-cline a b))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-by-cline cline)
      #/in-cline? cline x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-by-cline cline)
      #/name-of-by-cline cline x))
    
    (define (dex-internals-call this a b)
      (dissect this (dex-internals-by-cline cline)
      #/w- cline-result (compare-by-cline cline a b)
      #/expect cline-result (just cline-result) cline-result
      #/list
      #/mat cline-result (ordering-lt) (ordering-private-lt)
      #/mat cline-result (ordering-gt) (ordering-private-gt)
        cline-result))
  ])

(define/contract (dex-by-cline cline)
  (-> cline? dex?)
  (dex-encapsulated #/dex-internals-by-cline cline))


(struct-easy "a cline-internals-by-dex" (cline-internals-by-dex dex)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-by-dex)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-by-dex dex)
      #/list 'cline-by-dex #/autoname-dex dex))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-dex a)
      #/dissect other (cline-internals-by-dex b)
      #/compare-by-dex dex-dex a b))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-by-dex dex)
      #/in-dex? dex x))
    
    (define (cline-internals-name-of this x)
      (dissect this (cline-internals-by-dex dex)
      #/name-of dex x))
    
    (define (cline-internals-call this a b)
      (dissect this (cline-internals-by-dex dex)
      #/compare-by-dex dex a b))
  ])

(define/contract (cline-by-dex dex)
  (-> dex? cline?)
  (cline-encapsulated #/cline-internals-by-dex dex))


(struct-easy "a cline-internals-give-up" (cline-internals-give-up)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-give-up)
    
    (define (cline-internals-autoname this)
      'cline-give-up)
    
    (define (cline-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (cline-internals-in? this x)
      #f)
    
    (define (cline-internals-name-of this x)
      (nothing))
    
    (define (cline-internals-call this a b)
      (nothing))
  ])

(define/contract cline-give-up cline?
  (cline-encapsulated #/cline-internals-give-up))


(struct-easy "a cline-internals-default"
  (cline-internals-default
    cline-for-trying-first
    cline-for-trying-second)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-default)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-default first second)
      #/list 'cline-default
        (autoname-cline first)
        (autoname-cline second)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-default a1 a2)
      #/dissect other (cline-internals-default b1 b2)
      #/dissect (compare-by-dex dex-cline a1 b1) result
      #/expect result (just #/ordering-eq) result
      #/compare-by-dex dex-cline a2 b2))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-default first second)
      #/or (in-cline? first x) (in-cline? second x)))
    
    (define (cline-internals-name-of this x)
      (dissect this (cline-internals-default first second)
      #/mat (name-of-by-cline first x) (just result) (just result)
      #/name-of-by-cline second x))
    
    (define (cline-internals-call this a b)
      (dissect this (cline-internals-default first second)
      #/w- first-result (compare-by-cline first a b)
      #/mat first-result (just -) first-result
      #/if (in-cline? first a)
        (if (in-cline? second b)
          (just #/ordering-lt)
          (nothing))
      #/if (in-cline? first b)
        (if (in-cline? second a)
          (just #/ordering-gt)
          (nothing))
      #/compare-by-cline second a b))
  ])

(define/contract
  (cline-default cline-for-trying-first cline-for-trying-second)
  (-> cline? cline? cline?)
  (cline-encapsulated
  #/cline-internals-default
    cline-for-trying-first
    cline-for-trying-second))


(struct-easy "a cline-internals-by-own-method"
  (cline-internals-by-own-method dexable-get-method)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-by-own-method)
    
    (define (cline-internals-autoname this)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/list 'cline-by-own-method
        (autoname-dex dex)
        (name-of dex get-method)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-own-method a)
      #/dissect other (cline-internals-by-own-method b)
      #/dexables-autodex a b))
    
    (define (cline-internals-in? this x)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method x) (just method) #f
        ; NOTE: Since our contract for `cline-by-own-method` enforces
        ; that the method must return a cline that contains the value,
        ; we don't have to bother with an `in-cline?` call here.
        #t))
    
    (define (cline-internals-name-of this x)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method x) (just method) (nothing)
      #/name-of-by-cline method x))
    
    (define (cline-internals-call this a b)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method a) (just a-method) (nothing)
      #/expect (get-method b) (just b-method) (nothing)
      #/expect (compare-by-dex dex-cline a-method b-method)
        (just #/ordering-eq)
        ; TODO: Choose an error message.
        (error "Called a cline-by-own-method on two values with different methods")
      #/compare-by-cline a-method a b))
  ])

(define/contract
  (cline-by-own-method dexable-get-method)
  (->
    (dexableof
    #/->i ([x any/c]) [result (x) (maybe/c #/cline-containing x)])
    cline?)
  (cline-encapsulated
  #/cline-internals-by-own-method dexable-get-method))


(struct-easy "a cline-internals-fix"
  (cline-internals-fix dexable-unwrap)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-fix)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/list 'cline-fix
        (autoname-dex dex)
        (name-of dex unwrap)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-fix a)
      #/dissect other (cline-internals-fix b)
      #/dexables-autodex a b))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/in-cline? (unwrap #/cline-encapsulated this) x))
    
    (define (cline-internals-name-of this x)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/name-of-by-cline (unwrap #/cline-encapsulated this) x))
    
    (define (cline-internals-call this a b)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/compare-by-cline (unwrap #/cline-encapsulated this) a b))
  ])

(define/contract (cline-fix dexable-unwrap)
  (-> (dexableof #/-> cline? cline?) cline?)
  (cline-encapsulated #/cline-internals-fix dexable-unwrap))
