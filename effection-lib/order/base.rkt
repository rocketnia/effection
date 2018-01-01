#lang parendown racket/base

(require #/only-in racket/contract/base
  -> ->i and/c any/c chaperone-contract? cons/c contract?
  contract-projection flat-contract? or/c struct/c struct/dc)
(require #/only-in racket/contract/combinator
  contract-first-order-passes? make-contract)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define-generics)

(require #/only-in lathe dissect expect mat w-)

(require "../private/util.rkt")


; TODO: Put this somewhere better.
(provide maybe/c)

(provide #/struct-out ordering-lt)
(provide #/struct-out ordering-eq)
(provide #/struct-out ordering-gt)
; TODO: Consider exporting functions that construct
; `ordering-private-lt` and `ordering-private-gt` values.
(provide ordering-private?)
(provide dex-result?)
(provide cline-result?)

(provide cline?)
(provide cline-containing)
(provide in-cline? call-cline)

(provide dex?)
(provide dex/c)
(provide in-dex? call-dex)

(provide #/struct-out dexable)
(provide dexable/c dexableof)
(provide dexables-autodex)

(provide dex-dex dex-cline)
(provide
  cline-by-dex
  cline-give-up
  cline-default
  cline-by-own-method
  cline-fix)



; TODO: Put this somewhere better.
(define/contract (maybe/c c)
  (-> contract? contract?)
  (or/c null? #/cons/c c null?))


(struct-easy "a ordering-lt" (ordering-lt) #:equal)
(struct-easy "a ordering-private-lt" (ordering-private-lt) #:equal)
(struct-easy "a ordering-eq" (ordering-eq) #:equal)
(struct-easy "a ordering-private-gt" (ordering-private-gt) #:equal)
(struct-easy "a ordering-gt" (ordering-gt) #:equal)

(define/contract (ordering-private? x)
  (-> any/c boolean?)
  (or (ordering-private-lt? x) (ordering-private-gt? x)))

(define/contract (dex-result? x)
  (-> any/c boolean?)
  (or (ordering-private? x) (ordering-eq? x)))

(define/contract (cline-result? x)
  (-> any/c boolean?)
  (or (dex-result? x) (ordering-lt? x) (ordering-gt? x)))


(define-generics cline-internals
  (cline-internals-name cline-internals)
  (cline-internals-autodex cline-internals other)
  (cline-internals-in? cline-internals x)
  (cline-internals-call cline-internals a b))

(struct-easy "a cline-encapsulated" (cline-encapsulated internals))

(define/contract (cline? x)
  (-> any/c boolean?)
  (cline-encapsulated? x))

(define/contract (cline-containing x)
  (-> any/c flat-contract?)
  (and/c cline? #/lambda (cline) (in-cline? cline x)))

(define/contract (in-cline? cline x)
  (-> cline? any/c boolean?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-in? internals x))

(define/contract (call-cline cline a b)
  (-> cline? any/c any/c #/maybe/c cline-result?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-call internals a b))


(define-generics dex-internals
  (dex-internals-name dex-internals)
  (dex-internals-autodex dex-internals other)
  (dex-internals-in? dex-internals x)
  (dex-internals-call dex-internals a b))

(struct-easy "a dex-encapsulated" (dex-encapsulated internals))

(define/contract (dex? x)
  (-> any/c boolean?)
  (dex-encapsulated? x))

(define/contract (dex/c dex)
  (-> dex? flat-contract?)
  (lambda (x) (in-dex? dex x)))

(define/contract (in-dex? dex x)
  (-> dex? any/c boolean?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-in? internals x))

(define/contract (call-dex dex a b)
  (-> dex? any/c any/c #/maybe/c dex-result?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-call internals a b))


(struct-easy "a dexable" (dexable dex value))

(define/contract dexable/c contract?
  (struct/dc dexable
    [dex dex?]
    [value (dex) #:flat (dex/c dex)]))

(define/contract (dexableof c)
  (-> contract? contract?)
  (and/c dexable/c
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

(define/contract (dexables-autodex a b)
  (-> dexable/c dexable/c boolean?)
  (dissect a (dexable a-dex a)
  #/dissect b (dexable b-dex b)
  #/expect (call-dex dex-dex a-dex b-dex) (list #/ordering-eq) (list)
  #/call-dex a-dex a b))



(struct-easy "a dex-internals-dex" (dex-internals-dex)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-name this)
      'dex-internals-dex)
    
    (define (dex-internals-autodex this other)
      (list #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (dex? x))
    
    (define (dex-internals-call this a b)
      (expect a (dex-encapsulated a) (list)
      #/expect b (dex-encapsulated b) (list)
      #/w- a-name (dex-internals-name a)
      #/w- b-name (dex-internals-name b)
      #/if (symbol<? a-name b-name) (list #/ordering-lt)
      #/if (symbol<? b-name a-name) (list #/ordering-gt)
      #/dex-internals-autodex a b))
  ])

(define/contract dex-dex dex? #/dex-encapsulated #/dex-internals-dex)


(struct-easy "a dex-internals-cline" (dex-internals-cline)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-name this)
      'dex-internals-cline)
    
    (define (dex-internals-autodex this other)
      (list #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (cline? x))
    
    (define (dex-internals-call this a b)
      (expect a (cline-encapsulated a) (list)
      #/expect b (cline-encapsulated b) (list)
      #/w- a-name (cline-internals-name a)
      #/w- b-name (cline-internals-name b)
      #/if (symbol<? a-name b-name) (list #/ordering-lt)
      #/if (symbol<? b-name a-name) (list #/ordering-gt)
      #/cline-internals-autodex a b))
  ])

(define/contract dex-cline dex?
  (dex-encapsulated #/dex-internals-cline))


(struct-easy "a dex-internals-by-cline" (dex-internals-by-cline cline)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (dex-internals-name this)
      'dex-internals-by-cline)
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-by-cline a)
      #/dissect other (dex-internals-by-cline b)
      #/call-dex dex-cline a b))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-by-cline cline)
      #/in-cline? cline x))
    
    (define (dex-internals-call this a b)
      (dissect this (dex-internals-by-cline cline)
      #/w- cline-result (call-cline cline a b)
      #/expect cline-result (list cline-result) cline-result
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
    
    (define (cline-internals-name this)
      'cline-internals-by-dex)
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-dex a)
      #/dissect other (cline-internals-by-dex b)
      #/call-dex dex-dex a b))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-by-dex dex)
      #/in-dex? dex x))
    
    (define (cline-internals-call this a b)
      (dissect this (cline-internals-by-dex dex)
      #/call-dex dex a b))
  ])

(define/contract (cline-by-dex dex)
  (-> dex? cline?)
  (cline-encapsulated #/cline-internals-by-dex dex))


(struct-easy "a cline-internals-give-up" (cline-internals-give-up)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-name this)
      'cline-internals-give-up)
    
    (define (cline-internals-autodex this other)
      (list #/ordering-eq))
    
    (define (cline-internals-in? this x)
      #f)
    
    (define (cline-internals-call this a b)
      (list))
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
    
    (define (cline-internals-name this)
      'cline-internals-default)
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-default a1 a2)
      #/dissect other (cline-internals-default b1 b2)
      #/dissect (call-dex dex-cline a1 b1) result
      #/expect result (list #/ordering-eq) result
      #/call-dex dex-cline a2 b2))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-default first second)
      #/or (in-cline? first x) (in-cline? second x)))
    
    (define (cline-internals-call this a b)
      (dissect this (cline-internals-default first second)
      #/w- first-result (call-cline first a b)
      #/mat first-result (list -) first-result
      #/if (in-cline? first a)
        (if (in-cline? second b)
          (list #/ordering-lt)
          (list))
      #/if (in-cline? first b)
        (if (in-cline? second a)
          (list #/ordering-gt)
          (list))
      #/call-cline second a b))
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
    
    (define (cline-internals-name this)
      'cline-internals-by-own-method)
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-own-method a)
      #/dissect other (cline-internals-by-own-method b)
      #/dexables-autodex a b))
    
    (define (cline-internals-in? this x)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method x) (list method) #f
      ; TODO: Currently, this is redundant because our contract for
      ; `cline-by-own-method` enforces that the method must return a
      ; cline that contains the value. If we keep that contract,
      ; remove this check here, and vice versa.
      #/in-cline? method x))
    
    (define (cline-internals-call this a b)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method a) (list a-method) (list)
      #/expect (get-method b) (list b-method) (list)
      #/expect (call-dex dex-cline a-method b-method)
        (list #/ordering-eq)
        
        ; TODO: See if we should indeed raise an error here. An
        ; alternative would be to use the result of this `call-dex` as
        ; the result.
        ;
        ; TODO: Choose an error message.
        ;
        (error "Called a cline-by-own-method on two values with different methods")
      #/call-cline a-method a b))
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
    
    (define (cline-internals-name this)
      'cline-internals-fix)
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-fix a)
      #/dissect other (cline-internals-fix b)
      #/dexables-autodex a b))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/in-cline? (unwrap #/cline-encapsulated this) x))
    
    (define (cline-internals-call this a b)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/call-cline (unwrap #/cline-encapsulated this) a b))
  ])

(define/contract (cline-fix dexable-unwrap)
  (-> (dexableof #/-> cline? cline?) cline?)
  (cline-encapsulated #/cline-internals-fix dexable-unwrap))
