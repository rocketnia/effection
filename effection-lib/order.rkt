#lang parendown racket/base

(require #/only-in racket/contract/base -> any/c)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts expect fn)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial trivial?)

(require effection/order/base)
(require #/submod effection/order/base private/order)
(require #/only-in effection/order/private
  exact-rational? lt-autocline lt-autodex)
(require #/prefix-in internal: #/only-in effection/order/unsafe
  cline dex fuse gen:cline-internals gen:dex-internals
  gen:furge-internals name)

(provide #/all-from-out effection/order/base)
(provide #/all-from-out #/submod effection/order/base private/order)

(provide
  
  dex-trivial
  dex-immutable-string
  cline-immutable-string
  dex-exact-rational
  cline-exact-rational
  fuse-exact-rational-by-plus
  fuse-exact-rational-by-times
  
  eq-by-dex?
  
  )


(define-syntax-rule
  (define-datum-dex
    dex-internals-id dex-id tag:dex-id name:id
    id? id->name-internals id<?)
  (begin
    (struct-easy (dex-internals-id)
      
      #:other
      
      #:methods internal:gen:dex-internals
      [
        
        (define (dex-internals-tag this)
          'tag:dex-id)
        
        (define (dex-internals-autoname this)
          'tag:dex-id)
        
        (define (dex-internals-autodex this other)
          (just #/ordering-eq))
        
        (define (dex-internals-in? this x)
          (id? x))
        
        (define (dex-internals-name-of this x)
          (if (id? x)
            (just #/internal:name
            #/list 'name:id #/id->name-internals x)
            (nothing)))
        
        (define (dex-internals-compare this a b)
          (expect (id? a) #t (nothing)
          #/expect (id? b) #t (nothing)
          #/just #/lt-autodex a b id<?))
      ])
    
    (define/contract (dex-id)
      (-> dex?)
      (internal:dex #/dex-internals-id))
  ))

(define-syntax-rule
  (define-datum-cline
    dex-internals-id dex-id tag:dex-id
    cline-internals-id cline-id tag:cline-id
    name:id id? id->name-internals id<?)
  (begin
    (define-datum-dex
      dex-internals-id dex-id tag:dex-id name:id
      id? id->name-internals id<?)
    
    (struct-easy (cline-internals-id)
      #:other
      
      #:methods internal:gen:cline-internals
      [
        
        (define (cline-internals-tag this)
          'tag:cline-id)
        
        (define (cline-internals-autoname this)
          'tag:cline-id)
        
        (define (cline-internals-autodex this other)
          (just #/ordering-eq))
        
        (define (cline-internals-dex this)
          (dex-id))
        
        (define (cline-internals-in? this x)
          (id? x))
        
        (define (cline-internals-compare this a b)
          (expect (id? a) #t (nothing)
          #/expect (id? b) #t (nothing)
          #/just #/lt-autocline a b id<?))
      ])
    
    (define/contract (cline-id)
      (-> cline?)
      (internal:cline #/cline-internals-id))
  ))


(define-datum-dex
  dex-internals-trivial dex-trivial tag:dex-trivial name:trivial
  trivial? (fn x 'trivial) (fn a b #f))

(define-datum-cline
  dex-internals-immutable-string dex-immutable-string
  tag:dex-immutable-string
  cline-internals-immutable-string cline-immutable-string
  tag:cline-immutable-string
  name:immutable-string immutable-string? string->symbol string<?)

(define-datum-cline
  dex-internals-exact-rational dex-exact-rational
  tag:dex-exact-rational
  cline-internals-exact-rational cline-exact-rational
  tag:cline-exact-rational
  name:exact-rational exact-rational? (fn x x) <)


(struct-easy (fuse-internals-exact-rational-by-plus)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-exact-rational-by-plus)
    
    (define (furge-internals-autoname this)
      'tag:fuse-exact-rational-by-plus)
    
    (define (furge-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (furge-internals-call this a b)
      (expect (exact-rational? a) #t (nothing)
      #/expect (exact-rational? b) #t (nothing)
      #/just #/+ a b))
  ])

(define/contract (fuse-exact-rational-by-plus)
  (-> fuse?)
  (internal:fuse #/fuse-internals-exact-rational-by-plus))


(struct-easy (fuse-internals-exact-rational-by-times)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-exact-rational-by-times)
    
    (define (furge-internals-autoname this)
      'tag:fuse-exact-rational-by-times)
    
    (define (furge-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (furge-internals-call this a b)
      (expect (exact-rational? a) #t (nothing)
      #/expect (exact-rational? b) #t (nothing)
      #/just #/* a b))
  ])

(define/contract (fuse-exact-rational-by-times)
  (-> fuse?)
  (internal:fuse #/fuse-internals-exact-rational-by-times))


(define/contract (eq-by-dex? dex a b)
  (-> dex? any/c any/c boolean?)
  (expect (compare-by-dex dex a b) (just comparison)
    (raise-arguments-error 'eq-by-dex?
      "expected a and b to be members of the domain of dex"
      "dex" dex
      "a" a
      "b" b)
  #/ordering-eq? comparison))
