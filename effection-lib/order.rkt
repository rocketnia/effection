#lang parendown racket/base

(require #/only-in racket/contract/base -> any/c)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts expect)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct struct-easy)

(require effection/order/base)
(require #/only-in effection/order/private
  exact-rational? lt-autocline lt-autodex)
(require #/prefix-in internal: #/only-in effection/order/unsafe
  cline dex gen:cline-internals gen:dex-internals name)

(provide #/all-from-out effection/order/base)

(provide
  dex-immutable-string
  cline-immutable-string
  dex-exact-rational
  cline-exact-rational)


(define-syntax-rule
  (define-datum-cline
    dex-internals-id dex-id tag:dex-id
    cline-internals-id cline-id tag:cline-id
    name:id id? id<?)
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
            #/list 'name:id #/string->symbol x)
            (nothing)))
        
        (define (dex-internals-compare this a b)
          (expect (id? a) #t (nothing)
          #/expect (id? b) #t (nothing)
          #/just #/lt-autodex a b string<?))
      ])
    
    (define/contract (dex-id)
      (-> dex?)
      (internal:dex #/dex-internals-id))
    
    
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
          #/just #/lt-autocline a b string<?))
      ])
    
    (define/contract (cline-id)
      (-> cline?)
      (internal:cline #/cline-internals-id))
  ))


(define-datum-cline
  dex-internals-immutable-string dex-immutable-string
  tag:dex-immutable-string
  cline-internals-immutable-string cline-immutable-string
  tag:cline-immutable-string
  name:immutable-string immutable-string? string<?)

(define-datum-cline
  dex-internals-exact-rational dex-exact-rational
  tag:dex-exact-rational
  cline-internals-exact-rational cline-exact-rational
  tag:cline-exact-rational
  name:exact-rational exact-rational? <)
