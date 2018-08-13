#lang parendown racket/base

(require #/only-in racket/contract/base -> any/c)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts expect)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/struct struct-easy)

(require effection/order/base)
(require #/only-in effection/order/private lt-autocline lt-autodex)
(require #/prefix-in internal: #/only-in effection/order/unsafe
  cline dex gen:cline-internals gen:dex-internals name)

(provide #/all-from-out effection/order/base)

(provide
  dex-immutable-string
  cline-immutable-string)


(define/contract (immutable-string? v)
  (-> any/c boolean?)
  (and (string? v) (immutable? v)))

(struct-easy (dex-internals-immutable-string)
  
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-immutable-string)
    
    (define (dex-internals-autoname this)
      'tag:dex-immutable-string)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (immutable-string? x))
    
    (define (dex-internals-name-of this x)
      (if (immutable-string? x)
        (just #/internal:name
        #/list 'name:immutable-string #/string->symbol x)
        (nothing)))
    
    (define (dex-internals-compare this a b)
      (expect (immutable-string? a) #t (nothing)
      #/expect (immutable-string? b) #t (nothing)
      #/just #/lt-autodex a b string<?))
  ])

(define/contract (dex-immutable-string)
  (-> dex?)
  (internal:dex #/dex-internals-immutable-string))


(struct-easy (cline-internals-immutable-string)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-immutable-string)
    
    (define (cline-internals-autoname this)
      'tag:cline-immutable-string)
    
    (define (cline-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (cline-internals-dex this)
      (dex-immutable-string))
    
    (define (cline-internals-in? this x)
      (immutable-string? x))
    
    (define (cline-internals-compare this a b)
      (expect (immutable-string? a) #t (nothing)
      #/expect (immutable-string? b) #t (nothing)
      #/just #/lt-autocline a b string<?))
  ])

(define/contract (cline-immutable-string)
  (-> cline?)
  (internal:cline #/cline-internals-immutable-string))
