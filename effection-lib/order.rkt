#lang parendown racket/base

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> any/c contract? contract-name)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract contract-first-order-passes?
  raise-blame-error)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe-comforts dissect expect fn mat w-)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/string immutable-string?)
(require #/only-in lathe-comforts/struct struct-easy)
(require #/only-in lathe-comforts/trivial trivial trivial?)

(require effection/order/base)
(require #/submod effection/order/base private/order)
(require #/only-in effection/order/private
  exact-rational? lt-autocline lt-autodex
  make-appropriate-non-chaperone-contract)
(require #/prefix-in internal: #/only-in effection/order/unsafe
  cline dex fuse gen:cline-internals gen:dex-internals
  gen:furge-internals name)

(provide #/all-from-out effection/order/base)
(provide #/all-from-out #/submod effection/order/base private/order)

(provide
  
  dex-trivial
  dex-boolean
  cline-boolean-by-truer
  cline-boolean-by-falser
  merge-boolean-by-and
  merge-boolean-by-or
  dex-immutable-string
  cline-immutable-string
  dex-exact-rational
  cline-exact-rational
  fuse-exact-rational-by-plus
  fuse-exact-rational-by-times
  
  eq-by-dex?
  table-kv-map
  table-kv-all?
  table-kv-any?
  table-v-map
  table-v-all?
  table-v-any?
  table-v-of
  
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
  dex-internals-boolean dex-boolean tag:dex-boolean
  cline-internals-boolean-by-truer cline-boolean-by-truer
  tag:cline-boolean-by-truer
  name:boolean boolean? (fn x #/if x 't 'f) (fn a b #/and (not a) b))

(define/contract (cline-boolean-by-falser)
  (-> cline?)
  (cline-flip #/cline-boolean-by-truer))

(define/contract (merge-boolean-by-and)
  (-> cline?)
  (merge-by-cline-min #/cline-boolean-by-truer))

(define/contract (merge-boolean-by-or)
  (-> cline?)
  (merge-by-cline-max #/cline-boolean-by-truer))

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

; TODO: Come up with a better name for this, and export it.
(define/contract
  (table-kv-map-fuse-default table fuse-0 fuse-2 kv-to-operand)
  (-> table? any/c fuse? (-> name? any/c any/c) any/c)
  (mat
    (table-map-fuse table fuse-2 #/fn k
      (dissect (table-get k table) (just v)
      #/kv-to-operand k v))
    (just result)
    result
    fuse-0))

(define/contract (table-kv-map table kv-to-v)
  (-> table? (-> name? any/c any/c) table?)
  (table-kv-map-fuse-default table (table-empty)
    (fuse-by-merge #/merge-table #/merge-by-dex #/dex-give-up)
  #/fn k v
    (table-shadow k (just #/kv-to-v k v) #/table-empty)))

(define/contract (table-kv-all? table kv-accepted?)
  (-> table? (-> name? any/c boolean?) table?)
  (table-kv-map-fuse-default table #t
    (fuse-by-merge #/merge-boolean-by-and)
    kv-accepted?))

(define/contract (table-kv-any? table kv-accepted?)
  (-> table? (-> name? any/c boolean?) table?)
  (table-kv-map-fuse-default table #f
    (fuse-by-merge #/merge-boolean-by-or)
    kv-accepted?))

(define/contract (table-v-map table v-to-v)
  (-> table? (-> any/c any/c) table?)
  (table-kv-map table #/fn k v #/v-to-v v))

(define/contract (table-v-all? table v-accepted?)
  (-> table? (-> any/c boolean?) table?)
  (table-kv-all? table #/fn k v #/v-accepted? v))

(define/contract (table-v-any? table v-accepted?)
  (-> table? (-> any/c boolean?) table?)
  (table-kv-any? table #/fn k v #/v-accepted? v))

(define/contract (table-v-of c)
  (-> contract? contract?)
  (w- c (coerce-contract 'table-v-of c)
  #/ (make-appropriate-non-chaperone-contract c)
    
    #:name `(table-v-of ,(contract-name c))
    
    #:first-order
    (fn v
      (and (table? v)
      #/table-v-all? v #/fn v
        (contract-first-order-passes? c v)))
    
    #:late-neg-projection
    (fn blame
      (w- c-late-neg-projection
        ( (get/build-late-neg-projection c)
          (blame-add-context blame "a value of"))
      #/fn v missing-party
        (expect (table? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a table" given: "~e")
            v)
        #/table-v-map v #/fn v
          (c-late-neg-projection v missing-party))))))
