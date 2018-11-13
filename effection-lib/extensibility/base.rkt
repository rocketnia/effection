#lang parendown racket/base

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> any/c chaperone-contract? contract? contract-name flat-contract?
  listof or/c)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract contract-first-order-passes?
  make-chaperone-contract make-contract make-flat-contract
  raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts dissect expect fn mat w-)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/struct istruct/c struct-easy)
(require #/only-in lathe-comforts/trivial trivial?)

(require #/only-in effection/order/base
  dexable dexableof fuse? name? ordering-eq table? tableof
  valid-dexable?)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe
  fuse gen:furge-internals)


; TODO: Finish implementing each of these exports, and document them.
(provide
  
  extfx?
  dspace?
  
  extfx-noop
  fuse-extfx
  extfx-later
  extfx-table-each
  
  extfx-dspace-create-shadower
  
  authorized-name?
  authorized-name-get-name
  name-subname
  authorized-name-subname
  extfx-claim-and-split
  
  optionally-dexable?
  optionally-dexable-of
  optionally-dexable-once
  optionally-dexable-dexable
  
  extfx-put
  extfx-get
  
  extfx-private-put
  extfx-private-get
  
  pub?
  sub?
  pub-restrict
  sub-restrict
  extfx-establish-pubsub
  extfx-pub
  extfx-sub
  
  familiarity-ticket?
  extfx-ft-split-list
  extfx-ft-split-table
  extfx-ft-subname
  extfx-ft-restrict
  
  extfx-ft-disburse
  extfx-ft-claim
  
  extfx-contribute
  extfx-collect
  
  )


(module private racket/base
  
  (require #/only-in lathe-comforts/struct struct-easy)
  
  
  (define-syntax-rule (provide-struct (name field ...) option ...)
    (begin
      (struct-easy (name field ...) option ...)
      (provide #/struct-out name)))
  
  
  (provide-struct (optionally-dexable-once v) #:equal)
  (provide-struct (optionally-dexable-dexable v) #:equal)
  
  
  (provide-struct (extfx-noop))
  (provide-struct (extfx-fused a b))
  (provide-struct (extfx-later then))
  (provide-struct (extfx-table-each t on-element then))
  
  (provide-struct (extfx-dspace-create-shadower ds then))
  
  (provide-struct (extfx-claim-and-split n times then))
  
  (provide-struct (extfx-put ds n comp))
  (provide-struct (extfx-get ds n then))
  
  (provide-struct (extfx-private-put ds putter-name getter-name comp))
  (provide-struct (extfx-private-get ds putter-name getter-name then))
  
  (provide-struct (extfx-establish-pubsub ds pubsub-name then))
  (provide-struct (extfx-pub ds p pubber-name arg))
  (provide-struct (extfx-sub ds s subber-name func))
  
  (provide-struct (extfx-ft-split-list ticket times then))
  (provide-struct (extfx-ft-split-table ticket times then))
  (provide-struct (extfx-ft-subname ticket key then))
  (provide-struct (extfx-ft-restrict ticket ds then))
  
  (provide-struct (extfx-ft-disburse ds hub-name comp-ticket))
  (provide-struct (extfx-ft-claim ds t then))
  
  (provide-struct
    (extfx-contribute ds collector-familiarity-ticket comp))
  (provide-struct (extfx-collect ds collector-name then))
  
  
  )

(require #/prefix-in internal: 'private)



; TODO: This is also defined privately in `effection/order/base`. See
; if we can extract it into a library or something.
(define (make-appropriate-contract c)
  (if (flat-contract? c)
    make-flat-contract
  #/if (chaperone-contract? c)
    make-chaperone-contract
    make-contract))



(define/contract (extfx? v)
  (-> any/c boolean?)
  
  (mat v (internal:extfx-noop) #t
  #/mat v (internal:extfx-fused a b) #t
  #/mat v (internal:extfx-later then) #t
  #/mat v (internal:extfx-table-each t on-element then) #t
  
  #/mat v (internal:extfx-dspace-create-shadower ds then) #t
  
  #/mat v (internal:extfx-claim-and-split n times then) #t
  
  #/mat v (internal:extfx-put ds n comp) #t
  #/mat v (internal:extfx-get ds n then) #t
  
  #/mat v (internal:extfx-private-put ds putter-name getter-name comp)
    #t
  #/mat v (internal:extfx-private-get ds putter-name getter-name then)
    #t
  
  #/mat v (internal:extfx-establish-pubsub ds pubsub-name then) #t
  #/mat v (internal:extfx-pub ds p pubber-name arg) #t
  #/mat v (internal:extfx-pub ds s subber-name func) #t
  
  #/mat v (internal:extfx-ft-split-list ticket times then) #t
  #/mat v (internal:extfx-ft-split-table ticket times then) #t
  #/mat v (internal:extfx-ft-subname ticket key then) #t
  #/mat v (internal:extfx-ft-restrict ticket ds then) #t
  
  #/mat v (internal:extfx-ft-disburse ds hub-name comp-ticket) #t
  #/mat v (internal:extfx-ft-claim ds t then) #t
  
  #/mat v
    (internal:extfx-contribute ds collector-familiarity-ticket comp)
    #t
  #/mat v (internal:extfx-collect ds collector-name then) #t
  
    #f))

(define/contract (dspace? v)
  (-> any/c boolean?)
  'TODO)


(define/contract (extfx-noop)
  (-> extfx?)
  (internal:extfx-noop))

(struct-easy (fuse-internals-extfx)
  #:other
  
  #:methods unsafe:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-extfx)
    
    (define (furge-internals-autoname this)
      'tag:fuse-extfx)
    
    (define (furge-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (furge-internals-call this a b)
      (expect (extfx? a) #t (nothing)
      #/expect (extfx? b) #t (nothing)
      #/just #/internal:extfx-fused a b))
  ])

(define/contract (fuse-extfx)
  (-> fuse?)
  (unsafe:fuse #/fuse-internals-extfx))

(define/contract (extfx-later then)
  (-> (-> extfx?) extfx?)
  (internal:extfx-later then))

(define/contract (extfx-table-each t on-element then)
  (-> table? (-> any/c (-> any/c extfx?) extfx?) (-> table? extfx?)
    extfx?)
  (internal:extfx-table-each t on-element then))


(define/contract (extfx-dspace-create-shadower ds then)
  (-> dspace? (-> dspace? extfx?) extfx?)
  (internal:extfx-dspace-create-shadower ds then))


(define/contract (authorized-name? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (authorized-name-get-name n)
  (-> authorized-name? name?)
  'TODO)

(define/contract (name-subname key-name original-name)
  (-> name? name? name?)
  'TODO)

(define/contract (authorized-name-subname key-name original-name)
  (-> name? authorized-name? authorized-name?)
  'TODO)

(define/contract (extfx-claim-and-split n times then)
  (-> authorized-name? natural? (-> (listof authorized-name?) extfx?)
    extfx?)
  (internal:extfx-claim-and-split n times then))


(define/contract (optionally-dexable? v)
  (-> any/c boolean?)
  (or
    (internal:optionally-dexable-once? v)
    (internal:optionally-dexable-dexable? v)))

(define/contract (optionally-dexable-of c)
  (-> contract? contract?)
  (w- c (coerce-contract 'optionally-dexable-of c)
  #/ (make-appropriate-contract c)
    
    #:name `(optionally-dexable-of ,(contract-name c))
    
    #:first-order
    (fn v
      (contract-first-order-passes?
        (or/c
          (istruct/c internal:optionally-dexable-once any/c)
          (istruct/c internal:optionally-dexable-dexable
            (dexableof c)))
        v))
    
    #:late-neg-projection
    (fn blame
      (w- c-late-neg-projection
        ( (get/build-late-neg-projection c)
          (blame-add-context blame "the value of"))
      #/fn v missing-party
        (expect (optionally-dexable? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "an optionally dexable value" given: "~e")
            v)
        #/expect v (internal:optionally-dexable-dexable v) v
        #/dissect v (dexable dex v)
        #/internal:optionally-dexable-dexable
        #/dexable dex #/c-late-neg-projection v missing-party)))))

(define/contract (optionally-dexable-once v)
  (-> any/c optionally-dexable?)
  (internal:optionally-dexable-once v))

(define/contract (optionally-dexable-dexable v)
  (-> valid-dexable? optionally-dexable?)
  (internal:optionally-dexable-dexable v))


(define/contract (extfx-put ds n comp)
  (-> dspace? authorized-name?
    (-> (-> optionally-dexable? extfx?) extfx?)
    extfx?)
  (internal:extfx-put ds n comp))

(define/contract (extfx-get ds n then)
  (-> dspace? name? (-> any/c extfx?) extfx?)
  (internal:extfx-get ds n then))


(define/contract
  (extfx-private-put ds putter-name getter-name comp)
  (-> dspace? authorized-name? name?
    (-> (-> optionally-dexable? extfx?) extfx?)
    extfx?)
  (internal:extfx-private-put ds putter-name getter-name comp))

(define/contract (extfx-private-get ds putter-name getter-name then)
  (-> dspace? name? authorized-name? (-> any/c extfx?) extfx?)
  (internal:extfx-private-get ds putter-name getter-name then))


(define/contract (pub? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (sub? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (pub-restrict ds p)
  (-> dspace? pub? pub?)
  'TODO)

(define/contract (sub-restrict ds s)
  (-> dspace? sub? sub?)
  'TODO)

(define/contract (extfx-establish-pubsub ds pubsub-name then)
  (-> dspace? authorized-name? (-> pub? sub? extfx?) extfx?)
  (internal:extfx-establish-pubsub ds pubsub-name then))

(define/contract (extfx-pub ds p pubber-name arg)
  (-> dspace? pub? authorized-name? optionally-dexable? extfx?)
  (internal:extfx-pub ds p pubber-name arg))

(define/contract (extfx-sub ds s subber-name func)
  (-> dspace? sub? authorized-name?
    (optionally-dexable-of #/-> any/c extfx?)
    extfx?)
  (internal:extfx-sub ds s subber-name func))


(define/contract (familiarity-ticket? v)
  (-> any/c boolean?)
  'TODO)

(define/contract (extfx-ft-split-list ticket times then)
  (-> familiarity-ticket? natural?
    (-> (listof familiarity-ticket?) extfx?)
    extfx?)
  (internal:extfx-ft-split-list ticket times then))

(define/contract (extfx-ft-split-table ticket times then)
  (-> familiarity-ticket? (tableof trivial?)
    (-> (tableof familiarity-ticket?) extfx?)
    extfx?)
  (internal:extfx-ft-split-table ticket times then))

(define/contract (extfx-ft-subname ticket key then)
  (-> familiarity-ticket? name? (-> familiarity-ticket? extfx?)
    extfx?)
  (internal:extfx-ft-subname ticket key then))

(define/contract (extfx-ft-restrict ticket ds then)
  (-> familiarity-ticket? dspace? (-> familiarity-ticket? extfx?)
    extfx?)
  (internal:extfx-ft-restrict ticket ds then))


(define/contract (extfx-ft-disburse ds hub-name comp-ticket)
  (-> dspace? authorized-name?
    (-> (-> familiarity-ticket? extfx?) extfx?)
    extfx?)
  (internal:extfx-ft-disburse ds hub-name comp-ticket))

(define/contract (extfx-ft-claim ds t then)
  (-> dspace? familiarity-ticket? (-> familiarity-ticket? extfx?)
    extfx?)
  (internal:extfx-ft-claim ds t then))


(define/contract
  (extfx-contribute ds collector-familiarity-ticket comp)
  (-> dspace? familiarity-ticket?
    (-> (-> optionally-dexable? extfx?) extfx?)
    extfx?)
  (internal:extfx-contribute ds collector-familiarity-ticket comp))

(define/contract (extfx-collect ds collector-name then)
  (-> dspace? authorized-name? (-> table? extfx?) extfx?)
  (internal:extfx-collect ds collector-name then))
