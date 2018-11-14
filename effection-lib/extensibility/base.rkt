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

(require #/only-in lathe-comforts dissect expect fn mat w- w-loop)
(require #/only-in lathe-comforts/hash hash-ref-maybe)
(require #/only-in lathe-comforts/list list-map nat->maybe)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/struct istruct/c struct-easy)
(require #/only-in lathe-comforts/trivial trivial trivial?)

(require #/only-in effection/order eq-by-dex?)
(require #/only-in effection/order/base
  dexable dexableof dex-dex dex-name fuse? name? name-of ordering-eq
  table? table-empty table-get tableof table-shadow valid-dexable?)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe
  fuse gen:furge-internals name)


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

; TODO: Document this export, which winds up exported from
; `effection/extensibility/unsafe`.
(module+ private/unsafe #/provide run-extfx!)


(module private racket/base
  
  (require #/only-in lathe-comforts/struct struct-easy)
  
  
  (define-syntax-rule (provide-struct (name field ...) option ...)
    (begin
      (struct-easy (name field ...) option ...)
      (provide #/struct-out name)))
  
  
  (provide-struct (dspace ds-symbol parents-list parents-hash))
  
  
  (provide-struct (authorized-name ds name parents))
  
  
  (provide-struct (optionally-dexable-once v))
  (provide-struct
    (optionally-dexable-dexable dex value name-of-dex name-of-value))
  
  
  (provide-struct (pub ds pubsub-name))
  (provide-struct (sub ds pubsub-name))
  
  
  (provide-struct (familiarity-ticket ticket-symbol ds n))
  
  
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
  
  
  (provide-struct (run-extfx-errors errors))
  
  
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



(struct-easy (extfx-finish-put ds n value))

(struct-easy (extfx-spend ds ticket-symbol then))
(struct-easy (extfx-finish-run ds value))

(define/contract (extfx? v)
  (-> any/c boolean?)
  
  (mat v (internal:extfx-noop) #t
  #/mat v (internal:extfx-fused a b) #t
  #/mat v (internal:extfx-later then) #t
  #/mat v (internal:extfx-table-each t on-element then) #t
  
  #/mat v (internal:extfx-dspace-create-shadower ds then) #t
  
  #/mat v (internal:extfx-claim-and-split n times then) #t
  
  #/mat v (internal:extfx-put ds n comp) #t
  #/mat v (extfx-finish-put ds n value) #t
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
  
  #/mat v (extfx-spend ds ticket-symbol then) #t
  #/mat v (extfx-finish-run ds value) #t
  
    #f))

(define/contract (dspace? v)
  (-> any/c boolean?)
  (internal:dspace? v))


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

; TODO: See if this should be exported.
(define/contract (dspace-eq? a b)
  (-> dspace? dspace? boolean?)
  (dissect a (internal:dspace a-symbol _ _)
  #/dissect a (internal:dspace b-symbol _ _)
  #/eq? a-symbol b-symbol))

; TODO: See if this should be exported.
(define/contract (dspace-descends? ancestor descendant)
  (-> dspace? dspace? boolean?)
  (dissect ancestor (internal:dspace ancestor-symbol _ _)
  #/dissect descendant
    (internal:dspace descendant-symbol _ descendant-parents-hash)
  #/or
    (eq? ancestor-symbol descendant-symbol)
    (hash-has-key? descendant-parents-hash ancestor-symbol)))


(define/contract (authorized-name? v)
  (-> any/c boolean?)
  (internal:authorized-name? v))

; TODO: See if this should be exported.
;
; TODO: See if this should use `dspace-descends?` instead of
; `dspace-eq?`. At the moment don't offer any way (or at least any
; well thought-out way) to restrict an authorized name to a shadowing
; definition space, and if we did, we would have to figure out what it
; meant to claim the same authorized name in two separate shadowing
; definition spaces. But maybe we will figure that out, and once we
; do, we might know if this needs to check for `dspace-descends?`.
;
(define/contract (authorized-name-for? ds name)
  (-> dspace? authorized-name? boolean?)
  (dissect name (internal:authorized-name name-ds _ _)
  #/dspace-eq? ds name-ds))

(define/contract (authorized-name-get-name n)
  (-> authorized-name? name?)
  (dissect n (internal:authorized-name ds n parents)
    n))

(define/contract (name-subname key-name original-name)
  (-> name? name? name?)
  (dissect key-name (unsafe:name key-name)
  #/dissect original-name (unsafe:name original-name)
  
  ; TODO: The tag `name:subname` we're using here is identical to the
  ; one we're using for `sink-name-subname` in Cene for Racket.
  ; Fortunately, we're using it for the exact same purpose.
  ; Nevertheless, once we have this file working smoothly, we should
  ; modify Cene for Racket so it uses this rather than creating its
  ; own `name:subname` names.
  ;
  #/unsafe:name #/list 'name:subname key-name original-name))

(define/contract (authorized-name-subname key-name original-name)
  (-> name? authorized-name? authorized-name?)
  (dissect original-name
    (internal:authorized-name ds original-name parents)
  #/internal:authorized-name
    ds
    (name-subname key-name original-name)
    (table-shadow original-name (just #/trivial) parents)))

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
            any/c c any/c any/c))
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
        #/expect v
          (internal:optionally-dexable-dexable
            dex value name-of-dex name-of-value)
          v
        #/w- new-value (c-late-neg-projection value missing-party)
        #/expect (eq-by-dex? dex value new-value) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "an optionally dexable value which, if dexable, projected to something equal to the original by that dex" given: "~e")
            v)
        #/internal:optionally-dexable-dexable
          dex new-value name-of-dex name-of-value)))))

(define/contract (optionally-dexable-once v)
  (-> any/c optionally-dexable?)
  (internal:optionally-dexable-once v))

(define/contract (optionally-dexable-dexable v)
  (-> valid-dexable? optionally-dexable?)
  (dissect v (dexable dex value)
  #/dissect (name-of (dex-dex) dex) (just name-of-dex)
  #/dissect (name-of dex value) (just name-of-value)
  #/internal:optionally-dexable-dexable
    dex value name-of-dex name-of-value))

; TODO: See if we should export this.
(define/contract (optionally-dexable-value od)
  (-> optionally-dexable? any/c)
  (mat od (internal:optionally-dexable-once v) v
  #/dissect od
    (internal:optionally-dexable-dexable
      dex value name-of-dex name-of-value)
    value))


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
  (internal:pub? v))

(define/contract (sub? v)
  (-> any/c boolean?)
  (internal:sub? v))

(define/contract (pub-restrict ds p)
  (-> dspace? pub? pub?)
  (dissect p (internal:pub original-ds pubsub-name)
  #/expect (dspace-descends? original-ds ds) #t
    (error "Expected ds to be a shadowing descendant of the dspace of the pub p")
  #/internal:pub ds pubsub-name))

(define/contract (sub-restrict ds s)
  (-> dspace? sub? sub?)
  (dissect s (internal:sub original-ds pubsub-name)
  #/expect (dspace-descends? original-ds ds) #t
    (error "Expected ds to be a shadowing descendant of the dspace of the sub s")
  #/internal:sub ds pubsub-name))

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
  (internal:familiarity-ticket? v))

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


(struct-easy (run-extfx-result-success value) #:equal)
(struct-easy (run-extfx-result-failure errors) #:equal)

(define/contract (run-extfx-errors? v)
  (-> any/c boolean?)
  (internal:run-extfx-errors? v))

(struct-easy (unspent-ticket-entry-familiarity-ticket ds n))
(struct-easy (unspent-ticket-entry-anonymous))
(struct-easy (db-put-entry-do-not-conflict existing-values))
(struct-easy (db-put-entry-written existing-value))

; NOTE: The `authorized-name?` and the `familiarity-ticket?` passed
; into the body are for the same `name?`. The `authorized-name?`
; allows owner-style access to state resources using this name and its
; subnames, and the `familiarity-ticket?` allows making
; closed-world assumption (CWA) contributions to those state
; resources. Since the resources are all fresh, the body receives full
; permissions over them, and it can subdivide these permissions as
; needed for various access control policies.
;
; TODO: Clients can abuse a call to `run-extfx!` just to generate a
; fresh `name?` value for use outside that call. If there's anything
; we can do to prevent that, let's consider doing it.
;
(define/contract (run-extfx! body)
  (->
    (-> dspace? authorized-name? familiarity-ticket? (-> any/c extfx?)
      extfx?)
    (or/c
      (istruct/c run-extfx-result-failure run-extfx-errors?)
      (istruct/c run-extfx-result-success any/c)))
  (w- root-ds-symbol (gensym)
  #/w- root-ds (internal:dspace root-ds-symbol (list) (hasheq))
  #/w- root-unique-name
    (unsafe:name #/list 'name:root-unique-name root-ds-symbol)
  #/w- root-unique-authorized-name
    (internal:authorized-name root-ds root-unique-name (table-empty))
  #/w- root-familiarity-ticket-symbol (gensym)
  #/w- root-familiarity-ticket
    (internal:familiarity-ticket
      root-familiarity-ticket-symbol root-ds root-unique-name)
  #/w- finish-ticket-symbol (gensym)
  #/w-loop next-full
    
    processes
    (list #/body root-ds root-unique-authorized-name #/fn result
      (extfx-spend root-ds finish-ticket-symbol
      #/extfx-finish-run root-ds result))
    
    rev-next-processes (list)
    
    unspent-tickets
    (hasheq
      root-familiarity-ticket-symbol
      (unspent-ticket-entry-familiarity-ticket
        root-ds root-unique-name)
      finish-ticket-symbol
      (unspent-ticket-entry-anonymous))
    
    db (hasheq 'finish-run (nothing) 'put (hasheq))
    rev-errors (list)
    did-something #f
    
    (expect processes (cons process processes)
      (mat rev-next-processes (list)
        ; If there are no processes left, we're done. We check that
        ; there are no unspent tickets, and we return either the
        ; `finish-run` value or the collection of errors.
        (w- rev-errors
          (if (hash-empty? unspent-tickets)
            rev-errors
            (cons "Expected each ticket to be spent" rev-errors))
        #/mat rev-errors (list)
          (dissect (hash-ref db 'finish-run) (just finish-value)
          #/run-extfx-result-success finish-value)
          (run-extfx-result-failure #/internal:run-extfx-errors
          #/reverse rev-errors))
      #/if (not did-something)
        ; The processes are stalled. We log errors corresponding to
        ; all the processes.
        (run-extfx-result-failure #/internal:run-extfx-errors
        #/reverse #/append
          (list-map rev-next-processes #/fn process
            ; TODO: Generate different errors for different processes.
            "Read from a name that was never defined")
          rev-errors)
      #/next-full (reverse rev-next-processes) (list) unspent-tickets
        db rev-errors #f)
    #/w- next-simple
      (fn processes
        (next-full
          processes rev-next-processes unspent-tickets db rev-errors
          #t))
    #/w- next-zero
      (fn
        (next-simple processes))
    #/w- next-one-fruitful
      (fn process
        (next-simple #/cons process processes))
    #/w- next-fruitless
      (fn
        ; NOTE: We can't use `next-one-fruitful` here because we don't
        ; want to make `did-something` true if it isn't already.
        (next-full
          processes (cons process rev-next-processes) unspent-tickets
          db rev-errors did-something))
    #/w- next-with-error
      (fn error
        (next-full
          processes rev-next-processes unspent-tickets db
          (cons error rev-errors)
          #t))
    
    
    #/mat process (internal:extfx-noop)
      (next-zero)
    #/mat process (internal:extfx-fused a b)
      (next-simple #/list* b a processes)
    #/mat process (internal:extfx-later then)
      (next-one-fruitful #/then)
    #/mat process (internal:extfx-table-each t on-element then)
      'TODO
    
    #/mat process (internal:extfx-dspace-create-shadower ds then)
      (dissect ds
        (internal:dspace ds-symbol parents-list parents-hash)
      ; TODO: See if we really need to enforce that `ds` descends from
      ; `root-ds` here.
      #/expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ds to be a definition space descending from the extfx runner's root definition space")
      #/next-one-fruitful
      #/then #/internal:dspace (gensym)
        (cons ds-symbol parents-list)
        (hash-set parents-hash ds-symbol #t))
    
    #/mat process (internal:extfx-claim-and-split n times then)
      'TODO
    
    #/mat process (internal:extfx-put ds n comp)
      ; TODO: Modify `(hash-ref db 'put)` here so that we know that
      ; this value will be put by this process. That way, we'll be
      ; able to focus the error message by removing "couldn't read"
      ; errors if the corresponding writing processes had their own
      ; errors.
      (expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ds to be a definition space descending from the extfx runner's root definition space")
      #/expect (authorized-name-for? root-ds n) #t
        (next-with-error "Expected n to be a name authorized for the extfx runner's root definition space")
      #/w- ticket (gensym)
      #/next-full
        processes
        (cons
          (comp #/fn value
            (extfx-spend root-ds ticket
            #/extfx-finish-put ds n value))
          rev-next-processes)
        (hash-set unspent-tickets ticket
          (unspent-ticket-entry-anonymous))
        db rev-errors #t)
    #/mat process (extfx-finish-put ds n value)
      ; If there has already been a definition installed at this name
      ; in this definition space, this checks that the proposed dex
      ; matches the stored dex and that the proposed value matches the
      ; stored value according to that dex. Otherwise, it stores the
      ; proposed dex and value without question.
      (expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ds to be a definition space descending from the extfx runner's root definition space")
      #/expect (authorized-name-for? root-ds n) #t
        (next-with-error "Expected n to be a name authorized for the extfx runner's root definition space")
      #/dissect ds
        (internal:dspace ds-symbol parents-list parents-hash)
      #/w- n (authorized-name-get-name n)
      #/w- err-once
        (fn
          (next-with-error "Wrote to the same name where at least one of the writes was only expecting one write overall"))
      #/w- do-not-conflict
        (fn existing-value then
          (mat existing-value
            (internal:optionally-dexable-once existing-value)
            (err-once)
          #/mat existing-value
            (internal:optionally-dexable-dexable
              existing-dex
              existing-value
              name-of-existing-dex
              name-of-existing-value)
            (mat value (internal:optionally-dexable-once value)
              (err-once)
            #/mat value
              (internal:optionally-dexable-dexable
                dex value name-of-dex name-of-value)
              (expect
                (eq-by-dex? (dex-name)
                  name-of-existing-dex
                  name-of-dex)
                #t
                (next-with-error "Wrote to the same name where two of the writes were dexable with different dexes")
              #/expect
                (eq-by-dex? (dex-name)
                  name-of-existing-value
                  name-of-value)
                #t
                (next-with-error "Wrote to the same name where two of the writes were dexable with different values")
              #/then)
            #/error "Internal error: Encountered an unknown kind of optionally dexable value")
          #/error "Internal error: Encountered an unknown kind of optionally dexable value"))
      #/w- db-put (hash-ref db 'put)
      #/w- check-ds-symbol
        (fn then
          (expect (hash-ref-maybe db-put ds-symbol)
            (just db-put-for-ds)
            (then #f)
          #/expect (table-get db-put-for-ds n) (just entry)
            (then #f)
          #/mat entry (db-put-entry-do-not-conflict existing-values)
            (w-loop next existing-values existing-values
              (expect existing-values
                (cons existing-value existing-values)
                (then #f)
              #/do-not-conflict existing-value #/fn
                (next existing-values)))
          #/mat entry (db-put-entry-written existing-value)
            (do-not-conflict existing-value #/fn
              (then #t))
          #/error "Internal error: Encountered an unknown kind of db put entry"))
      #/check-ds-symbol #/fn already-written
      #/if already-written
        (next-zero)
      #/w- check-parents
        (fn then
          (w-loop next parents-to-check parents-list
            (expect parents-to-check (cons parent parents-to-check)
              (then #f)
            #/expect (hash-ref-maybe db-put parent)
              (just db-put-for-ds)
              (next parents-to-check)
            #/expect (table-get db-put-for-ds n) (just entry)
              (next parents-to-check)
            #/mat entry (db-put-entry-do-not-conflict existing-values)
              (next parents-to-check)
            #/mat entry (db-put-entry-written existing-value)
              
              ; NOTE: If we find a `db-put-entry-written` entry for
              ; even one of the parents, checking that one is enough
              ; to check all the parents. That's why we can proceed
              ; with `(then #t)` instead of `(next parents-to-check)`
              ; here.
              ;
              (do-not-conflict existing-value #/fn
                (then #t))
            
            #/error "Internal error: Encountered an unknown kind of db put entry")))
      #/check-parents #/fn already-written
      #/if already-written
        (next-zero)
      #/w- write-ds-symbol
        (fn db-put then
          (w- then-with-db-put-for-ds
            (fn db-put-for-ds
              (then #/hash-set db-put ds-symbol
                (table-shadow n (just #/db-put-entry-written value)
                  db-put-for-ds)))
          #/expect (hash-ref-maybe db-put ds-symbol)
            (just db-put-for-ds)
            (then-with-db-put-for-ds #/table-empty)
          #/expect (table-get db-put-for-ds n) (just entry)
            (then-with-db-put-for-ds db-put-for-ds)
          #/mat entry (db-put-entry-do-not-conflict existing-values)
            (then-with-db-put-for-ds db-put-for-ds)
          #/mat entry (db-put-entry-written existing-value)
            (then db-put)
          #/error "Internal error: Encountered an unknown kind of db-put entry"))
      #/write-ds-symbol db-put #/fn db-put
      #/w- write-parents
        (fn db-put then
          (w-loop next parents-to-write parents-list db-put db-put
            (expect parents-to-write (cons parent parents-to-write)
              (then db-put)
            #/w- next-with-db-put-for-ds-and-existing-values
              (fn db-put-for-ds existing-values
                (next parents-to-write #/hash-set db-put ds-symbol
                  (table-shadow n
                    (just #/db-put-entry-do-not-conflict
                    #/cons value existing-values)
                    db-put-for-ds)))
            #/expect (hash-ref-maybe db-put parent)
              (just db-put-for-ds)
              (next-with-db-put-for-ds-and-existing-values
                (table-empty)
                (list))
            #/expect (table-get db-put-for-ds n) (just entry)
              (next-with-db-put-for-ds-and-existing-values
                db-put-for-ds
                (list))
            #/mat entry (db-put-entry-do-not-conflict existing-values)
              (next-with-db-put-for-ds-and-existing-values
                db-put-for-ds
                existing-values)
            #/mat entry (db-put-entry-written existing-value)
              (error "Internal error: Expected already-written to become true if any of the parents had db-put-entry-written")
            #/error "Internal error: Encountered an unknown kind of db-put entry")))
      #/write-parents db-put #/fn db-put
      #/next-full
        processes rev-next-processes unspent-tickets
        (hash-set db 'put db-put)
        rev-errors #t)
    #/mat process (internal:extfx-get ds n then)
      ; If there has not yet been a definition installed at this name
      ; in this definition space, we set this process aside and come
      ; back to it later. If there has, we call `then` with that
      ; defined value and set aside its result as a process to come
      ; back to later.
      (expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ds to be a definition space descending from the extfx runner's root definition space")
      #/dissect ds
        (internal:dspace ds-symbol parents-list parents-hash)
      #/w-loop next places-to-check (cons ds-symbol parents-list)
        (expect places-to-check (cons place places-to-check)
          (next-fruitless)
        #/expect (hash-ref-maybe (hash-ref db 'put) place)
          (just db-put-for-ds)
          (next places-to-check)
        #/expect (table-get n db-put-for-ds) (just value)
          (next places-to-check)
        #/expect value (db-put-entry-written existing-value)
          (next places-to-check)
        #/next-one-fruitful #/then #/optionally-dexable-value value))
    
    #/mat process
      (internal:extfx-private-put ds putter-name getter-name comp)
      'TODO
    #/mat process
      (internal:extfx-private-get ds putter-name getter-name then)
      'TODO
    
    #/mat process
      (internal:extfx-establish-pubsub ds pubsub-name then)
      'TODO
    #/mat process (internal:extfx-pub ds p pubber-name arg)
      'TODO
    #/mat process (internal:extfx-pub ds s subber-name func)
      'TODO
    
    #/mat process (internal:extfx-ft-split-list ticket times then)
      (dissect ticket
        (internal:familiarity-ticket ticket-symbol ds n)
      #/expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ticket to be a familiarity ticket descending from the extfx runner's root definition space")
      #/expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error "Tried to spend a ticket twice")
      #/w-loop next
        unspent-tickets (hash-remove unspent-tickets ticket-symbol)
        result (list)
        
        (expect (nat->maybe times) (just times)
          (next-full
            (cons (then result) processes)
            rev-next-processes unspent-tickets db rev-errors #t)
        #/w- fresh-ticket-symbol (gensym)
        #/next
          (hash-set unspent-tickets fresh-ticket-symbol
            (unspent-ticket-entry-familiarity-ticket ds n))
          (cons (internal:familiarity-ticket fresh-ticket-symbol ds n)
            result)))
    #/mat process (internal:extfx-ft-split-table ticket times then)
      'TODO
    #/mat process (internal:extfx-ft-subname ticket key then)
      'TODO
    #/mat process (internal:extfx-ft-restrict ticket ds then)
      'TODO
    
    #/mat process (internal:extfx-ft-disburse ds hub-name comp-ticket)
      'TODO
    #/mat process (internal:extfx-ft-claim ds t then)
      'TODO
    
    #/mat process
      (internal:extfx-contribute ds collector-familiarity-ticket comp)
      'TODO
    #/mat process (internal:extfx-collect ds collector-name then)
      'TODO
    
    #/mat process (extfx-spend ds ticket-symbol then)
      (expect (dspace-eq? root-ds ds) #t
        (next-with-error "Expected ds to be the extfx runner's root definition space")
      #/expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error "Tried to spend a ticket twice")
      #/next-full
        processes rev-next-processes
        (hash-remove unspent-tickets ticket-symbol)
        db rev-errors #t)
    #/mat process (extfx-finish-run ds value)
      (expect (dspace-eq? root-ds ds) #t
        (next-with-error "Expected ds to be the extfx runner's root definition space")
      #/dissect (hash-ref db 'finish-run) (nothing)
      #/next-full
        processes rev-next-processes unspent-tickets
        (hash-set db 'finish-run (just value))
        rev-errors #t)
    
    #/error "Internal error: Expected process to be an extfx value")))
