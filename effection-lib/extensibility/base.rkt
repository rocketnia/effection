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
(require #/only-in lathe-comforts/list list-map)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/struct istruct/c struct-easy)
(require #/only-in lathe-comforts/trivial trivial?)

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

; TODO: Document these exports, which wind up exported from
; `effection/extensibility/unsafe`.
(module+ private/unsafe #/provide
  new-dspace!
  run-extfx!)
;
; NOTE: The `private/unsafe` submodule exports the `authorized-name`
; struct, which also ends up being exported from
; `effection/extensibility/unsafe`.
;
; TODO: Is there a safer way to obtain a root authorized name (e.g.
; having `run-extfx!` create one itself to pass into its body)?


(module private racket/base
  
  (require #/only-in lathe-comforts/struct struct-easy)
  
  ; TODO: Document this export.
  (module+ unsafe #/provide #/struct-out authorized-name)
  
  
  (define-syntax-rule (provide-struct (name field ...) option ...)
    (begin
      (struct-easy (name field ...) option ...)
      (provide #/struct-out name)))
  
  
  (provide-struct (dspace ds-symbol parents))
  
  
  (provide-struct (authorized-name name))
  
  
  (provide-struct (optionally-dexable-once v))
  (provide-struct
    (optionally-dexable-dexable dex value name-of-dex name-of-value))
  
  
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

(struct-easy (extfx-spend ticket-symbol then))
(struct-easy (extfx-finish-run value))

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
  
  #/mat v (extfx-spend ticket-symbol then) #t
  #/mat v (extfx-finish-run value) #t
  
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


(define/contract (authorized-name? v)
  (-> any/c boolean?)
  (internal:authorized-name? v))

(define/contract (authorized-name-get-name n)
  (-> authorized-name? name?)
  (dissect n (internal:authorized-name n)
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
  (dissect original-name (internal:authorized-name original-name)
  #/internal:authorized-name #/name-subname key-name original-name))

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


; TODO: This is an export of `effection/extensibility/unsafe` right
; now, but we may need to refactor it a bit more before it's stable,
; since `run-extfx!` is far from being fully implemented. Is there a
; safer way to obtain a root definition space (e.g. having
; `run-extfx!` create one itself to pass into its body)?
;
(define/contract (new-dspace!)
  (-> dspace?)
  (internal:dspace (gensym) #/list))


(struct-easy (run-extfx-result-success value) #:equal)
(struct-easy (run-extfx-result-failure errors) #:equal)

(define/contract (run-extfx-errors? v)
  (-> any/c boolean?)
  (internal:run-extfx-errors? v))

(struct-easy (db-put-entry-do-not-conflict existing-values))
(struct-easy (db-put-entry-written existing-value))

; TODO: This is an export of `effection/extensibility/unsafe` right
; now, but we may need to refactor it a bit more before it's stable,
; since it's far from being fully implemented. Is there a safer way to
; design this (e.g. a way to prohibit the result value from containing
; lingering definition spaces, ticket values, authorized names, pubs,
; subs, etc.)?
;
(define/contract (run-extfx! body)
  (-> (-> (-> any/c extfx?) extfx?)
    (or/c
      (istruct/c run-extfx-result-failure run-extfx-errors?)
      (istruct/c run-extfx-result-success any/c)))
  (w- finish-ticket (gensym)
  #/w-loop next-full
    
    processes
    (list #/body #/fn result
      (extfx-spend finish-ticket #/extfx-finish-run result))
    
    rev-next-processes (list)
    unspent-tickets (hasheq finish-ticket #t)
    finish-value (nothing)
    db-put (hasheq)
    rev-errors (list)
    did-something #f
    
    (expect processes (cons process processes)
      (mat rev-next-processes (list)
        ; If there are no processes left, we're done. We check that
        ; there are no unspent tickets, and we return either the
        ; finish value or the collection of errors.
        (w- rev-errors
          (if (hash-empty? unspent-tickets)
            rev-errors
            (cons "Expected each ticket to be spent" rev-errors))
        #/mat rev-errors (list)
          (dissect finish-value (just finish-value)
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
        finish-value db-put rev-errors #f)
    #/w- next-simple
      (fn rev-next-processes
        (next-full
          processes rev-next-processes unspent-tickets finish-value
          db-put rev-errors #t))
    #/w- next-with-error
      (fn error
        (next-full
          processes rev-next-processes unspent-tickets finish-value
          db-put (cons error rev-errors) #t))
    #/w- next-fruitless
      (fn
        (next-full
          processes (cons process rev-next-processes) unspent-tickets
          finish-value db-put rev-errors did-something))
    
    
    #/mat process (internal:extfx-noop)
      (next-simple rev-next-processes)
    #/mat process (internal:extfx-fused a b)
      (next-simple #/list* b a rev-next-processes)
    #/mat process (internal:extfx-later then)
      (next-simple #/cons (then) rev-next-processes)
    #/mat process (internal:extfx-table-each t on-element then)
      'TODO
    
    #/mat process (internal:extfx-dspace-create-shadower ds then)
      (dissect ds (internal:dspace ds-symbol parents)
      #/next-simple #/cons
        (then #/internal:dspace (gensym) #/cons ds-symbol parents)
        rev-next-processes)
    
    #/mat process (internal:extfx-claim-and-split n times then)
      'TODO
    
    #/mat process (internal:extfx-put ds n comp)
      ; TODO: Modify `db-put` here so that we know that this value
      ; will be put by this process. That way, we'll be able to focus
      ; the error message by removing "couldn't read" errors if the
      ; corresponding writing processes had their own errors.
      (w- ticket (gensym)
      #/next-full
        processes
        (cons
          (comp #/fn value
            (extfx-spend ticket #/extfx-finish-put ds n value))
          rev-next-processes)
        (hash-set unspent-tickets ticket #t)
        finish-value db-put rev-errors #t)
    #/mat process (extfx-finish-put ds n value)
      ; If there has already been a definition installed at this name
      ; in this definition space, this checks that the proposed dex
      ; matches the stored dex and that the proposed value matches the
      ; stored value according to that dex. Otherwise, it stores the
      ; proposed dex and value without question.
      (dissect ds (internal:dspace ds-symbol parents)
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
          #/error "Internal error: Encountered an unknown kind of db-put entry"))
      #/check-ds-symbol #/fn already-written
      #/if already-written
        (next-simple rev-next-processes)
      #/w- check-parents
        (fn then
          (w-loop next parents-to-check parents
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
              ; with `then` instead of `(fn #/next parents-to-check)`
              ; here.
              ;
              (do-not-conflict existing-value #/fn
                (then #t))
            
            #/error "Internal error: Encountered an unknown kind of db-put entry")))
      #/check-parents #/fn already-written
      #/if already-written
        (next-simple rev-next-processes)
      #/w- write-ds-symbol
        (fn db-put then
          (w- then-with-db-put-for-ds
            (fn db-put-for-ds
              (then #/hash-set db-put ds-symbol
                (table-shadow n
                  (just #/db-put-entry-written value)
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
          (w-loop next parents-to-write parents db-put db-put
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
        processes rev-next-processes unspent-tickets finish-value
        db-put rev-errors #t)
    #/mat process (internal:extfx-get ds n then)
      ; If there has not yet been a definition installed at this name
      ; in this definition space, we set this process aside and come
      ; back to it later. If there has, we call `then` with that
      ; defined value and set aside its result as a process to come
      ; back to later.
      (dissect ds (internal:dspace ds-symbol parents)
      #/w- n (authorized-name-get-name n)
      #/w-loop next places-to-check (cons ds-symbol parents)
        (expect places-to-check (cons place places-to-check)
          (next-fruitless)
        #/expect (hash-ref-maybe db-put place) (just db-put-for-ds)
          (next places-to-check)
        #/expect (table-get n db-put-for-ds) (just value)
          (next places-to-check)
        #/expect value (db-put-entry-written existing-value)
          (next places-to-check)
        #/next-simple #/cons
          (then #/optionally-dexable-value value)
          rev-next-processes))
    
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
      'TODO
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
    
    #/mat process (extfx-spend ticket-symbol then)
      (expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error "Tried to spend a ticket twice")
      #/next-full
        processes rev-next-processes
        (hash-remove unspent-tickets ticket-symbol)
        finish-value db-put rev-errors #t)
    #/mat process (extfx-finish-run value)
      (dissect finish-value (nothing)
      #/next-full
        processes rev-next-processes unspent-tickets
        (just value)
        db-put rev-errors #t)
    
    #/error "Internal error: Expected process to be an extfx value")))
