#lang parendown racket/base

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> any any/c chaperone-contract? contract? contract-name
  flat-contract? list/c listof or/c)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract contract-first-order-passes?
  make-chaperone-contract make-contract make-flat-contract
  raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/hash hash-ref-maybe)
(require #/only-in lathe-comforts/list
  list-bind list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/maybe just nothing)
(require #/only-in lathe-comforts/struct istruct/c struct-easy)
(require #/only-in lathe-comforts/trivial trivial trivial?)

(require #/only-in effection/order eq-by-dex?)
(require #/only-in effection/order/base
  dexable dexableof dex-dex dex-name fuse? name? name-of ordering-eq
  table? table-empty table-get tableof table-shadow valid-dexable?)
(require #/prefix-in unsafe: #/only-in effection/order/unsafe
  fuse gen:furge-internals name table)


; TODO: Finish implementing each of these exports, and document them.
(provide
  
  extfx?
  dspace?
  
  error-definer?
  error-definer-uninformative
  error-definer-from-message
  success-or-error-definer?
  success-or-error-definer
  
  ticket?
  intuitionistic-ticket?
  continuation-ticket?
  continuation-ticket-of
  familiarity-ticket?
  
  extfx-noop
  fuse-extfx
  extfx-later
  extfx-table-each
  
  extfx-dspace-create-shadower
  
  authorized-name?
  authorized-name-get-name
  name-subname
  authorized-name-subname
  extfx-claim-unique
  
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
  
  extfx-freshen
  extfx-split-list
  extfx-split-table
  extfx-disburse
  extfx-imburse
  extfx-ct-continue
  extfx-ft-subname
  extfx-ft-restrict
  
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
  
  (provide-struct (error-definer-uninformative))
  (provide-struct (error-definer-from-message message))
  (provide-struct (success-or-error-definer on-error on-success))
  
  (provide-struct (authorized-name ds name parents))
  
  (provide-struct (optionally-dexable-once v))
  (provide-struct
    (optionally-dexable-dexable dex value name-of-dex name-of-value))
  
  (provide-struct (pub ds pubsub-name))
  (provide-struct (sub ds pubsub-name))
  
  (provide-struct
    (continuation-ticket ticket-symbol on-unspent ds then))
  (provide-struct (familiarity-ticket ticket-symbol on-unspent ds n))
  
  
  (provide-struct (extfx-noop))
  (provide-struct (extfx-fused a b))
  (provide-struct (extfx-later then))
  (provide-struct (extfx-table-each t on-element then))
  
  (provide-struct (extfx-dspace-create-shadower ds then))
  
  (provide-struct
    (extfx-claim-unique
      n on-conflict on-familiarity-ticket-unspent then))
  
  (provide-struct (extfx-put ds n on-cont-unspent comp))
  (provide-struct (extfx-get ds n on-stall then))
  
  (provide-struct
    (extfx-private-put
      ds putter-name getter-name on-cont-unspent comp))
  (provide-struct
    (extfx-private-get ds putter-name getter-name on-stall then))
  
  (provide-struct (extfx-establish-pubsub ds pubsub-name then))
  (provide-struct (extfx-pub ds p pubber-name on-conflict arg))
  (provide-struct (extfx-sub ds s subber-name on-conflict func))
  
  (provide-struct (extfx-freshen ticket on-conflict then))
  (provide-struct (extfx-split-list ticket times on-conflict then))
  (provide-struct
    (extfx-split-table ticket times on-conflict then))
  (provide-struct
    (extfx-disburse ds hub-name on-cont-unspent comp-ticket))
  (provide-struct
    (extfx-imburse ds hub-familiarity-ticket on-conflict then))
  (provide-struct (extfx-ct-continue ticket on-conflict value))
  (provide-struct (extfx-ft-subname ticket key on-conflict then))
  (provide-struct (extfx-ft-restrict ticket ds on-conflict then))
  
  (provide-struct
    (extfx-contribute
      ds collector-familiarity-ticket on-cont-unspent comp))
  (provide-struct (extfx-collect ds collector-name then))
  
  
  (provide-struct (run-extfx-errors errors))
  
  
  )

(require #/prefix-in internal: 'private)



; TODO FRIENDLIER ERRORS:
;
; We should consider being more proactive about handling errors
; (especially so that Cene for Racket can use this implementation but
; replace its errors with a Cene concept of errors). Here's a rundown
; of all the remaining errors we should let the user provide handlers
; for.
;
; Note that we don't mention any errors which the user could work
; around by performing checks on their values before passing them in.
; Effectively, those can be handled already.
;
; Also, although we say think of these as "handlers," if an error ever
; occurs, we should encourage (if not totally enforce) that the user
; "handle" it merely by tailoring the error message. Tailored error
; messages could send us down an extravagant path if we let
; them -- maybe even using error-handling-time definition spaces to
; communicate across concurrently occurring errors; adding ways for a
; caller to analyze the errors the called code produces and replace
; them with fewer and more focused errors; and adding ways to
; twist-tie (so to speak) a bunch of unspent tickets into a single
; unspent ticket which has custom error behavior when it fails to be
; spent. All in all, these might be better termed "error customizers"
; than "error handlers." The term we use in the code here is
; `error-definer?`.
;
; We've actually whittled this list down to one particular kind of
; error at this point. This kind of error might be avoidable by
; letting the user perform more detailed checks on their values.
;
;
; pub-restrict
;   errors where the given definition space isn't a descendant of the
;     given pub
; sub-restrict
;   errors where the given definition space isn't a descendant of the
;     given sub
; extfx-ft-restrict
;   errors where the given definition space isn't a descendant of the
;     given familiarity ticket



(define (make-appropriate-chaperone-contract c)
  (if (chaperone-contract? c)
    make-chaperone-contract
    make-contract))

; TODO: This is also defined privately in `effection/order/base`. See
; if we can extract it into a library or something.
(define (make-appropriate-contract c)
  (if (flat-contract? c)
    make-flat-contract
  #/make-appropriate-chaperone-contract c))



(struct-easy (extfx-finish-put ds n on-conflict value))

(struct-easy (extfx-finish-run ds value))

(define/contract (extfx? v)
  (-> any/c boolean?)
  
  (mat v (internal:extfx-noop) #t
  #/mat v (internal:extfx-fused a b) #t
  #/mat v (internal:extfx-later then) #t
  #/mat v (internal:extfx-table-each t on-element then) #t
  
  #/mat v (internal:extfx-dspace-create-shadower ds then) #t
  
  #/mat v
    (internal:extfx-claim-unique
      n on-conflict on-familiarity-ticket-unspent then)
    #t
  
  #/mat v (internal:extfx-put ds n on-cont-unspent comp) #t
  #/mat v (extfx-finish-put ds n on-conflict value) #t
  #/mat v (internal:extfx-get ds n on-stall then) #t
  
  #/mat v
    (internal:extfx-private-put
      ds putter-name getter-name on-cont-unspent comp)
    #t
  #/mat v
    (internal:extfx-private-get
      ds putter-name getter-name on-stall then)
    #t
  
  #/mat v (internal:extfx-establish-pubsub ds pubsub-name then) #t
  #/mat v (internal:extfx-pub ds p pubber-name on-conflict arg) #t
  #/mat v (internal:extfx-sub ds s subber-name on-conflict func) #t
  
  #/mat v (internal:extfx-freshen ticket on-conflict then) #t
  #/mat v (internal:extfx-split-list ticket times on-conflict then) #t
  #/mat v (internal:extfx-split-table ticket times on-conflict then)
    #t
  #/mat v
    (internal:extfx-disburse ds hub-name on-cont-unspent comp-ticket)
    #t
  #/mat v
    (internal:extfx-imburse
      ds hub-familiarity-ticket on-conflict then)
    #t
  #/mat v (internal:extfx-ct-continue ticket on-conflict value) #t
  #/mat v (internal:extfx-ft-subname ticket key on-conflict then) #t
  #/mat v (internal:extfx-ft-restrict ticket ds on-conflict then) #t
  
  #/mat v
    (internal:extfx-contribute
      ds collector-familiarity-ticket on-cont-unspent comp)
    #t
  #/mat v (internal:extfx-collect ds collector-name then) #t
  
  #/mat v (extfx-finish-run ds value) #t
  
    #f))

(define/contract (dspace? v)
  (-> any/c boolean?)
  (internal:dspace? v))


(define/contract (error-definer? v)
  (-> any/c boolean?)
  (mat v (internal:error-definer-uninformative) #t
  #/mat v (internal:error-definer-from-message message) #t
    #f))

; TODO FRIENDLIER ERRORS: Add more expressive ways to create
; `error-definer?` values. It seems like in general, they should be
; similar to top-level Cene definitions (i.e. `extfx?`-returning
; functions which take a unique `authorized-name?` and a
; `(-> name? authorized-name?)` qualify function), but with the
; distinction that the information they define is only used to
; construct a good error message.

(define/contract (error-definer-uninformative)
  (-> error-definer?)
  (internal:error-definer-uninformative))

(define/contract (error-definer-from-message message)
  (-> string? error-definer?)
  (internal:error-definer-from-message message))

; TODO: See if we should export this.
(define/contract (error-definer-or-message ed message)
  (-> error-definer? string? error-definer?)
  (expect ed (internal:error-definer-uninformative) ed
  #/internal:error-definer-from-message message))

; NOTE: In our interfaces here, the point of
; `success-or-error-definer?` values is not only to let conflicts be
; reported, but to let certain `extfx?` computations run only after
; the avoidance of a conflict has been (at least provisionally)
; confirmed. Using these success handlers is about as easy as using
; `fuse-extfx` to run the other `extfx?` computations concurrently,
; but this way we save the program from the trouble of running those
; computations if we already know there's an error.

(define/contract (success-or-error-definer? v)
  (-> any/c boolean?)
  (internal:success-or-error-definer? v))

(define/contract (success-or-error-definer on-error on-success)
  (-> error-definer? extfx? success-or-error-definer?)
  (internal:success-or-error-definer on-error on-success))


(define/contract (ticket? v)
  (-> any/c boolean?)
  (mat v
    (internal:continuation-ticket ticket-symbol on-unspent ds then)
    #t
  #/mat v (internal:familiarity-ticket ticket-symbol on-unspent ds n)
    #t
    #f))

(define/contract (intuitionistic-ticket? v)
  (-> any/c boolean?)
  (mat v (internal:familiarity-ticket ticket-symbol on-unspent ds n)
    #t
    #f))

(define/contract (continuation-ticket? v)
  (-> any/c boolean?)
  (internal:continuation-ticket? v))

(define/contract (continuation-ticket-of c)
  (-> contract? contract?)
  (w- c (coerce-contract 'continuation-ticket-of c)
  #/ (make-appropriate-chaperone-contract c)
    
    #:name `(continuation-ticket-of ,(contract-name c))
    
    #:first-order
    (fn v
      (contract-first-order-passes?
        (istruct/c internal:continuation-ticket any/c any/c any/c
          (-> c any))
        v))
    
    #:late-neg-projection
    (fn blame
      (w- c-late-neg-projection
        ( (get/build-late-neg-projection c)
          (blame-add-context blame #:swap? #t
            "the anticipated value of"))
      #/fn v missing-party
        (expect v
          (internal:continuation-ticket
            ticket-symbol on-unspent ds then)
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a continuation ticket" given: "~e")
            v)
        #/internal:continuation-ticket ticket-symbol on-unspent ds
          (fn result
            (c-late-neg-projection result missing-party)))))))

(define/contract (familiarity-ticket? v)
  (-> any/c boolean?)
  (internal:familiarity-ticket? v))


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
  (-> table?
    (-> any/c
    #/list/c error-definer? #/-> continuation-ticket? extfx?)
    (-> table? extfx?)
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
; `dspace-eq?`. At the moment we don't offer any way (or at least any
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

; NOTE: The `authorized-name?` and the `familiarity-ticket?` passed
; into the body are for the same `name?`. The `authorized-name?`
; allows owner-style access to state resources using this name and its
; subnames, and the `familiarity-ticket?` allows making
; closed-world assumption (CWA) contributions to those state
; resources. Since the resources are all fresh, the body receives full
; permissions over them, and it can subdivide these permissions as
; needed for various access control policies.
;
(define/contract
  (extfx-claim-unique
    n on-conflict on-familiarity-ticket-unspent then)
  (-> authorized-name? error-definer? error-definer?
    (-> authorized-name? familiarity-ticket? extfx?)
    extfx?)
  (internal:extfx-claim-unique
    n on-conflict on-familiarity-ticket-unspent then))


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


(define/contract (extfx-put ds n on-cont-unspent comp)
  (-> dspace? authorized-name? error-definer?
    (->
      (continuation-ticket-of
      #/list/c success-or-error-definer? optionally-dexable?)
      extfx?)
    extfx?)
  (internal:extfx-put ds n on-cont-unspent comp))

(define/contract (extfx-get ds n on-stall then)
  (-> dspace? name? error-definer? (-> any/c extfx?) extfx?)
  (internal:extfx-get ds n on-stall then))


(define/contract
  (extfx-private-put ds putter-name getter-name on-cont-unspent comp)
  (-> dspace? authorized-name? name? error-definer?
    (->
      (continuation-ticket-of
      #/list/c success-or-error-definer? optionally-dexable?)
      extfx?)
    extfx?)
  (internal:extfx-private-put
    ds putter-name getter-name on-cont-unspent comp))

(define/contract
  (extfx-private-get ds putter-name getter-name on-stall then)
  (-> dspace? name? authorized-name? error-definer? (-> any/c extfx?)
    extfx?)
  (internal:extfx-private-get
    ds putter-name getter-name on-stall then))


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

(define/contract (extfx-pub ds p pubber-name on-conflict arg)
  (->
    dspace?
    pub?
    authorized-name?
    success-or-error-definer?
    optionally-dexable?
    extfx?)
  (internal:extfx-pub ds p pubber-name on-conflict arg))

(define/contract (extfx-sub ds s subber-name on-conflict func)
  (-> dspace? sub? authorized-name? success-or-error-definer?
    (optionally-dexable-of #/-> any/c extfx?)
    extfx?)
  (internal:extfx-sub ds s subber-name on-conflict func))


(define/contract (extfx-freshen ticket on-conflict then)
  (-> ticket? error-definer? (-> ticket? extfx?) extfx?)
  (internal:extfx-freshen ticket on-conflict then))

(define/contract (extfx-split-list ticket times on-conflict then)
  (-> intuitionistic-ticket? natural? error-definer?
    (-> (listof intuitionistic-ticket?) extfx?)
    extfx?)
  (internal:extfx-split-list ticket times on-conflict then))

(define/contract (extfx-split-table ticket times on-conflict then)
  (-> intuitionistic-ticket? (tableof trivial?) error-definer?
    (-> (tableof intuitionistic-ticket?) extfx?)
    extfx?)
  (internal:extfx-split-table ticket times on-conflict then))

(define/contract
  (extfx-disburse ds hub-name on-cont-unspent comp-ticket)
  (-> dspace? authorized-name? error-definer?
    (->
      (continuation-ticket-of
      #/list/c success-or-error-definer? intuitionistic-ticket?)
      extfx?)
    extfx?)
  (internal:extfx-disburse ds hub-name on-cont-unspent comp-ticket))

(define/contract
  (extfx-imburse ds hub-familiarity-ticket on-conflict then)
  (-> dspace? familiarity-ticket? error-definer?
    (-> intuitionistic-ticket? extfx?)
    extfx?)
  (internal:extfx-imburse ds hub-familiarity-ticket on-conflict then))

(define/contract (extfx-ct-continue ticket on-conflict value)
  (-> continuation-ticket? error-definer? any/c extfx?)
  (internal:extfx-ct-continue ticket on-conflict value))

(define/contract (extfx-ft-subname ticket key on-conflict then)
  (-> familiarity-ticket? name? error-definer?
    (-> familiarity-ticket? extfx?)
    extfx?)
  (internal:extfx-ft-subname ticket key on-conflict then))

(define/contract (extfx-ft-restrict ticket ds on-conflict then)
  (-> familiarity-ticket? dspace? error-definer?
    (-> familiarity-ticket? extfx?)
    extfx?)
  (internal:extfx-ft-restrict ticket ds on-conflict then))


(define/contract
  (extfx-contribute
    ds collector-familiarity-ticket on-cont-unspent comp)
  (-> dspace? familiarity-ticket? error-definer?
    (->
      (continuation-ticket-of
      #/list/c success-or-error-definer? optionally-dexable?)
      extfx?)
    extfx?)
  (internal:extfx-contribute
    ds collector-familiarity-ticket on-cont-unspent comp))

(define/contract (extfx-collect ds collector-name then)
  (-> dspace? authorized-name? (-> table? extfx?) extfx?)
  (internal:extfx-collect ds collector-name then))


(struct-easy (run-extfx-result-success value) #:equal)
(struct-easy (run-extfx-result-failure errors) #:equal)

(define/contract (run-extfx-errors? v)
  (-> any/c boolean?)
  (internal:run-extfx-errors? v))


(struct-easy
  (unspent-ticket-entry-familiarity-ticket on-unspent ds n))
(struct-easy (unspent-ticket-entry-anonymous on-unspent))
(struct-easy (db-put-entry-do-not-conflict existing-values))
(struct-easy (db-put-entry-written existing-value))

; TODO: Clients can abuse a call to `run-extfx!` just to generate a
; fresh `name?` value for use outside that call. If there's anything
; we can do to prevent that, let's consider doing it.
(define/contract (run-extfx! on-continuation-ticket-unspent body)
  (-> error-definer?
    (-> dspace? authorized-name? continuation-ticket? extfx?)
    (or/c
      (istruct/c run-extfx-result-failure run-extfx-errors?)
      (istruct/c run-extfx-result-success any/c)))
  (w- root-ds-symbol (gensym)
  #/w- root-ds (internal:dspace root-ds-symbol (list) (hasheq))
  #/w- root-unique-name
    (unsafe:name #/list 'name:root-unique-name root-ds-symbol)
  #/w- root-unique-authorized-name
    (internal:authorized-name root-ds root-unique-name (table-empty))
  #/w- root-continuation-ticket-symbol (gensym)
  #/w- root-continuation-ticket
    (internal:continuation-ticket
      root-continuation-ticket-symbol
      on-continuation-ticket-unspent
      root-ds
      (fn result
        (extfx-finish-run root-ds result)))
  #/w-loop next-full
    
    processes
    (list
    #/body root-ds root-unique-authorized-name
      root-continuation-ticket)
    
    rev-next-processes (list)
    
    unspent-tickets
    (hasheq
      root-continuation-ticket-symbol
      (unspent-ticket-entry-anonymous on-continuation-ticket-unspent))
    
    db
    (hasheq
      'claim-unique (table-empty)
      'put (hasheq)
      'finish-run (nothing))
    
    rev-errors (list)
    did-something #f
    
    (expect processes (cons process processes)
      (mat rev-next-processes (list)
        ; If there are no processes left, we're done. We add errors
        ; corresponding to any unspent tickets, and we return either
        ; the collection of errors (if there are any) or `finish-run`
        ; value (otherwise).
        (w- rev-errors
          (append
            (list-map (hash->list unspent-tickets)
            #/dissectfn (cons ticket-symbol entry)
              (mat entry
                (unspent-ticket-entry-familiarity-ticket
                  on-unspent ds n)
                on-unspent
              #/mat entry (unspent-ticket-entry-anonymous on-unspent)
                on-unspent
              #/error "Internal error: Encountered an unrecognized unspent ticket entry"))
            rev-errors)
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
          (list-bind rev-next-processes #/fn process
            (mat process (internal:extfx-get ds n on-stall then)
              (list #/error-definer-or-message on-stall
                "Read from a name that was never defined")
            #/mat process
              (internal:extfx-private-get
                ds putter-name getter-name on-stall then)
              (list #/error-definer-or-message on-stall
                "Read from a private name that was never defined")
            #/mat process
              (internal:extfx-collect ds collector-name then)
              ; NOTE: These processes can't stall unless there's still
              ; an unspent ticket at large, so the error message for
              ; that unspent ticket can describe a more proximal cause
              ; than we can here.
              (list)
            #/error "Internal error: Encountered a stalled process that shouldn't have been able to stall"))
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
    #/w- next-with-error-definer
      (fn on-error default-message
        (next-full
          processes rev-next-processes unspent-tickets db
          (cons (error-definer-or-message on-error default-message)
            rev-errors)
          #t))
    #/w- next-with-error
      (fn error
        (next-with-error-definer
          (internal:error-definer-uninformative)
          error))
    
    
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
    
    #/mat process
      (internal:extfx-claim-unique
        n on-conflict on-familiarity-ticket-unspent then)
      (expect (authorized-name-for? root-ds n) #t
        (next-with-error "Expected n to be a name authorized for the extfx runner's root definition space")
      #/w- n (authorized-name-get-name n)
      #/w- db-claim-unique (hash-ref db 'claim-unique)
      #/expect (table-get n db-claim-unique) (nothing)
        (next-with-error-definer on-conflict
          "Tried to claim a name unique twice")
      #/w- fresh-ticket-symbol (gensym)
      #/w- fresh-name
        (unsafe:name #/list 'name:claim-unique-result root-ds-symbol)
      #/w- fresh-authorized-name
        (internal:authorized-name root-ds fresh-name (table-empty))
      #/w- familiarity-ticket-symbol (gensym)
      #/w- on-familiarity-ticket-unspent
        (error-definer-or-message on-familiarity-ticket-unspent
          "Expected an extfx-put continuation to be continued")
      #/w- familiarity-ticket
        (internal:familiarity-ticket
          familiarity-ticket-symbol on-familiarity-ticket-unspent
          
          ; NOTE: This creates a familiarity ticket for `root-ds`,
          ; even if the part of the program that performs an
          ; `extfx-claim-unique` effect is otherwise localized to a
          ; descendant shadowing definition space.
          ;
          root-ds
          
          fresh-name)
      #/next-full
        (cons (then fresh-authorized-name familiarity-ticket)
          processes)
        rev-next-processes
        (hash-set unspent-tickets familiarity-ticket-symbol
          (unspent-ticket-entry-familiarity-ticket
            on-familiarity-ticket-unspent root-ds fresh-name))
        (hash-set db 'claim-unique
          (table-shadow n (just #/trivial) db-claim-unique))
        rev-errors #t)
    
    #/mat process (internal:extfx-put ds n on-cont-unspent comp)
      ; TODO: Modify `(hash-ref db 'put)` here so that we know that
      ; this value will be put by this process. That way, we'll be
      ; able to focus the error message by removing "couldn't read"
      ; errors if the corresponding writing processes had their own
      ; errors.
      (expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ds to be a definition space descending from the extfx runner's root definition space")
      #/expect (authorized-name-for? root-ds n) #t
        (next-with-error "Expected n to be a name authorized for the extfx runner's root definition space")
      #/w- continuation-ticket-symbol (gensym)
      #/w- on-cont-unspent
        (error-definer-or-message on-cont-unspent
          "Expected an extfx-put continuation to be continued")
      #/w- continuation-ticket
        (internal:continuation-ticket
          continuation-ticket-symbol on-cont-unspent root-ds
          (dissectfn (list on-conflict value)
            (extfx-finish-put ds n on-conflict value)))
      #/next-full
        (cons (comp continuation-ticket) processes)
        rev-next-processes
        (hash-set unspent-tickets continuation-ticket
          (unspent-ticket-entry-anonymous on-cont-unspent))
        db rev-errors #t)
    #/mat process (extfx-finish-put ds n on-conflict value)
      ; If there has already been a definition installed at this name
      ; in this definition space, this checks that the proposed dex
      ; matches the stored dex and that the proposed value matches the
      ; stored value according to that dex. Otherwise, it stores the
      ; proposed dex and value without question.
      (expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ds to be a definition space descending from the extfx runner's root definition space")
      #/expect (authorized-name-for? root-ds n) #t
        (next-with-error "Expected n to be a name authorized for the extfx runner's root definition space")
      #/dissect on-conflict
        (internal:success-or-error-definer on-conflict on-no-conflict)
      #/dissect ds
        (internal:dspace ds-symbol parents-list parents-hash)
      #/w- n (authorized-name-get-name n)
      #/w- err-once
        (fn
          (next-with-error-definer on-conflict
            "Wrote to the same name where at least one of the writes was only expecting one write overall"))
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
                (next-with-error-definer on-conflict
                  "Wrote to the same name where two of the writes were dexable with different dexes")
              #/expect
                (eq-by-dex? (dex-name)
                  name-of-existing-value
                  name-of-value)
                #t
                (next-with-error-definer on-conflict
                  "Wrote to the same name where two of the writes were dexable with different values")
              #/then)
            #/error "Internal error: Encountered an unknown kind of optionally dexable value")
          #/error "Internal error: Encountered an unknown kind of optionally dexable value"))
      #/w- db-put (hash-ref db 'put)
      #/w- check-ds-symbol
        (fn then
          (expect (hash-ref-maybe db-put ds-symbol)
            (just db-put-for-ds)
            (then #f)
          #/expect (table-get n db-put-for-ds) (just entry)
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
            #/expect (table-get n db-put-for-ds) (just entry)
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
          #/expect (table-get n db-put-for-ds) (just entry)
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
            #/expect (table-get n db-put-for-ds) (just entry)
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
        (cons on-no-conflict processes)
        rev-next-processes unspent-tickets
        (hash-set db 'put db-put)
        rev-errors #t)
    #/mat process (internal:extfx-get ds n on-stall then)
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
      (internal:extfx-private-put
        ds putter-name getter-name on-cont-unspent comp)
      'TODO
    #/mat process
      (internal:extfx-private-get
        ds putter-name getter-name on-stall then)
      'TODO
    
    #/mat process
      (internal:extfx-establish-pubsub ds pubsub-name then)
      ; TODO: See if we really need to enforce that `ds` descends from
      ; `root-ds` here.
      (expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ds to be a definition space descending from the extfx runner's root definition space")
      #/expect (authorized-name-for? root-ds pubsub-name) #t
        (next-with-error "Expected pubsub-name to be a name authorized for the extfx runner's root definition space")
      #/next-one-fruitful #/then
        (internal:pub ds pubsub-name)
        (internal:sub ds pubsub-name))
    #/mat process
      (internal:extfx-pub ds p pubber-name on-conflict arg)
      'TODO
    #/mat process
      (internal:extfx-sub ds s subber-name on-conflict func)
      'TODO
    
    #/w- parse-ticket
      (fn ticket
        (mat ticket
          (internal:continuation-ticket
            ticket-symbol on-unspent ds then)
          (list
            ticket-symbol
            ds
            (unspent-ticket-entry-anonymous on-unspent)
            (fn new-ticket-symbol
              (internal:continuation-ticket
                new-ticket-symbol on-unspent ds then)))
        #/mat ticket
          (internal:familiarity-ticket ticket-symbol on-unspent ds n)
          (list
            ticket-symbol
            ds
            (unspent-ticket-entry-familiarity-ticket on-unspent ds n)
            (fn new-ticket-symbol
              (internal:familiarity-ticket
                new-ticket-symbol on-unspent ds n)))
        #/error "Internal error: Encountered an unrecognized ticket value"))
    #/mat process (internal:extfx-freshen ticket on-conflict then)
      (dissect (parse-ticket ticket)
        (list ticket-symbol ds entry wrap-fresh)
      #/expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ticket to be a ticket descending from the extfx runner's root definition space")
      #/expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error-definer on-conflict
          "Tried to spend a ticket twice")
      #/w- fresh-ticket-symbol (gensym)
      #/next-full
        (cons (then #/wrap-fresh fresh-ticket-symbol) processes)
        rev-next-processes
        (hash-set
          (hash-remove unspent-tickets ticket-symbol)
          fresh-ticket-symbol
          entry)
        db rev-errors #t)
    #/mat process
      (internal:extfx-split-list ticket times on-conflict then)
      (dissect (parse-ticket ticket)
        (list ticket-symbol ds entry wrap-fresh)
      #/expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ticket to be a ticket descending from the extfx runner's root definition space")
      #/expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error-definer on-conflict
          "Tried to spend a ticket twice")
      #/w-loop next
        unspent-tickets (hash-remove unspent-tickets ticket-symbol)
        result (list)
        
        (expect (nat->maybe times) (just times)
          (next-full
            (cons (then result) processes)
            rev-next-processes unspent-tickets db rev-errors #t)
        #/w- fresh-ticket-symbol (gensym)
        #/next
          (hash-set unspent-tickets fresh-ticket-symbol entry)
          (cons (wrap-fresh fresh-ticket-symbol) result)))
    #/mat process
      (internal:extfx-split-table ticket times on-conflict then)
      (dissect (parse-ticket ticket)
        (list ticket-symbol ds entry wrap-fresh)
      #/expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ticket to be a familiarity ticket descending from the extfx runner's root definition space")
      #/dissect times (unsafe:table times)
      #/next-one-fruitful
      #/internal:extfx-split-list
        ticket (hash-count times) on-conflict
      #/fn tickets
      #/then #/unsafe:table #/make-immutable-hash
      #/list-zip-map (hash->list times) tickets #/fn time ticket
        (dissect time (cons k #/trivial)
        #/cons k ticket))
    #/mat process
      (internal:extfx-disburse
        ds hub-name on-cont-unspent comp-ticket)
      'TODO
    #/mat process
      (internal:extfx-imburse
        ds hub-familiarity-ticket on-conflict then)
      'TODO
    #/mat process
      (internal:extfx-ct-continue ticket on-conflict value)
      (dissect ticket
        (internal:continuation-ticket
          ticket-symbol on-unspent ds then)
      #/expect (dspace-eq? root-ds ds) #t
        (next-with-error "Expected ticket to be a continuation ticket associated with the extfx runner's root definition space")
      #/expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error-definer on-conflict
          "Tried to spend a ticket twice")
      #/next-full
        (cons (then value) processes)
        rev-next-processes
        (hash-remove unspent-tickets ticket-symbol)
        db rev-errors #t)
    #/mat process
      (internal:extfx-ft-subname ticket key on-conflict then)
      (dissect ticket
        (internal:familiarity-ticket ticket-symbol on-unspent ds n)
      #/expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ticket to be a familiarity ticket descending from the extfx runner's root definition space")
      #/expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error-definer on-conflict
          "Tried to spend a ticket twice")
      #/w- fresh-ticket-symbol (gensym)
      #/w- n (name-subname key n)
      #/next-full
        (cons
          (then
          #/internal:familiarity-ticket
            fresh-ticket-symbol on-unspent ds n)
          processes)
        rev-next-processes
        (hash-set
          (hash-remove unspent-tickets ticket-symbol)
          fresh-ticket-symbol
          (unspent-ticket-entry-familiarity-ticket on-unspent ds n))
        db rev-errors #t)
    #/mat process
      (internal:extfx-ft-restrict ticket new-ds on-conflict then)
      (dissect ticket
        (internal:familiarity-ticket ticket-symbol on-unspent ds n)
      #/expect (dspace-descends? root-ds ds) #t
        (next-with-error "Expected ticket to be a familiarity ticket descending from the extfx runner's root definition space")
      #/expect (dspace-descends? ds new-ds) #t
        (next-with-error "Expected ds to be a definition space descending from ticket's definition space")
      #/expect (hash-has-key? unspent-tickets ticket-symbol) #t
        (next-with-error-definer on-conflict
          "Tried to spend a ticket twice")
      #/w- fresh-ticket-symbol (gensym)
      #/next-full
        (cons
          (then
          #/internal:familiarity-ticket
            fresh-ticket-symbol on-unspent new-ds n)
          processes)
        rev-next-processes
        (hash-set
          (hash-remove unspent-tickets ticket-symbol)
          fresh-ticket-symbol
          (unspent-ticket-entry-familiarity-ticket
            on-unspent new-ds n))
        db rev-errors #t)
    
    #/mat process
      (internal:extfx-contribute
        ds collector-familiarity-ticket on-cont-unspent comp)
      'TODO
    #/mat process (internal:extfx-collect ds collector-name then)
      'TODO
    
    #/mat process (extfx-finish-run ds value)
      (expect (dspace-eq? root-ds ds) #t
        (next-with-error "Expected ds to be the extfx runner's root definition space")
      #/dissect (hash-ref db 'finish-run) (nothing)
      #/next-full
        processes rev-next-processes unspent-tickets
        (hash-set db 'finish-run (just value))
        rev-errors #t)
    
    #/error "Internal error: Expected process to be an extfx value")))
