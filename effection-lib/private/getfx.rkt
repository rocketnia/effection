#lang parendown racket/base

; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> ->i any any/c contract? contract-name contract-out list/c listof
  none/c or/c rename-contract)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract contract-first-order-passes?
  make-contract make-flat-contract raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/hash hash-union)
(require #/only-in racket/math natural?)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w- w-loop)
(require #/only-in lathe-comforts/hash
  hash-ref-maybe hash-v-all hash-v-any hash-v-map)
(require #/only-in lathe-comforts/list
  list-any list-bind list-foldl list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/match match/c)
(require #/only-in lathe-comforts/maybe
  just maybe? maybe-bind nothing)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct istruct/c
  struct-easy)
(require #/only-in lathe-comforts/trivial trivial trivial?)

(require #/only-in effection/order dex-trivial eq-by-dex? table-v-of)
(require #/only-in effection/order/base
  dex? dex-dex dexed? dexed/c dexed-first-order/c dexed-get-name
  dexed-get-value dex-name fuse? name? name-of ordering-eq table?
  table-empty? table-empty table-get table-shadow)
(require #/only-in (submod effection/order/base private)
  dex-internals-simple-dexed-of)

(require #/prefix-in unsafe: #/only-in effection/order/unsafe
  dex fuse gen:dex-internals gen:furge-internals name table)


(provide #/all-defined-out)


(module private racket/base
  
  (require #/only-in lathe-comforts/struct struct-easy)
  
  
  (define-syntax-rule (provide-struct (name field ...) option ...)
    (begin
      (struct-easy (name field ...) option ...)
      (provide #/struct-out name)))
  
  
  (provide-struct (error-definer-uninformative))
  (provide-struct (error-definer-from-message message))
  
  
  (provide-struct (getfx-done result))
  (provide-struct (getfx-bind effects then))
  (provide-struct (getfx-err on-execute))
  
  ; NOTE: We define this here so we can define `getfx?`, but we really
  ; only finish defining it in `effection/extensibility/base`.
  (provide-struct (getfx-get ds n on-stall))
  
  ; NOTE: We define this here so we can define `getfx?`, but we really
  ; only finish defining it in `effection/extensibility/base`.
  (provide-struct
    (getfx-private-get ds putter-name getter-name on-stall))
  
  )

(require #/prefix-in internal: 'private)
(provide #/all-from-out 'private)


; An `error-definer?` is a way of specifying a custom error message.
; Although all the ways of constructing `error-definer?` values are
; currently very simple, they may someday (TODO) perform more
; sophisticated computations to produce holistic error reports.
;
; NOTE:
;
; Once they do this, it may be tempting to call them "error handlers".
; However, they cannot be used to recover from an error. They can only
; produce an error report.
;
; The Effection extensibility process calculus depends on monotonicity
; of all state resources to ensure the backwards compatibility of each
; extension. If Effection-safe computations had a way to recover from
; all errors, then a computation could positively depend on the
; *presence* of an error, even an error that results from the *lack*
; of some definition or an *incmplete* implementation, meaning that
; the very act of implementing an unimplemented thing could break
; backwards compatibility. We can't very well disallow implementing
; things, so we disallow recovering from errors instead.

; TODO:
;
; Add more expressive ways to create `error-definer?` values. It seems
; like in general, they should be similar to top-level Cene
; definitions (i.e. `extfx?`-returning functions which take a unique
; `authorized-name?` and a `(-> name? authorized-name?)` name
; qualification function), but with the distinction that the
; information they define is only used to construct a detailed and
; focused error report.
;
; Treating them as *services* (i.e. top-level definitions which have
; familiarity tickets for each other) this way would make it possible
; for them to coordinate to produce *simpler* error reports than they
; could produce independently. However, for them to obtain familiarity
; tickets for each other, we'll need to create variations of
; `extfx-split-list`, `extfx-split-table`, and `extfx-disburse` which
; take their own top-level definitions that act like phone operator
; switchboards to allow cousin unspent ticket errors to connect with
; each other. We may also need variations of `fuse-extfx` and
; `extfx-table-each` which do the same kind of thing to allow
; concurrent processes' error definers to coordinate with each other,
; as well as possibly some more effects (unlike any we currently have)
; which allow concurrent errors and unspent ticket errors to interact
; with each other.
;
; It's possible we may also want a way to twist-tie (so to speak) some
; ticket values so that their unspent ticket errors are managed
; together. Perhaps in order to do this, we could hide them all inside
; a single ticket value until it's unwrapped again, but it seems like
; we might just be able to install this kind of connection using a
; side effect without changing the way we pass the tickets around.

(define (error-definer? v)
  (mat v (internal:error-definer-uninformative) #t
  #/mat v (internal:error-definer-from-message message) #t
    #f))

(define (error-definer-uninformative)
  (internal:error-definer-uninformative))

(define (error-definer-from-message message)
  (internal:error-definer-from-message message))

; TODO: See if we should export this.
(define/contract (error-definer-or-message ed message)
  (-> error-definer? string? error-definer?)
  (expect ed (internal:error-definer-uninformative) ed
  #/internal:error-definer-from-message message))


(define (getfx? v)
  (mat v (internal:getfx-done result) #t
  #/mat v (internal:getfx-bind effects then) #t
  #/mat v (internal:getfx-err on-execute) #t
  
  #/mat v (internal:getfx-get ds n on-stall) #t
  
  #/mat v
    (internal:getfx-private-get ds putter-name getter-name on-stall)
    #t
  
    #f))

(define (getfx-done result)
  (internal:getfx-done result))

(define (getfx-bind effects then)
  (internal:getfx-bind effects then))

(define (getfx-err on-execute)
  (internal:getfx-err on-execute))

; TODO: See if we should export this from `effection/extensibility`.
(define (getfx-map effects func)
  (getfx-bind effects #/fn result
  #/getfx-done #/func result))

(define (getfx/c c)
  (w- c (coerce-contract 'getfx/c c)
  #/make-contract
    
    #:name `(getfx/c ,(contract-name c))
    
    #:first-order (fn v #/getfx? v)
    
    #:late-neg-projection
    (fn blame
      (w- c-late-neg-projection
        ( (get/build-late-neg-projection c)
          (blame-add-context blame #:swap? #t
            "the anticipated value of"))
      #/fn v missing-party
        (expect (getfx? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a getfx effectful computation" given: "~e")
            v)
        #/getfx-map v #/fn result
          (c-late-neg-projection result missing-party))))))
