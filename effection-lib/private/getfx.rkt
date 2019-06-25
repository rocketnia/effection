#lang parendown racket/base


(require #/for-syntax #/only-in syntax/parse expr)


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
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect expectfn fn mat w- w-loop)
(require #/only-in lathe-comforts/hash
  hash-ref-maybe hash-v-all hash-v-any hash-v-map)
(require #/only-in lathe-comforts/list
  list-any list-bind list-foldl list-map list-zip-map nat->maybe)
(require #/only-in lathe-comforts/match match/c)
(require #/only-in lathe-comforts/maybe
  just maybe? maybe-bind maybe/c nothing)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct istruct/c
  struct-easy)
(require #/only-in lathe-comforts/trivial trivial trivial?)


(provide #/all-defined-out)


(module private racket/base
  
  (require #/only-in lathe-comforts/struct struct-easy)
  
  
  (define-syntax-rule (provide-struct (name field ...) option ...)
    (begin
      (struct-easy (name field ...) option ...)
      (provide #/struct-out name)))
  
  
  (provide-struct (error-definer-uninformative))
  (provide-struct (error-definer-from-message message))
  (provide-struct (error-definer-from-exn exn))
  
  
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
  #/mat v (internal:error-definer-from-exn exn) #t
    #f))

(define (error-definer-uninformative)
  (internal:error-definer-uninformative))

(define (error-definer-from-message message)
  (internal:error-definer-from-message message))

(define (error-definer-from-exn exn)
  (internal:error-definer-from-exn exn))

; TODO: See if we should export this.
;
; TODO: Make a corresponding `error-definer-or-exn`. Consider
; migrating all uses of this one to that one.
;
(define/contract (error-definer-or-message ed message)
  (-> error-definer? string? error-definer?)
  (expect ed (internal:error-definer-uninformative) ed
  #/internal:error-definer-from-message message))

; TODO: See if we should export this.
(define/contract (raise-error-definer error-definer)
  (-> error-definer? none/c)
  (mat error-definer (internal:error-definer-uninformative)
    ; TODO: See if we should make this more informative.
    (error "error")
  #/mat error-definer (internal:error-definer-from-message message)
    ; TODO: See if we should make this more informative, like being a
    ; specific kind of exception.
    (error message)
  #/dissect error-definer (internal:error-definer-from-exn exn)
    (raise exn)))


(define (getfx? v)
  (mat v (internal:getfx-done result) #t
  #/mat v (internal:getfx-bind effects then) #t
  #/mat v (internal:getfx-err on-execute) #t
  
  #/mat v (internal:getfx-get ds n on-stall) #t
  
  #/mat v
    (internal:getfx-private-get ds putter-name getter-name on-stall)
    #t
  
    #f))

(define (pure-run-getfx effects)
  (mat effects (internal:getfx-done result) result
  #/mat effects (internal:getfx-bind effects then)
    (pure-run-getfx #/then #/pure-run-getfx effects)
  #/mat effects (internal:getfx-err on-execute)
    (raise-error-definer on-execute)
  
  #/mat effects (internal:getfx-get ds n on-stall)
    ; TODO: See if we should use `on-stall` here.
    (raise-arguments-error 'pure-run-getfx
      "expected a getfx computation that did not perform a getfx-get"
      "ds" ds
      "n" n
      "on-stall" on-stall)
  
  #/dissect effects
    (internal:getfx-private-get ds putter-name getter-name on-stall)
    ; TODO: See if we should use `on-stall` here.
    (raise-arguments-error 'pure-run-getfx
      "expected a getfx computation that did not perform a getfx-private-get"
      "ds" ds
      "putter-name" putter-name
      "getter-name" getter-name
      "on-stall" on-stall)))

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

(define (getfx/c result/c)
  (w- result/c (coerce-contract 'getfx/c result/c)
  #/make-contract
    
    #:name `(getfx/c ,(contract-name result/c))
    
    #:first-order (fn v #/getfx? v)
    
    #:late-neg-projection
    (fn blame
      (w- result/c-late-neg-projection
        ( (get/build-late-neg-projection result/c)
          (blame-add-context blame #:swap? #t
            "the anticipated value of"))
      #/fn v missing-party
        (expect (getfx? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a getfx effectful computation" given: "~e")
            v)
        #/getfx-map v #/fn result
          (result/c-late-neg-projection result missing-party))))))

(define/contract (getfx-err-unraise-fn body)
  (-> (-> any) getfx?)
  (dissect
    (with-handlers ([exn:fail? (fn e #/list #t e)])
      (list #f #/call-with-values body #/fn results results))
    (list okay result)
  #/expect okay #t
    (raise-arguments-error 'getfx-err-unraise
      "expected the body to raise an exception rather than return values"
      "return-values" result)
  #/getfx-err #/error-definer-from-exn result))

; TODO: See if we should export this.
(define-simple-macro (getfx-err-unraise body:expr)
  (getfx-err-unraise-fn #/fn body))

; TODO: See if we should export this.
(define/contract (getmaybefx-bind effects then)
  (-> (getfx/c maybe?) (-> any/c #/getfx/c maybe?) #/getfx/c maybe?)
  (getfx-bind effects #/fn maybe-intermediate
  #/expect maybe-intermediate (just intermediate)
    (getfx-done #/nothing)
  #/then intermediate))

; TODO: See if we should export this from somewhere.
(define/contract (monad-list-map fx-done fx-bind list-of-fx)
  (-> (-> any/c any/c) (-> any/c (-> any/c any/c) any/c) list? any/c)
  (w-loop next rest list-of-fx rev-result (list)
    (expect rest (cons fx-first rest)
      (fx-done #/reverse rev-result)
    #/fx-bind fx-first #/fn first
    #/next rest (cons first rev-result))))

; TODO: See if we should export this.
(define/contract (getfx-list-map list-of-getfx)
  (-> (listof getfx?) #/getfx/c list?)
  (monad-list-map
    (fn result #/getfx-done result)
    (fn effects then #/getfx-bind effects then)
    list-of-getfx))

; TODO: See if we should export this.
(define/contract (getmaybefx-list-map list-of-getmaybefx)
  (-> (listof #/getfx/c maybe?) #/getfx/c #/maybe/c list?)
  (monad-list-map
    (fn result #/getfx-done #/just result)
    (fn effects then #/getmaybefx-bind effects then)
    list-of-getmaybefx))
