#lang parendown racket/base


(require #/for-syntax racket/base)
(require #/for-syntax #/only-in racket/struct-info
  extract-struct-info struct-info?)
(require #/for-syntax #/only-in racket/contract/base -> any/c listof)
(require #/for-syntax #/only-in racket/contract/region
  define/contract)
(require #/for-syntax #/only-in syntax/parse expr id nat syntax-parse)

(require #/for-syntax #/only-in lathe-comforts
  dissect expect fn mat w- w-loop)
(require #/for-syntax #/only-in lathe-comforts/list
  list-all list-kv-map list-map)


; NOTE: The Racket documentation says `get/build-late-neg-projection`
; is in `racket/contract/combinator`, but it isn't. It's in
; `racket/contract/base`. Since it's also in `racket/contract` and the
; documentation correctly says it is, we require it from there.
(require #/only-in racket/contract get/build-late-neg-projection)
(require #/only-in racket/contract/base
  -> and/c any any/c case-> cons/c contract? contract-name
  contract-out hash/c list/c listof none/c rename-contract)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract contract-first-order
  contract-first-order-passes? raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/hash hash-union)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/hash
  hash-ref-maybe hash-set-maybe hash-v-all hash-v-any hash-v-map)
(require #/only-in lathe-comforts/list
  list-all list-any list-bind list-map)
(require #/only-in lathe-comforts/match match/c)
(require #/only-in lathe-comforts/maybe
  just just? just-value maybe? maybe-bind maybe/c maybe-map nothing
  nothing?)
(require #/only-in lathe-comforts/struct
  auto-write define-imitation-simple-struct struct-easy)

(require #/only-in effection/private/order
  cline-result? dex-result? lt-autocline lt-autodex
  names-autocline-candid names-autodex
  make-appropriate-non-chaperone-contract name?
  object-identities-autodex
  
  ordering-eq ordering-eq?
  
  ordering-private ordering-private?
  
  ordering-gt ordering-gt?
  
  ordering-lt ordering-lt?
  )
(require #/only-in effection/private/getfx
  getfx? getfx-bind getfx/c getfx-done getfx-err-unraise
  getfx-list-map getmaybefx-bind getmaybefx-list-map monad-list-map
  pure-run-getfx)
(require #/prefix-in internal: #/only-in
  effection/private/order-unsafe
  
  cline cline? cline-internals-autodex cline-internals-autoname
  cline-internals-compare cline-internals-dex cline-internals-in?
  cline-internals-tag dex dex? dex-internals? dex-internals-autodex
  dex-internals-autoname dex-internals-compare dex-internals-dexed-of
  dex-internals-in? dex-internals-tag dex-internals-name-of
  furge-internals-autodex furge-internals-autoname furge-internals-tag
  fusable-function fusable-function? fuse fuse? gen:cline-internals
  gen:dex-internals gen:furge-internals getfx-furge-internals-call
  merge merge? name table table?)


; ==== Orderings ====

(provide ordering-lt ordering-eq ordering-private ordering-gt)
(provide #/contract-out
  [ordering-lt? (-> any/c boolean?)]
  [ordering-eq? (-> any/c boolean?)]
  [ordering-private? (-> any/c boolean?)]
  [ordering-gt? (-> any/c boolean?)])
(provide dex-result? cline-result?)


; ==== Names, dexes, and dexed values ====

(provide name?)

(provide dex?)
(module+ private/unsafe #/provide
  autoname-dex)
(provide #/contract-out
  [in-dex? (-> dex? any/c boolean?)]
  [name-of (-> dex? any/c #/maybe/c name?)]
  [dexed-of (-> dex? any/c #/maybe/c dexed?)]
  [compare-by-dex (-> dex? any/c any/c #/maybe/c dex-result?)])
(module+ private/order #/provide
  eq-by-dex?)
(module+ private #/provide
  dex-internals-simple-dexed-of)

(module+ private/unsafe #/provide
  dexed)
(provide #/contract-out
  [dexed? (-> any/c boolean?)]
  [dexed/c (-> contract? contract?)]
  [dexed-first-order/c (-> contract? contract?)]
  [dexed-get-dex (-> dexed? dex?)]
  [dexed-get-name (-> dexed? name?)]
  [dexed-get-value (-> dexed? any/c)])

(provide dex-name dex-dex dex-dexed)

(provide
  dex-give-up
  dex-default
  dex-opaque
  dex-by-own-method
  dex-fix
  dex-struct-by-field-position
  dex-struct)

(module+ private/unsafe #/provide
  (struct-out dex-by-own-method::raise-different-methods-error)
  (struct-out dex-by-own-method::get-method)
  dex-by-own-method-delegate/c
  dex-by-own-method-thorough)


; ==== Clines ====

(provide cline?)
(module+ private/unsafe #/provide
  autoname-cline)
(provide get-dex-from-cline in-cline? compare-by-cline)
(provide dex-cline)

(provide
  cline-by-dex
  cline-give-up
  cline-default
  cline-opaque
  cline-by-own-method
  cline-fix
  cline-struct-by-field-position
  cline-struct
  cline-flip)

(module+ private/unsafe #/provide
  (struct-out cline-by-own-method::raise-different-methods-error)
  (struct-out cline-by-own-method::get-method)
  cline-by-own-method-delegate/c
  cline-by-own-method-thorough)


; ==== Merges and fuses ====

(provide
  merge?
  fuse?)
(module+ private/unsafe #/provide
  autoname-merge
  autoname-fuse)
(provide
  getfx-call-merge
  getfx-call-fuse)
(provide
  dex-merge
  dex-fuse)

(provide
  fuse-by-merge)

(provide
  merge-by-dex
  merge-by-cline-min
  merge-by-cline-max)

(provide
  merge-opaque
  merge-by-own-method
  merge-fix
  merge-struct-by-field-position
  merge-struct)
(module+ private/unsafe #/provide
  (struct-out merge-by-own-method::getfx-err-different-input-methods)
  (struct-out merge-by-own-method::getfx-err-cannot-get-output-method)
  (struct-out merge-by-own-method::getfx-err-different-output-method)
  (struct-out merge-by-own-method::getfx-get-method)
  merge-by-own-method-delegate/c
  merge-by-own-method-thorough)
(provide
  fuse-opaque
  fuse-by-own-method
  fuse-fix
  fuse-struct-by-field-position
  fuse-struct)
(module+ private/unsafe #/provide
  (struct-out fuse-by-own-method::getfx-err-different-input-methods)
  (struct-out fuse-by-own-method::getfx-err-cannot-get-output-method)
  (struct-out fuse-by-own-method::getfx-err-different-output-method)
  (struct-out fuse-by-own-method::getfx-get-method)
  fuse-by-own-method-delegate/c
  fuse-by-own-method-thorough)


; ==== Tables ====

(provide
  table? table-empty? table-get table-empty table-shadow
  getfx-table-map-fuse table-sort)
(module+ private/order #/provide
  assocs->table-if-mutually-unique)
(module+ private/unsafe #/provide
  table->sorted-list)
(provide dex-table merge-table fuse-table)


; ==== Fusable functions ====

(provide
  fusable-function? make-fusable-function fuse-fusable-function)
(module+ private/unsafe #/provide
  (struct-out fuse-fusable-function::getfx-err-cannot-combine-results)
  (struct-out fuse-fusable-function::getfx-arg-to-method)
  fuse-fusable-function-delegate/c
  fuse-fusable-function-thorough)


; ==== Boolean clines and contracts ====

(module+ private #/provide
  define-datum-dex
  define-datum-cline)
(module+ private/order #/provide
  
  dex-boolean
  cline-boolean-by-truer
  cline-boolean-by-falser
  merge-boolean-by-and
  merge-boolean-by-or
  
  table-kv-map
  table-kv-all?
  table-kv-any?
  table-v-map
  table-v-all?
  table-v-any?
  
  )
(provide
  table-v-of)



; ===== Miscellaneous utilities ======================================

; TODO: See if we should export this from somewhere.
(define/contract (monad-map fx-done fx-bind effects func)
  (->
    (-> any/c any/c)
    (-> any/c (-> any/c any/c) any/c)
    any/c
    (-> any/c any/c)
    any/c)
  (fx-bind effects #/fn intermediate
  #/fx-done #/func intermediate))

(begin-for-syntax
  
  (define/contract (proper-syntax-pair? x)
    (-> any/c boolean?)
    (mat x (list) #t
    #/mat x (cons first rest)
      (and (syntax? first) #/proper-syntax-pair? rest)
    #/and (syntax? x) #/proper-syntax-pair? #/syntax-e x))
  
  (define/contract (desyntax-list syntax-list)
    (-> proper-syntax-pair? #/listof syntax?)
    (w-loop next syntax-list syntax-list
      (mat syntax-list (list) (list)
      #/mat syntax-list (cons first rest)
        (cons first #/next rest)
      #/next #/syntax-e syntax-list)))
)

(define-for-syntax (get-immutable-root-ancestor-struct-info stx id)
  (w- struct-info (syntax-local-value id)
  #/expect (struct-info? struct-info) #t
    (raise-syntax-error #f
      "not an identifier with struct-info attached"
      stx id)
  #/dissect (extract-struct-info struct-info)
    (list struct:foo make-foo foo? getters setters super)
  #/expect super #t
    (raise-syntax-error #f
      (w-loop next super super
        (mat super #f
          ; The super-type is unknown.
          "not the root super-type in a structure type hierarchy"
        #/dissect (extract-struct-info #/syntax-local-value super)
          (list _ _ _ _ _ super-super)
        #/expect super-super #t (next super-super)
        ; There is no super-type beyond this one.
        #/format
          "not the root super-type in a structure type hierarchy, which in this case would be ~s"
          (syntax-e super)))
      stx id)
  #/expect (list-all getters #/fn getter #/not #/eq? #f getter) #t
    (raise-syntax-error #f
      "not a structure type with all of its getters available"
      stx id)
  #/expect (list-all setters #/fn setter #/eq? #f setter) #t
    (raise-syntax-error #f
      "not an immutable structure type"
      stx id)
  ; TODO: Also verify that `struct:foo`, `make-foo`, `foo?`, and
  ; `getters` all belong to the same structure type and that the
  ; `getters` are exhaustive and arranged in the correct order.
  ; Otherwise, this could create a dex that doesn't behave
  ; consistently with other dexes for the same structure type.
  #/list struct:foo make-foo foo? #/reverse getters))



; ===== Orderings ====================================================

(define-simple-macro (maybe-ordering-or first:expr second:expr)
  (w- result first
  #/expect result (just #/ordering-eq) result
    second))

(define (maybe-compare-aligned-lists as bs maybe-compare-elems)
  (expect (list as bs) (list (cons a as) (cons b bs))
    (just #/ordering-eq)
  #/maybe-ordering-or (maybe-compare-elems a b)
  #/maybe-compare-aligned-lists as bs maybe-compare-elems))



; ===== Names, dexes, and dexed values ===============================

(define/contract (dex? x)
  (-> any/c boolean?)
  (internal:dex? x))

(define-imitation-simple-struct
  (dexed? dexed-get-unwrapped-dex dexed-get-name dexed-get-value)
  dexed 'dexed (current-inspector) (auto-write))

(define/contract (autoname-dex x)
  (-> dex? any)
  (dissect x (internal:dex internals)
  #/cons 'name:dex #/internal:dex-internals-autoname internals))

(define (in-dex? dex x)
  (dissect dex (internal:dex internals)
  #/internal:dex-internals-in? internals x))

(define (name-of dex x)
  (dissect dex (internal:dex internals)
  #/internal:dex-internals-name-of internals x))

(define (dexed-of dex x)
  (dissect dex (internal:dex internals)
  #/internal:dex-internals-dexed-of internals x))

(define (compare-by-dex dex a b)
  (dissect dex (internal:dex internals)
  #/internal:dex-internals-compare internals a b))

(define/contract (eq-by-dex? dex a b)
  (-> dex? any/c any/c boolean?)
  (expect (compare-by-dex dex a b) (just comparison)
    (raise-arguments-error 'eq-by-dex?
      "expected a and b to be members of the domain of dex"
      "dex" dex
      "a" a
      "b" b)
  #/ordering-eq? comparison))

(define/contract (dex-internals-simple-dexed-of this x)
  (-> internal:dex-internals? any/c #/maybe/c dexed?)
  (w- this (internal:dex this)
  #/maybe-map (name-of this x) #/fn name
    (dexed (dex-particular this name x) name x)))


(define (dexed/c c)
  (w- c (coerce-contract 'dexed/c c)
  #/ (make-appropriate-non-chaperone-contract c)
    
    #:name `(dexed/c ,(contract-name c))
    
    #:first-order
    (fn v
      (contract-first-order-passes? (match/c dexed any/c any/c c) v))
    
    #:late-neg-projection
    (fn blame
      (w- c-late-neg-projection
        ( (get/build-late-neg-projection c)
          (blame-add-context blame "the value of"))
      #/fn v missing-party
        (expect v (dexed dex name value)
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a dexed value" given: "~e")
            v)
        #/w- new-value (c-late-neg-projection value missing-party)
        #/expect (compare-by-dex dex value new-value)
          (just #/ordering-eq)
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a dexed value which projected to something ordering-eq to the original" given: "~e")
            v)
        #/dexed dex name value)))))

(define (dexed-first-order/c c)
  (w- c (coerce-contract 'dexed-first-order/c c)
  #/rename-contract
    (dexed/c #/contract-first-order c)
    `(dexed-first-order/c ,(contract-name c))))



(struct-easy (dex-internals-particular dex-val name-rep val)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-particular)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-particular dex-val name-rep val)
      #/list 'tag:dex-particular name-rep))
    
    (define (dex-internals-autodex this other)
      (dissect this
        (dex-internals-particular a-dex-val a-name-rep a-val)
      #/dissect other
        (dex-internals-particular b-dex-val b-name-rep b-val)
      #/maybe-ordering-or
        (compare-by-dex (dex-dex) a-dex-val b-dex-val)
        (compare-by-dex a-dex-val a-val b-val)))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-particular dex-val name-rep val)
      #/mat (compare-by-dex dex-val val x) (just #/ordering-eq)
        #t
        #f))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-particular dex-val name-rep val)
      #/name-of dex-val x))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-particular dex-val name-rep val)
      #/dexed-of dex-val x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-particular dex-val name-rep val)
      #/compare-by-dex dex-val a b))
  ])

(define/contract (dex-particular dex-val name val)
  (-> dex? name? any/c dex?)
  (dissect name (internal:name name-rep)
  #/internal:dex #/dex-internals-particular dex-val name-rep val))


(struct-easy (dex-internals-for-dexed dex)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-for-dexed)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-for-dexed dex)
      #/list 'tag:dex-for-dexed #/autoname-dex dex))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-for-dexed a-dex)
      #/dissect other (dex-internals-for-dexed b-dex)
      #/compare-by-dex (dex-dex) a-dex b-dex))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-for-dexed dex)
      #/in-dex? dex x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-for-dexed dex)
      #/name-of dex x))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-for-dexed dex)
      #/dexed-of dex x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-for-dexed dex)
      #/compare-by-dex dex a b))
  ])

(define/contract (dex-for-dexed dex)
  (-> dex? dex?)
  (internal:dex #/dex-internals-for-dexed dex))

(define (dexed-get-dex d)
  (dissect d (dexed unwrapped-dex name value)
  #/dex-for-dexed unwrapped-dex))


(struct-easy (dex-internals-name)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-name)
    
    (define (dex-internals-autoname this)
      'tag:dex-name)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (name? x))
    
    (define (dex-internals-name-of this x)
      (expect x (internal:name rep) (nothing)
      #/just #/internal:name #/list 'name:name rep))
    
    (define (dex-internals-dexed-of this x)
      (dex-internals-simple-dexed-of this x))
    
    (define (dex-internals-compare this a b)
      (if (and (name? a) (name? b))
        (just #/names-autodex a b)
        (nothing)))
  ])

(define/contract (dex-name)
  (-> dex?)
  (internal:dex #/dex-internals-name))


(struct-easy (dex-internals-dex)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-dex)
    
    (define (dex-internals-autoname this)
      'tag:dex-dex)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (dex? x))
    
    (define (dex-internals-name-of this x)
      (if (dex? x)
        (just #/internal:name #/autoname-dex x)
        (nothing)))
    
    (define (dex-internals-dexed-of this x)
      (dex-internals-simple-dexed-of this x))
    
    (define (dex-internals-compare this a b)
      (expect a (internal:dex a) (nothing)
      #/expect b (internal:dex b) (nothing)
      #/w- tag internal:dex-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:dex-internals-autodex a b))
  ])

(define/contract (dex-dex)
  (-> dex?)
  (internal:dex #/dex-internals-dex))


(struct-easy (dex-internals-dexed)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-dexed)
    
    (define (dex-internals-autoname this)
      'tag:dex-dexed)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (dexed? x))
    
    (define (dex-internals-name-of this x)
      (expect (dexed? x) #t (nothing)
      #/dissect (dexed-get-name x) (internal:name rep)
      #/just #/internal:name #/list 'name:dexed rep))
    
    (define (dex-internals-dexed-of this x)
      (dex-internals-simple-dexed-of this x))
    
    (define (dex-internals-compare this a b)
      (expect (dexed? a) #t (nothing)
      #/expect (dexed? b) #t (nothing)
      #/compare-by-dex (dex-name)
        (dexed-get-name a)
        (dexed-get-name b)))
  ])

(define/contract (dex-dexed)
  (-> dex?)
  (internal:dex #/dex-internals-dexed))


(struct-easy (dex-internals-give-up)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-give-up)
    
    (define (dex-internals-autoname this)
      'tag:dex-give-up)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      #f)
    
    (define (dex-internals-name-of this x)
      (nothing))
    
    (define (dex-internals-dexed-of this x)
      (nothing))
    
    (define (dex-internals-compare this a b)
      (nothing))
  ])

(define/contract (dex-give-up)
  (-> dex?)
  (internal:dex #/dex-internals-give-up))


(struct-easy
  (dex-internals-default
    dex-for-trying-first
    dex-for-trying-second)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-default)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-default first second)
      #/list 'tag:dex-default
        (autoname-dex first)
        (autoname-dex second)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-default a1 a2)
      #/dissect other (dex-internals-default b1 b2)
      #/maybe-ordering-or
        (compare-by-dex (dex-dex) a1 b1)
        (compare-by-dex (dex-dex) a2 b2)))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-default first second)
      #/or (in-dex? first x) (in-dex? second x)))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-default first second)
      #/mat (name-of first x) (just result) (just result)
      #/name-of second x))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-default first second)
      #/mat (dexed-of first x) (just result) (just result)
      #/dexed-of second x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-default first second)
      #/w- first-result (compare-by-dex first a b)
      #/mat first-result (just _) first-result
      #/if (in-dex? first a)
        (if (in-dex? second b)
          (just #/ordering-lt)
          (nothing))
      #/if (in-dex? first b)
        (if (in-dex? second a)
          (just #/ordering-gt)
          (nothing))
      #/compare-by-dex second a b))
  ])

(define/contract
  (dex-default dex-for-trying-first dex-for-trying-second)
  (-> dex? dex? dex?)
  (internal:dex
  #/dex-internals-default dex-for-trying-first dex-for-trying-second))


(struct-easy (dex-internals-opaque name dex)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-opaque)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-opaque (internal:name name) dex)
      #/list 'tag:dex-opaque name #/autoname-dex dex))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-opaque a-name a-dex)
      #/dissect other (dex-internals-default b-name b-dex)
      #/maybe-ordering-or
        (compare-by-dex (dex-name) a-name b-name)
        (compare-by-dex (dex-dex) a-dex b-dex)))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-opaque name dex)
      #/in-dex? dex x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-opaque name dex)
      #/name-of dex x))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-opaque name dex)
      #/dexed-of dex x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-opaque name dex)
      #/compare-by-dex dex a b))
  ])

(define/contract (dex-opaque name dex)
  (-> name? dex? dex?)
  (internal:dex #/dex-internals-opaque name dex))


(define-syntax-rule
  (define-cmp-by-own-method
    internal:cmp
    cmp?
    cmp-by-own-method::raise-different-methods-error
    cmp-by-own-method::get-method
    cmp-by-own-method-delegate/c
    cmp-internals-by-own-method-delegate-raise-different-methods-error
    cmp-internals-by-own-method-delegate-get-method
    cmp-internals-by-own-method
    cmp-by-own-method-thorough
    cmp-by-own-method-unthorough
    cmp-by-own-method
    expected-raise-different-methods
    expected-the-result-of-delegate-get-method-to-be-a-cmp
    expected-the-result-of-dexed-get-method-to-be-a-cmp)
  (begin
    
    (struct-easy
      (cmp-by-own-method::raise-different-methods-error
        a b a-method b-method))
    (struct-easy
      (cmp-by-own-method::get-method source))
    
    (define/contract cmp-by-own-method-delegate/c
      contract?
      (case->
        (->
          (match/c cmp-by-own-method::raise-different-methods-error
            any/c any/c cmp? cmp?)
          none/c)
        (->
          (match/c cmp-by-own-method::get-method any/c)
          (maybe/c cmp?))))
    
    (define/contract
      (cmp-internals-by-own-method-delegate-raise-different-methods-error
        dexed-delegate a b a-method b-method)
      (->
        (dexed-first-order/c cmp-by-own-method-delegate/c)
        any/c
        any/c
        cmp?
        cmp?
        none/c)
      (w- delegate (dexed-get-value dexed-delegate)
      #/w- result
        (delegate #/cmp-by-own-method::raise-different-methods-error
          a b a-method b-method)
      #/raise-arguments-error 'cmp-by-own-method-thorough
        expected-raise-different-methods
        "dexed-delegate" dexed-delegate
        "a" a
        "b" b
        "a-method" a-method
        "b-method" b-method
        "result" result))
    
    (define/contract
      (cmp-internals-by-own-method-delegate-get-method
        dexed-delegate source)
      (-> (dexed-first-order/c cmp-by-own-method-delegate/c) any/c
        (maybe/c cmp?))
      (w- delegate (dexed-get-value dexed-delegate)
      #/w- result (delegate #/cmp-by-own-method::get-method source)
      #/expect (contract-first-order-passes? (maybe/c cmp?) result) #t
        (raise-arguments-error 'cmp-by-own-method-thorough
          expected-the-result-of-delegate-get-method-to-be-a-cmp
          "dexed-delegate" dexed-delegate
          "source" source
          "result" result)
        result))
    
    ; NOTE: If we weren't using this macro, we'd write the
    ; (struct-easy (cmp-internals-by-own-method ...) ...) declaration
    ; here. However, this definition substantially differs between the
    ; dex and the cline definitions, and the mutual recursion works
    ; out fine this way in Racket, so we leave it out of this and
    ; define `dex-internals-by-own-method` and
    ; `cline-internals-by-own-method` separately from this macro call.
    
    (define/contract (cmp-by-own-method-thorough dexed-delegate)
      (-> (dexed-first-order/c cmp-by-own-method-delegate/c) cmp?)
      (internal:cmp #/cmp-internals-by-own-method dexed-delegate))
    
    (struct-easy (cmp-by-own-method-unthorough dexed-get-method)
      #:other
      
      #:property prop:procedure
      (fn this command
        (dissect this (cmp-by-own-method-unthorough dexed-get-method)
        #/mat command
          (cmp-by-own-method::raise-different-methods-error
            a b a-method b-method)
          (raise-arguments-error 'cmp-by-own-method
            "obtained two different methods from the two values being compared"
            "dexed-get-method" dexed-get-method
            "a" a
            "b" b
            "a-method" a-method
            "b-method" b-method)
        #/dissect command (cmp-by-own-method::get-method source)
        #/w- get-method (dexed-get-value dexed-get-method)
        #/w- result (get-method source)
        #/expect (contract-first-order-passes? (maybe/c cmp?) result)
          #t
          (raise-arguments-error 'cmp-by-own-method
            expected-the-result-of-dexed-get-method-to-be-a-cmp
            "dexed-get-method" dexed-get-method
            "result" result)
          result)))
    
    (define/contract (cmp-by-own-method dexed-get-method)
      (-> (dexed-first-order/c #/-> any/c #/maybe/c cmp?) cmp?)
      (cmp-by-own-method-thorough #/just-value #/dexed-of
        (dex-struct cmp-by-own-method-unthorough #/dex-dexed)
        (cmp-by-own-method-unthorough dexed-get-method)))
  ))

(define-cmp-by-own-method
  internal:dex
  dex?
  dex-by-own-method::raise-different-methods-error
  dex-by-own-method::get-method
  dex-by-own-method-delegate/c
  dex-internals-by-own-method-delegate-raise-different-methods-error
  dex-internals-by-own-method-delegate-get-method
  dex-internals-by-own-method
  dex-by-own-method-thorough
  dex-by-own-method-unthorough
  dex-by-own-method
  "expected dexed-delegate not to return for dex-by-own-method::raise-different-methods-error"
  "expected the result of dexed-delegate for dex-by-own-method::get-method to be a maybe of a dex"
  "expected the result of dexed-get-method to be a maybe of a dex")

(struct-easy (dex-internals-by-own-method dexed-delegate)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-by-own-method)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-by-own-method dexed-delegate)
      #/dissect (dexed-get-name dexed-delegate) (internal:name rep)
      #/list 'tag:dex-by-own-method rep))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-by-own-method a)
      #/dissect other (dex-internals-by-own-method b)
      #/compare-by-dex (dex-dexed) a b))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-by-own-method dexed-delegate)
      #/expect
        (dex-internals-by-own-method-delegate-get-method
          dexed-delegate x)
        (just method)
        #f
      #/in-dex? method x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-by-own-method dexed-delegate)
      #/maybe-bind
        (dex-internals-by-own-method-delegate-get-method
          dexed-delegate x)
      #/fn method
      #/name-of method x))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-by-own-method dexed-delegate)
      #/maybe-bind
        (dex-internals-by-own-method-delegate-get-method
          dexed-delegate x)
      #/fn method
      #/dexed-of method x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-by-own-method dexed-delegate)
      #/maybe-bind
        (dex-internals-by-own-method-delegate-get-method
          dexed-delegate a)
      #/fn a-method
      #/maybe-bind
        (dex-internals-by-own-method-delegate-get-method
          dexed-delegate b)
      #/fn b-method
      #/expect (eq-by-dex? (dex-dex) a-method b-method) #t
        (dex-internals-by-own-method-delegate-raise-different-methods-error
          dexed-delegate a b a-method b-method)
      #/compare-by-dex a-method a b))
  ])


(define/contract (dex-internals-fix-delegate-unwrap dexed-unwrap this)
  (-> (dexed-first-order/c #/-> dex? dex?) dex? dex?)
  (w- unwrap (dexed-get-value dexed-unwrap)
  #/w- result (unwrap this)
  #/expect (dex? result) #t
    (raise-arguments-error 'dex-fix
      "expected the result of dexed-unwrap to be a dex"
      "dexed-unwrap" dexed-unwrap
      "this" this
      "result" result)
    result))

(struct-easy (dex-internals-fix dexed-unwrap)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-fix)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-fix dexed-unwrap)
      #/dissect (dexed-get-name dexed-unwrap) (internal:name rep)
      #/list 'tag:dex-fix rep))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-fix a)
      #/dissect other (dex-internals-fix b)
      #/compare-by-dex (dex-dexed) a b))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-fix dexed-unwrap)
      #/in-dex?
        (dex-internals-fix-delegate-unwrap dexed-unwrap
          (internal:dex this))
        x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-fix dexed-unwrap)
      #/name-of
        (dex-internals-fix-delegate-unwrap dexed-unwrap
          (internal:dex this))
        x))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-fix dexed-unwrap)
      #/dexed-of
        (dex-internals-fix-delegate-unwrap dexed-unwrap
          (internal:dex this))
        x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-fix dexed-unwrap)
      #/compare-by-dex
        (dex-internals-fix-delegate-unwrap dexed-unwrap
          (internal:dex this))
        a
        b))
  ])

(define/contract (dex-fix dexed-unwrap)
  (-> (dexed-first-order/c #/-> dex? dex?) dex?)
  (internal:dex #/dex-internals-fix dexed-unwrap))


(struct-easy (dex-internals-struct descriptor counts? fields)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-struct-by-field-position)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/list* 'tag:dex-struct-by-field-position descriptor
      #/list-map fields #/dissectfn (list getter position dex)
        (list position #/autoname-dex dex)))
    
    (define (dex-internals-autodex this other)
      (dissect this
        (dex-internals-struct a-descriptor a-counts? a-fields)
      #/dissect other
        (dex-internals-struct b-descriptor b-counts? b-fields)
      #/maybe-ordering-or
        (just #/object-identities-autodex a-descriptor b-descriptor)
      #/maybe-ordering-or
        (maybe-compare-aligned-lists a-fields b-fields
        #/fn a-field b-field
          (dissect a-field (list a-getter a-position a-dex)
          #/dissect b-field (list b-getter b-position b-dex)
          #/just #/lt-autodex a-position b-position <))
        (maybe-compare-aligned-lists a-fields b-fields
        #/fn a-field b-field
          (dissect a-field (list a-getter a-position a-dex)
          #/dissect b-field (list b-getter b-position b-dex)
          #/compare-by-dex (dex-dex) a-dex b-dex))))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/and (counts? x)
      #/w-loop next fields fields
        (expect fields (cons field fields) #t
        #/dissect field (list getter position dex)
        
        ; We do a tail call if we can.
        #/mat fields (list) (in-dex? dex #/getter x)
        
        #/and (in-dex? dex #/getter x)
        #/next fields)))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/expect (counts? x) #t (nothing)
      #/w-loop next fields fields unsorted-reps (list)
        (expect fields (cons field fields)
          (just #/internal:name #/list* 'name:struct descriptor
            (list-map
              (sort unsorted-reps #/fn a b
                (dissect a (cons a-position a-rep)
                #/dissect b (cons b-position b-rep)
                #/< a-position b-position))
            #/dissectfn (list position rep)
              rep))
        #/dissect field (list getter position dex)
        #/expect (name-of dex #/getter x) (just name) (nothing)
        #/dissect name (internal:name rep)
        #/next fields (cons (list position rep) unsorted-reps))))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/expect (counts? x) #t (nothing)
      #/w-loop next fields fields unsorted-dexeds (list)
        (expect fields (cons field fields)
          (w- dexeds
            (sort unsorted-dexeds #/fn a b
              (dissect a (list a-getter a-position a-dexed)
              #/dissect b (list b-getter b-position b-dexed)
              #/< a-position b-position))
          #/just #/dexed
            (internal:dex #/dex-internals-struct descriptor counts?
              (list-map dexeds
              #/dissectfn (list getter position (dexed dex name val))
                (list getter position dex)))
            (internal:name #/list* 'name:struct descriptor
              (list-map dexeds
              #/dissectfn
                (list getter position
                  (dexed dex (internal:name rep) val))
                rep))
            x)
        #/dissect field (list getter position dex)
        #/expect (dexed-of dex #/getter x) (just dexed) (nothing)
        #/next fields
          (cons (list getter position dexed) unsorted-dexeds))))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/expect (counts? a) #t (nothing)
      #/expect (counts? b) #t (nothing)
      #/w-loop next fields fields
        (expect fields (cons field fields) (just #/ordering-eq)
        #/dissect field (list getter position dex)
        
        ; We do a tail call if we can.
        #/mat fields (list) (compare-by-dex dex (getter a) (getter b))
        
        #/w- result (compare-by-dex dex (getter a) (getter b))
        #/expect result (just #/ordering-eq)
          (mat result (nothing) (nothing)
          ; We have a potential result to use, but first we check that
          ; the rest of the field values belong to their respective
          ; dexes' domains. If they don't, this structure instance is
          ; not part part of this dex's domain, so the result is
          ; `(nothing)`.
          #/w-loop next fields fields
            (expect fields (cons field fields) result
            #/dissect field (list getter position dex)
            #/expect
              (and (in-dex? dex #/getter a) (in-dex? dex #/getter b))
              #t
              (nothing)
            #/next fields))
        #/next fields)))
  ])

(define-syntax (dex-struct-by-field-position stx)
  (syntax-parse stx #/
    (_ struct-tag:id [field-position:nat field-dex:expr] ...)
  #/dissect (get-immutable-root-ancestor-struct-info stx #'struct-tag)
    (list struct:foo make-foo foo? getters)
  #/w- fields (desyntax-list #'#/[field-position field-dex] ...)
  #/w- n (length getters)
  #/expect (= n (length fields)) #t
    (raise-syntax-error #f
      (format "expected ~s dexes, got ~s"
        n
        (length fields))
      stx)
  #/w- seen (make-hasheq)
  #/syntax-protect
    #`(internal:dex #/dex-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-map fields #/fn field
             (dissect (desyntax-list field)
               (list position-stx dex)
             #/w- position (syntax-e position-stx)
             #/expect (< position n) #t
               (raise-syntax-error #f
                 (format
                   "expected a field position less than ~s, got ~s"
                   n
                   position)
                 stx position-stx)
             #/expect (hash-has-key? seen position) #f
               (raise-syntax-error #f
                 "duplicate field position"
                 stx position-stx)
             #/begin (hash-set! seen position #t)
               #`(list
                   #,(list-ref getters position)
                   #,position-stx
                   #,dex))))))

(define-syntax (dex-struct stx)
  (syntax-parse stx #/ (_ struct-tag:id field-dex:expr ...)
  #/dissect (get-immutable-root-ancestor-struct-info stx #'struct-tag)
    (list struct:foo make-foo foo? getters)
  #/w- fields (desyntax-list #'#/field-dex ...)
  #/w- n (length getters)
  #/expect (= n (length fields)) #t
    (raise-syntax-error #f
      (format "expected ~s dexes, got ~s"
        n
        (length fields))
      stx)
  #/syntax-protect
    #`(internal:dex #/dex-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-kv-map (map list fields getters) #/fn position field
             (dissect field (list dex getter)
               #`(list #,getter #,position #,dex))))))


; TODO: See if we should have `dex-match` functionality like this. If
; so, we'll need to finish this off with an implementation of
; `dex-internals-match` and do similar work to make
; `{cline,fuse,merge}-match`. However, there's still a design question
; in the way: How will these dexes be compared to each other? With
; `dex-struct`, they can be compared by the structure type
; descriptor's object identity. but a match expander doesn't give us
; an identity like that, does it?
;
#|
(define-syntax (dex-match-by-argument-position stx)
  (syntax-parse stx #/ (_ op:id [arg-position:nat arg-dex:expr] ...)
    #:with (local ...) (generate-temporarites #'(arg-dex ...))
  #/w- args (desyntax-list #'#/[arg-position arg-dex] ...)
  #/w- n (length args)
  #/w- seen (make-hasheq)
  #/syntax-protect
    #`(internal:dex #/dex-internals-match
        (fn local ... #/op local ...)
        (fn v
          (expect v (op local ...) #f
          #/list local ...))
      #/list
        #,@(list-map args #/fn arg
             (dissect (desyntax-list arg) (list position-stx dex)
             #/w- position (syntax-e position-stx)
             #/expect (< position n) #t
               (raise-syntax-error #f
                 (format
                   "expected an argument position less than ~s, got ~s"
                   n
                   position)
                 stx position-stx)
             #/expect (hash-has-key? seen position) #f
               (raise-syntax-error #f
                 "duplicate argument position"
                 stx position-stx)
             #/begin (hash-set! seen position #t)
               #`(list #,position-stx #,dex))))))

(define-syntax (dex-match stx)
  (syntax-parse stx #/ (_ op:id arg-dex:expr ...)
    #:with (local ...) (generate-temporarites #'(arg-dex ...))
    #:with (position ...) (range (length #'(arg-dex ...)))
  #/syntax-protect
    #'(internal:dex #/dex-internals-match
        (fn local ... #/op local ...)
        (fn v
          (expect v (op local ...) #f
          #/list local ...))
      #/list (list position arg-dex) ...))))
|#



; ===== Clines =======================================================

(define/contract (cline? x)
  (-> any/c boolean?)
  (internal:cline? x))

(define/contract (autoname-cline x)
  (-> cline? any)
  (dissect x (internal:cline internals)
  #/cons 'name:cline #/internal:cline-internals-autoname internals))

(define/contract (get-dex-from-cline cline)
  (-> cline? dex?)
  (dissect cline (internal:cline internals)
  #/internal:cline-internals-dex internals))

(define/contract (in-cline? cline x)
  (-> cline? any/c boolean?)
  (dissect cline (internal:cline internals)
  #/internal:cline-internals-in? internals x))

(define/contract (compare-by-cline cline a b)
  (-> cline? any/c any/c #/maybe/c cline-result?)
  (dissect cline (internal:cline internals)
  #/internal:cline-internals-compare internals a b))


(struct-easy (dex-internals-cline)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-cline)
    
    (define (dex-internals-autoname this)
      'tag:dex-cline)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (cline? x))
    
    (define (dex-internals-name-of this x)
      (if (cline? x)
        (just #/internal:name #/autoname-cline x)
        (nothing)))
    
    (define (dex-internals-dexed-of this x)
      (dex-internals-simple-dexed-of this x))
    
    (define (dex-internals-compare this a b)
      (expect a (internal:cline a) (nothing)
      #/expect b (internal:cline b) (nothing)
      #/w- tag internal:cline-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:cline-internals-autodex a b))
  ])

(define/contract (dex-cline)
  (-> dex?)
  (internal:dex #/dex-internals-cline))


(struct-easy (cline-internals-by-dex dex)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-by-dex)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-by-dex dex)
      #/list 'tag:cline-by-dex #/autoname-dex dex))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-dex a)
      #/dissect other (cline-internals-by-dex b)
      #/compare-by-dex (dex-dex) a b))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-by-dex dex)
        dex))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-by-dex dex)
      #/in-dex? dex x))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-by-dex dex)
      #/compare-by-dex dex a b))
  ])

(define/contract (cline-by-dex dex)
  (-> dex? cline?)
  (internal:cline #/cline-internals-by-dex dex))


(struct-easy (cline-internals-give-up)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-give-up)
    
    (define (cline-internals-autoname this)
      'tag:cline-give-up)
    
    (define (cline-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (cline-internals-dex this)
      (dex-give-up))
    
    (define (cline-internals-in? this x)
      #f)
    
    (define (cline-internals-compare this a b)
      (nothing))
  ])

(define/contract (cline-give-up)
  (-> cline?)
  (internal:cline #/cline-internals-give-up))


(struct-easy
  (cline-internals-default
    cline-for-trying-first
    cline-for-trying-second)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-default)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-default first second)
      #/list 'tag:cline-default
        (autoname-cline first)
        (autoname-cline second)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-default a1 a2)
      #/dissect other (cline-internals-default b1 b2)
      #/maybe-ordering-or
        (compare-by-dex (dex-cline) a1 b1)
        (compare-by-dex (dex-cline) a2 b2)))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-default first second)
      #/dex-default
        (get-dex-from-cline first)
        (get-dex-from-cline second)))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-default first second)
      #/or (in-cline? first x) (in-cline? second x)))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-default first second)
      #/w- first-result (compare-by-cline first a b)
      #/mat first-result (just _) first-result
      #/if (in-cline? first a)
        (if (in-cline? second b)
          (just #/ordering-lt)
          (nothing))
      #/if (in-cline? first b)
        (if (in-cline? second a)
          (just #/ordering-gt)
          (nothing))
      #/compare-by-cline second a b))
  ])

(define/contract
  (cline-default cline-for-trying-first cline-for-trying-second)
  (-> cline? cline? cline?)
  (internal:cline
  #/cline-internals-default
    cline-for-trying-first
    cline-for-trying-second))


(struct-easy (cline-internals-opaque name cline)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-opaque)
    
    (define (cline-internals-autoname this)
      (dissect this
        (cline-internals-opaque (internal:name name) cline)
      #/list 'tag:cline-opaque name #/autoname-cline cline))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-opaque a-name a-cline)
      #/dissect other (cline-internals-opaque b-name b-cline)
      #/maybe-ordering-or
        (compare-by-dex (dex-name) a-name b-name)
        (compare-by-dex (dex-cline) a-cline b-cline)))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-opaque name cline)
      #/dex-opaque name #/get-dex-from-cline cline))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-opaque name cline)
      #/in-cline? cline x))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-opaque name cline)
      #/compare-by-cline cline a b))
  ])

(define/contract (cline-opaque name cline)
  (-> name? cline? cline?)
  (internal:cline #/cline-internals-opaque name cline))


(define-cmp-by-own-method
  internal:cline
  cline?
  cline-by-own-method::raise-different-methods-error
  cline-by-own-method::get-method
  cline-by-own-method-delegate/c
  cline-internals-by-own-method-delegate-raise-different-methods-error
  cline-internals-by-own-method-delegate-get-method
  cline-internals-by-own-method
  cline-by-own-method-thorough
  cline-by-own-method-unthorough
  cline-by-own-method
  "expected dexed-delegate not to return for cline-by-own-method::raise-different-methods-error"
  "expected the result of dexed-delegate for cline-by-own-method::get-method to be a maybe of a cline"
  "expected the result of dexed-get-method to be a maybe of a cline")

(struct-easy (convert-dex-from-cline-by-own-method dexed-delegate)
  #:other
  
  #:property prop:procedure
  (fn this x
    (dissect this
      (convert-dex-from-cline-by-own-method dexed-delegate)
    #/maybe-bind
      (cline-internals-by-own-method-delegate-get-method
        dexed-delegate x)
    #/fn method
    #/just #/get-dex-from-cline method)))

(struct-easy (cline-internals-by-own-method dexed-delegate)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-by-own-method)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-by-own-method dexed-delegate)
      #/dissect (dexed-get-name dexed-delegate) (internal:name rep)
      #/list 'tag:cline-by-own-method rep))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-own-method a)
      #/dissect other (cline-internals-by-own-method b)
      #/compare-by-dex (dex-dexed) a b))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-by-own-method dexed-delegate)
      #/dex-by-own-method #/just-value #/dexed-of
        (dex-struct convert-dex-from-cline-by-own-method #/dex-dexed)
        (convert-dex-from-cline-by-own-method dexed-delegate)))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-by-own-method dexed-delegate)
      #/expect
        (cline-internals-by-own-method-delegate-get-method
          dexed-delegate x)
        (just method)
        #f
      #/in-cline? method x))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-by-own-method dexed-delegate)
      #/maybe-bind
        (cline-internals-by-own-method-delegate-get-method
          dexed-delegate a)
      #/fn a-method
      #/maybe-bind
        (cline-internals-by-own-method-delegate-get-method
          dexed-delegate b)
      #/fn b-method
      #/expect (eq-by-dex? (dex-cline) a-method b-method) #t
        (cline-internals-by-own-method-delegate-raise-different-methods-error
          dexed-delegate a b a-method b-method)
      #/compare-by-cline a-method a b))
  ])


(define/contract
  (cline-internals-fix-delegate-unwrap dexed-unwrap this)
  (-> (dexed-first-order/c #/-> cline? cline?) cline? cline?)
  (w- unwrap (dexed-get-value dexed-unwrap)
  #/w- result (unwrap this)
  #/expect (cline? result) #t
    (raise-arguments-error 'cline-fix
      "expected the result of dexed-unwrap to be a cline"
      "dexed-unwrap" dexed-unwrap
      "this" this
      "result" result)
    result))

(struct-easy (convert-dex-from-cline-fix dexed-unwrap)
  #:other
  
  #:property prop:procedure
  (fn this dex
    (dissect this (convert-dex-from-cline-fix dexed-unwrap)
    #/get-dex-from-cline
      (cline-internals-fix-delegate-unwrap dexed-unwrap
        (cline-by-dex dex)))))

(struct-easy (cline-internals-fix dexed-unwrap)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-fix)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-fix dexed-unwrap)
      #/dissect (dexed-get-name dexed-unwrap) (internal:name rep)
      #/list 'tag:cline-fix rep))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-fix a)
      #/dissect other (cline-internals-fix b)
      #/compare-by-dex (dex-dexed) a b))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-fix dexed-unwrap)
      #/dex-fix #/just-value #/dexed-of
        (dex-struct convert-dex-from-cline-fix #/dex-dexed)
        (convert-dex-from-cline-fix dexed-unwrap)))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-fix dexed-unwrap)
      #/in-cline?
        (cline-internals-fix-delegate-unwrap dexed-unwrap
          (internal:cline this))
        x))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-fix dexed-unwrap)
      #/compare-by-cline
        (cline-internals-fix-delegate-unwrap dexed-unwrap
          (internal:cline this))
        a
        b))
  ])

(define/contract (cline-fix dexed-unwrap)
  (-> (dexed-first-order/c #/-> cline? cline?) cline?)
  (internal:cline #/cline-internals-fix dexed-unwrap))


(struct-easy (cline-internals-struct descriptor counts? fields)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-struct-by-field-position)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/list* 'tag:cline-struct-by-field-position descriptor
      #/list-map fields #/dissectfn (list getter position cline)
        (list position #/autoname-cline cline)))
    
    (define (cline-internals-autodex this other)
      (dissect this
        (cline-internals-struct a-descriptor a-counts? a-fields)
      #/dissect other
        (cline-internals-struct b-descriptor b-counts? b-fields)
      #/maybe-ordering-or
        (just #/object-identities-autodex a-descriptor b-descriptor)
      #/maybe-ordering-or
        (maybe-compare-aligned-lists a-fields b-fields
        #/fn a-field b-field
          (dissect a-field (list a-getter a-position a-cline)
          #/dissect b-field (list b-getter b-position b-cline)
          #/just #/lt-autodex a-position b-position <))
        (maybe-compare-aligned-lists a-fields b-fields
        #/fn a-field b-field
          (dissect a-field (list a-getter a-position a-cline)
          #/dissect b-field (list b-getter b-position b-cline)
          #/compare-by-dex (dex-cline) a-cline b-cline))))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/and (counts? x)
      #/w-loop next fields fields
        (expect fields (cons field fields) #t
        #/dissect field (list getter position cline)
        
        ; We do a tail call if we can.
        #/mat fields (list) (in-cline? cline #/getter x)
        
        #/and (in-cline? cline #/getter x)
        #/next fields)))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/dex-internals-struct descriptor counts?
      #/list-map fields #/dissectfn (list getter position cline)
        (list getter position #/get-dex-from-cline cline)))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/expect (counts? a) #t (nothing)
      #/expect (counts? b) #t (nothing)
      #/w-loop next fields fields
        (expect fields (cons field fields) (just #/ordering-eq)
        #/dissect field (list getter position cline)
        
        ; We do a tail call if we can.
        #/mat fields (list)
          (compare-by-cline cline (getter a) (getter b))
        
        #/w- result (compare-by-cline cline (getter a) (getter b))
        #/expect result (just #/ordering-eq)
          (mat result (nothing) (nothing)
          ; We have a potential result to use, but first we check that
          ; the rest of the field values belong to their respective
          ; clines' domains. If they don't, this structure instance is
          ; not part part of this cline's domain, so the result is
          ; `(nothing)`.
          #/w-loop next fields fields
            (expect fields (cons field fields) result
            #/dissect field (list getter position cline)
            #/expect
              (and
                (in-cline? cline #/getter a)
                (in-cline? cline #/getter b))
              #t
              (nothing)
            #/next fields))
        #/next fields)))
  ])

(define-syntax (cline-struct-by-field-position stx)
  (syntax-parse stx #/
    (_ struct-tag:id [field-position:nat field-cline:expr] ...)
  #/dissect (get-immutable-root-ancestor-struct-info stx #'struct-tag)
    (list struct:foo make-foo foo? getters)
  #/w- fields (desyntax-list #'#/[field-position field-cline] ...)
  #/w- n (length getters)
  #/expect (= n (length fields)) #t
    (raise-syntax-error #f
      (format "expected ~s clines, got ~s"
        n
        (length fields))
      stx)
  #/w- seen (make-hasheq)
  #/syntax-protect
    #`(internal:cline #/cline-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-map fields #/fn field
             (dissect (desyntax-list field)
               (list position-stx cline)
             #/w- position (syntax-e position-stx)
             #/expect (< position n) #t
               (raise-syntax-error #f
                 (format
                   "expected a field position less than ~s, got ~s"
                   n
                   position)
                 stx position-stx)
             #/expect (hash-has-key? seen position) #f
               (raise-syntax-error #f
                 "duplicate field position"
                 stx position-stx)
             #/begin (hash-set! seen position #t)
               #`(list
                   #,(list-ref getters position)
                   #,position-stx
                   #,cline))))))

(define-syntax (cline-struct stx)
  (syntax-parse stx #/ (_ struct-tag:id field-cline:expr ...)
  #/dissect (get-immutable-root-ancestor-struct-info stx #'struct-tag)
    (list struct:foo make-foo foo? getters)
  #/w- fields (desyntax-list #'#/field-cline ...)
  #/w- n (length getters)
  #/expect (= n (length fields)) #t
    (raise-syntax-error #f
      (format "expected ~s clines, got ~s"
        n
        (length fields))
      stx)
  #/syntax-protect
    #`(internal:cline #/cline-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-kv-map (map list fields getters) #/fn position field
             (dissect field (list cline getter)
               #`(list #,getter #,position #,cline))))))


(struct-easy (cline-internals-flip cline)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-flip)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-flip cline)
      #/list 'tag:cline-flip #/autoname-cline cline))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-flip a)
      #/dissect other (cline-internals-flip b)
      #/compare-by-dex (dex-cline) a b))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-flip cline)
      #/get-dex-from-cline cline))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-flip cline)
      #/in-cline? cline x))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-flip cline)
      #/w- unflipped-result (compare-by-cline cline a b)
      #/mat unflipped-result (ordering-lt) (ordering-gt)
      #/mat unflipped-result (ordering-gt) (ordering-lt)
        unflipped-result))
  ])

(define/contract (cline-flip cline)
  (-> cline? cline?)
  (mat cline (internal:cline #/cline-internals-flip cline) cline
  #/internal:cline #/cline-internals-flip cline))



; ===== Merges and fuses =============================================

(define/contract (merge? x)
  (-> any/c boolean?)
  (internal:merge? x))

(define/contract (autoname-merge x)
  (-> merge? any)
  (dissect x (internal:merge internals)
  #/cons 'name:merge #/internal:furge-internals-autoname internals))

(define/contract (getfx-call-merge merge a b)
  (-> merge? any/c any/c #/getfx/c maybe?)
  (dissect merge (internal:merge internals)
  #/internal:getfx-furge-internals-call internals a b))

(define/contract (fuse? x)
  (-> any/c boolean?)
  (internal:fuse? x))

(define/contract (autoname-fuse x)
  (-> fuse? any)
  (dissect x (internal:fuse internals)
  #/cons 'name:fuse #/internal:furge-internals-autoname internals))

(define/contract (getfx-call-fuse fuse a b)
  (-> fuse? any/c any/c #/getfx/c maybe?)
  (dissect fuse (internal:fuse internals)
  #/internal:getfx-furge-internals-call internals a b))


(struct-easy (dex-internals-merge)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-merge)
    
    (define (dex-internals-autoname this)
      'tag:dex-merge)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (merge? x))
    
    (define (dex-internals-name-of this x)
      (if (merge? x)
        (just #/internal:name #/autoname-merge x)
        (nothing)))
    
    (define (dex-internals-dexed-of this x)
      (dex-internals-simple-dexed-of this x))
    
    (define (dex-internals-compare this a b)
      (expect a (internal:merge a) (nothing)
      #/expect b (internal:merge b) (nothing)
      #/w- tag internal:furge-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:furge-internals-autodex a b))
  ])

(define/contract (dex-merge)
  (-> dex?)
  (internal:dex #/dex-internals-merge))


(struct-easy (dex-internals-fuse)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-fuse)
    
    (define (dex-internals-autoname this)
      'tag:dex-fuse)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (fuse? x))
    
    (define (dex-internals-name-of this x)
      (if (fuse? x)
        (just #/internal:name #/autoname-fuse x)
        (nothing)))
    
    (define (dex-internals-dexed-of this x)
      (dex-internals-simple-dexed-of this x))
    
    (define (dex-internals-compare this a b)
      (expect a (internal:fuse a) (nothing)
      #/expect b (internal:fuse b) (nothing)
      #/w- tag internal:furge-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:furge-internals-autodex a b))
  ])

(define/contract (dex-fuse)
  (-> dex?)
  (internal:dex #/dex-internals-fuse))


(struct-easy (fuse-internals-by-merge merge)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-by-merge)
    
    (define (furge-internals-autoname this)
      (dissect this (fuse-internals-by-merge merge)
      #/list 'tag:fuse-by-merge #/autoname-merge merge))
    
    (define (furge-internals-autodex this other)
      (dissect this (fuse-internals-by-merge a)
      #/dissect other (fuse-internals-by-merge b)
      #/compare-by-dex (dex-merge) a b))
    
    (define (getfx-furge-internals-call this a b)
      (dissect this (fuse-internals-by-merge merge)
      #/getfx-call-merge merge a b))
  ])

(define/contract (fuse-by-merge merge)
  (-> merge? fuse?)
  (internal:fuse #/fuse-internals-by-merge merge))


(struct-easy (furge-internals-by-dex dex)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:furge-by-dex)
    
    (define (furge-internals-autoname this)
      (dissect this (furge-internals-by-dex dex)
      #/list 'tag:furge-by-dex #/autoname-dex dex))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-by-dex a)
      #/dissect other (furge-internals-by-dex b)
      #/compare-by-dex (dex-dex) a b))
    
    (define (getfx-furge-internals-call this a b)
      (dissect this (furge-internals-by-dex dex)
      #/getfx-done
        (mat (compare-by-dex dex a b) (just #/ordering-eq)
          (just a)
          (nothing))))
  ])

(define/contract (merge-by-dex dex)
  (-> dex? merge?)
  (internal:merge #/furge-internals-by-dex dex))

(struct-easy (furge-internals-by-cline-min cline)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:furge-by-cline-min)
    
    (define (furge-internals-autoname this)
      (dissect this (furge-internals-by-cline-min cline)
      #/list 'tag:furge-by-cline-min #/autoname-cline cline))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-by-cline-min a)
      #/dissect other (furge-internals-by-cline-min b)
      #/compare-by-dex (dex-cline) a b))
    
    (define (getfx-furge-internals-call this a b)
      (dissect this (furge-internals-by-cline-min cline)
      #/getfx-done
        (maybe-map (compare-by-cline cline a b) #/fn cline-result
          (mat cline-result (ordering-gt) b
            a))))
  ])

(define/contract (merge-by-cline-min cline)
  (-> cline? merge?)
  (internal:merge #/furge-internals-by-cline-min cline))

(define/contract (merge-by-cline-max cline)
  (-> cline? merge?)
  (internal:merge #/furge-internals-by-cline-min #/cline-flip cline))


(define-syntax-rule
  (define-furge-opaque
    internal:furge
    furge?
    autoname-furge
    getfx-call-furge
    dex-furge
    furge-internals-opaque
    tag:furge-opaque
    furge-opaque)
  (begin
    
    (struct-easy (furge-internals-opaque name furge)
      #:other
      
      #:methods internal:gen:furge-internals
      [
        
        (define (furge-internals-tag this)
          'tag:furge-opaque)
        
        (define (furge-internals-autoname this)
          (dissect this (fuse-internals-by-merge furge)
          #/list 'tag:furge-opaque #/autoname-furge furge))
        
        (define (furge-internals-autodex this other)
          (dissect this (furge-internals-opaque a-name a-furge)
          #/dissect other (furge-internals-opaque b-name b-furge)
          #/maybe-ordering-or
            (compare-by-dex (dex-name) a-name b-name)
            (compare-by-dex (dex-furge) a-furge b-furge)))
        
        (define (getfx-furge-internals-call this a b)
          (dissect this (furge-internals-opaque name furge)
          #/getfx-call-furge furge a b))
      ])
    
    (define/contract (furge-opaque name furge)
      (-> name? furge? furge?)
      (internal:furge #/furge-internals-opaque name furge))
  ))

(define-furge-opaque
  internal:merge
  merge?
  autoname-merge
  getfx-call-merge
  dex-merge
  merge-internals-opaque
  tag:merge-opaque
  merge-opaque)

(define-furge-opaque
  internal:fuse
  fuse?
  autoname-fuse
  getfx-call-fuse
  dex-fuse
  fuse-internals-opaque
  tag:fuse-opaque
  fuse-opaque)


(define-syntax-rule
  (define-furge-by-own-method
    internal:furge
    furge?
    getfx-call-furge
    dex-furge
    furge-by-own-method::getfx-err-different-input-methods
    furge-by-own-method::getfx-err-cannot-get-output-method
    furge-by-own-method::getfx-err-different-output-method
    furge-by-own-method::getfx-get-method
    furge-by-own-method-delegate/c
    furge-internals-by-own-method
    tag:furge-by-own-method
    furge-by-own-method-thorough
    furge-by-own-method-unthorough
    furge-by-own-method
    expected-getfx-err-different-input-methods
    expected-err-different-input-methods
    expected-getfx-err-cannot-get-output-method
    expected-err-cannot-get-output-method
    expected-getfx-err-different-output-method
    expected-err-different-output-method
    furge-result-str
    expected-getfx-delegate-get-method
    expected-delegate-get-method
    expected-getfx-get-method
    expected-get-method)
  (begin
    
    (struct-easy
      (furge-by-own-method::getfx-err-different-input-methods
        a b a-method b-method))
    (struct-easy
      (furge-by-own-method::getfx-err-cannot-get-output-method
        a b result input-method))
    (struct-easy
      (furge-by-own-method::getfx-err-different-output-method
        a b result input-method output-method))
    (struct-easy
      (furge-by-own-method::getfx-get-method source))
    
    (define/contract furge-by-own-method-delegate/c
      contract?
      (case->
        (->
          (match/c
            furge-by-own-method::getfx-err-different-input-methods
            any/c any/c furge? furge?)
          (getfx/c none/c))
        (->
          (match/c
            furge-by-own-method::getfx-err-cannot-get-output-method
            any/c any/c any/c furge?)
          (getfx/c none/c))
        (->
          (match/c
            furge-by-own-method::getfx-err-different-output-method
            any/c any/c any/c furge? furge?)
          (getfx/c none/c))
        (->
          (match/c furge-by-own-method::getfx-get-method any/c)
          (getfx/c #/maybe/c furge?))))
    
    (define/contract
      (getfx-err-furge-internals-by-own-method-delegate-different-input-methods
        dexed-delegate a b a-method b-method)
      (->
        (dexed-first-order/c furge-by-own-method-delegate/c)
        any/c
        any/c
        furge?
        furge?
        (getfx/c none/c))
      (w- delegate (dexed-get-value dexed-delegate)
      #/w- getfx-delegate-result
        (delegate #/furge-by-own-method::getfx-err-different-input-methods
          a b a-method b-method)
      #/expect (getfx? getfx-delegate-result) #t
        (getfx-err-unraise #/raise-arguments-error
          'furge-by-own-method-thorough
          expected-getfx-err-different-input-methods
          "dexed-delegate" dexed-delegate
          "a" a
          "b" b
          "a-method" a-method
          "b-method" b-method
          "getfx-delegate-result" getfx-delegate-result)
      #/getfx-bind getfx-delegate-result #/fn delegate-result
      #/getfx-err-unraise #/raise-arguments-error
        'furge-by-own-method-thorough
        expected-err-different-input-methods
        "dexed-delegate" dexed-delegate
        "a" a
        "b" b
        "a-method" a-method
        "b-method" b-method
        "delegate-result" delegate-result))
    
    (define/contract
      (getfx-err-furge-internals-by-own-method-delegate-cannot-get-output-method
        dexed-delegate a b furge-result input-method)
      (->
        (dexed-first-order/c furge-by-own-method-delegate/c)
        any/c
        any/c
        any/c
        furge?
        (getfx/c none/c))
      (w- delegate (dexed-get-value dexed-delegate)
      #/w- getfx-delegate-result
        (delegate #/furge-by-own-method::getfx-err-cannot-get-output-method
          a b furge-result input-method)
      #/expect (getfx? getfx-delegate-result) #t
        (getfx-err-unraise #/raise-arguments-error
          'furge-by-own-method-thorough
          expected-getfx-err-cannot-get-output-method
          "dexed-delegate" dexed-delegate
          "a" a
          "b" b
          furge-result-str furge-result
          "input-method" input-method
          "getfx-delegate-result" getfx-delegate-result)
      #/getfx-bind getfx-delegate-result #/fn delegate-result
      #/getfx-err-unraise #/raise-arguments-error
        'furge-by-own-method-thorough
        expected-err-cannot-get-output-method
        "dexed-delegate" dexed-delegate
        "a" a
        "b" b
        furge-result-str furge-result
        "input-method" input-method
        "delegate-result" delegate-result))
    
    (define/contract
      (getfx-err-furge-internals-by-own-method-delegate-different-output-method
        dexed-delegate a b furge-result input-method output-method)
      (->
        (dexed-first-order/c furge-by-own-method-delegate/c)
        any/c
        any/c
        any/c
        furge?
        furge?
        (getfx/c none/c))
      (w- delegate (dexed-get-value dexed-delegate)
      #/w- getfx-delegate-result
        (delegate #/furge-by-own-method::getfx-err-different-output-method
          a b furge-result input-method output-method)
      #/expect (getfx? getfx-delegate-result) #t
        (getfx-err-unraise #/raise-arguments-error
          'furge-by-own-method-thorough
          expected-getfx-err-different-output-method
          "dexed-delegate" dexed-delegate
          "a" a
          "b" b
          furge-result-str furge-result
          "input-method" input-method
          "output-method" output-method
          "getfx-delegate-result" getfx-delegate-result)
      #/getfx-bind getfx-delegate-result #/fn delegate-result
      #/getfx-err-unraise #/raise-arguments-error
        'furge-by-own-method-thorough
        expected-err-different-output-method
        "dexed-delegate" dexed-delegate
        "a" a
        "b" b
        furge-result-str furge-result
        "input-method" input-method
        "output-method" output-method
        "delegate-result" delegate-result))
    
    (define/contract
      (getfx-furge-internals-by-own-method-delegate-get-method
        dexed-delegate source)
      (-> (dexed-first-order/c furge-by-own-method-delegate/c) any/c
        (getfx/c #/maybe/c furge?))
      (w- delegate (dexed-get-value dexed-delegate)
      #/w- getfx-delegate-result
        (delegate #/furge-by-own-method::getfx-get-method source)
      #/expect (getfx? getfx-delegate-result) #t
        (getfx-err-unraise #/raise-arguments-error
          'furge-by-own-method-thorough
          expected-getfx-delegate-get-method
          "dexed-delegate" dexed-delegate
          "source" source
          "getfx-delegate-result" getfx-delegate-result)
      #/getfx-bind getfx-delegate-result #/fn delegate-result
      #/expect
        (contract-first-order-passes? (maybe/c furge?)
          delegate-result)
        #t
        (getfx-err-unraise #/raise-arguments-error
          'furge-by-own-method-thorough
          expected-delegate-get-method
          "dexed-delegate" dexed-delegate
          "source" source
          "delegate-result" delegate-result)
      #/getfx-done delegate-result))
    
    (struct-easy (furge-internals-by-own-method dexed-delegate)
      #:other
      
      #:methods internal:gen:furge-internals
      [
        
        (define (furge-internals-tag this)
          'tag:furge-by-own-method)
        
        (define (furge-internals-autoname this)
          (dissect this (furge-internals-by-own-method dexed-delegate)
          #/dissect (dexed-get-name dexed-delegate)
            (internal:name rep)
          #/list 'tag:furge-by-own-method rep))
        
        (define (furge-internals-autodex this other)
          (dissect this (furge-internals-by-own-method a)
          #/dissect other (furge-internals-by-own-method b)
          #/compare-by-dex (dex-dexed) a b))
        
        (define (getfx-furge-internals-call this a b)
          (dissect this (furge-internals-by-own-method dexed-delegate)
          #/getmaybefx-bind
            (getfx-furge-internals-by-own-method-delegate-get-method
              dexed-delegate a)
          #/fn a-method
          #/getmaybefx-bind
            (getfx-furge-internals-by-own-method-delegate-get-method
              dexed-delegate b)
          #/fn b-method
          #/expect (compare-by-dex (dex-furge) a-method b-method)
            (just #/ordering-eq)
            (getfx-err-furge-internals-by-own-method-delegate-different-input-methods
              dexed-delegate a b a-method b-method)
          #/getmaybefx-bind (getfx-call-furge a-method a b)
          #/fn result
          #/getfx-bind
            (getfx-furge-internals-by-own-method-delegate-get-method
              dexed-delegate result)
          #/fn maybe-result-method
          #/expect maybe-result-method (just result-method)
            (getfx-err-furge-internals-by-own-method-delegate-cannot-get-output-method
              dexed-delegate a b result a-method)
          #/expect (compare-by-dex (dex-furge) a-method result-method)
            (just #/ordering-eq)
            (getfx-err-furge-internals-by-own-method-delegate-different-output-method
              dexed-delegate a b result a-method result-method)
          #/getfx-done #/just result))
      ])
    
    (define/contract (furge-by-own-method-thorough dexed-delegate)
      (-> (dexed-first-order/c furge-by-own-method-delegate/c) furge?)
      (internal:furge #/furge-internals-by-own-method dexed-delegate))
    
    (struct-easy
      (furge-by-own-method-unthorough dexed-getfx-get-method)
      #:other
      
      #:property prop:procedure
      (fn this command
        (dissect this
          (furge-by-own-method-unthorough dexed-getfx-get-method)
        #/mat command
          (furge-by-own-method::getfx-err-different-input-methods
            a b a-method b-method)
          (getfx-err-unraise #/raise-arguments-error
            'furge-by-own-method
            "obtained two different methods from the two input values"
            "dexed-getfx-get-method" dexed-getfx-get-method
            "a" a
            "b" b
            "a-method" a-method
            "b-method" b-method)
        #/mat command
          (furge-by-own-method::getfx-err-cannot-get-output-method
            a b result input-method)
          (getfx-err-unraise #/raise-arguments-error
            'furge-by-own-method
            "could not obtain a method from the result value"
            "dexed-getfx-get-method" dexed-getfx-get-method
            "a" a
            "b" b
            "result" result
            "input-method" input-method)
        #/mat command
          (furge-by-own-method::getfx-err-different-output-method
            a b result input-method output-method)
          (getfx-err-unraise #/raise-arguments-error
            'furge-by-own-method
            "obtained two different methods from the input and the output"
            "dexed-getfx-get-method" dexed-getfx-get-method
            "a" a
            "b" b
            "result" result
            "input-method" input-method
            "output-method" output-method)
        #/dissect command
          (furge-by-own-method::getfx-get-method source)
        #/w- getfx-get-method (dexed-get-value dexed-getfx-get-method)
        #/w- getfx-method (getfx-get-method source)
        #/expect (getfx? getfx-method) #t
          (getfx-err-unraise #/raise-arguments-error
            'furge-by-own-method
            expected-getfx-get-method
            "dexed-getfx-get-method" dexed-getfx-get-method
            "source" source
            "getfx-method" getfx-method)
        #/getfx-bind getfx-method #/fn method
        #/expect
          (contract-first-order-passes? (maybe/c furge?) method)
          #t
          (getfx-err-unraise #/raise-arguments-error
            'furge-by-own-method
            expected-get-method
            "dexed-getfx-get-method" dexed-getfx-get-method
            "source" source
            "method" method)
          method)))
    
    (define/contract (furge-by-own-method dexed-get-method)
      (-> (dexed-first-order/c #/-> any/c #/getfx/c #/maybe/c furge?)
        furge?)
      (furge-by-own-method-thorough #/just-value #/dexed-of
        (dex-struct furge-by-own-method-unthorough #/dex-dexed)
        (furge-by-own-method-unthorough dexed-get-method)))
  ))

(define-furge-by-own-method
  internal:merge
  merge?
  getfx-call-merge
  dex-merge
  merge-by-own-method::getfx-err-different-input-methods
  merge-by-own-method::getfx-err-cannot-get-output-method
  merge-by-own-method::getfx-err-different-output-method
  merge-by-own-method::getfx-get-method
  merge-by-own-method-delegate/c
  merge-internals-by-own-method
  tag:merge-by-own-method
  merge-by-own-method-thorough
  merge-by-own-method-unthorough
  merge-by-own-method
  "expected the pure result of dexed-delegate for merge-by-own-method::getfx-err-different-input-methods to be a getfx effectful computation"
  "expected dexed-delegate not to have a result for merge-by-own-method::getfx-err-different-input-methods"
  "expected the pure result of dexed-delegate for merge-by-own-method::getfx-err-cannot-get-output-method to be a getfx effectful computation"
  "expected dexed-delegate not to have a result for merge-by-own-method::getfx-err-cannot-get-output-method"
  "expected the pure result of dexed-delegate for merge-by-own-method::getfx-err-different-output-method to be a getfx effectful computation"
  "expected dexed-delegate not to have a result for merge-by-own-method::getfx-err-different-output-method"
  "merge-result"
  "expected the pure result of dexed-delegate for merge-by-own-method::getfx-get-method to be a getfx effectful computation"
  "expected the result of dexed-delegate for merge-by-own-method::getfx-get-method to be a maybe of a merge"
  "expected the pure result of dexed-getfx-get-method to be a getfx effectful computation"
  "expected the result of dexed-getfx-get-method to be a maybe of a merge")

(define-furge-by-own-method
  internal:fuse
  fuse?
  getfx-call-fuse
  dex-fuse
  fuse-by-own-method::getfx-err-different-input-methods
  fuse-by-own-method::getfx-err-cannot-get-output-method
  fuse-by-own-method::getfx-err-different-output-method
  fuse-by-own-method::getfx-get-method
  fuse-by-own-method-delegate/c
  fuse-internals-by-own-method
  tag:fuse-by-own-method
  fuse-by-own-method-thorough
  fuse-by-own-method-unthorough
  fuse-by-own-method
  "expected the pure result of dexed-delegate for fuse-by-own-method::getfx-err-different-input-methods to be a getfx effectful computation"
  "expected dexed-delegate not to have a result for fuse-by-own-method::getfx-err-different-input-methods"
  "expected the pure result of dexed-delegate for fuse-by-own-method::getfx-err-cannot-get-output-method to be a getfx effectful computation"
  "expected dexed-delegate not to have a result for fuse-by-own-method::getfx-err-cannot-get-output-method"
  "expected the pure result of dexed-delegate for fuse-by-own-method::getfx-err-different-output-method to be a getfx effectful computation"
  "expected dexed-delegate not to have a result for fuse-by-own-method::getfx-err-different-output-method"
  "fuse-result"
  "expected the pure result of dexed-delegate for fuse-by-own-method::getfx-get-method to be a getfx effectful computation"
  "expected the result of dexed-delegate for fuse-by-own-method::getfx-get-method to be a maybe of a fuse"
  "expected the pure result of dexed-getfx-get-method to be a getfx effectful computation"
  "expected the result of dexed-getfx-get-method to be a maybe of a fuse")


(struct-easy
  (furge-internals-fix
    getfx-call-furge getfx-unwrap-internals dexed-getfx-unwrap)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:furge-fix)
    
    (define (furge-internals-autoname this)
      (dissect this (furge-internals-fix _ _ dexed-getfx-unwrap)
      #/dissect (dexed-get-name dexed-getfx-unwrap)
        (internal:name rep)
      #/list 'tag:furge-fix rep))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-fix _ _ a)
      #/dissect other (furge-internals-fix _ _ b)
      #/compare-by-dex (dex-dexed) a b))
    
    (define (getfx-furge-internals-call this a b)
      (dissect this
        (furge-internals-fix
          getfx-call-furge getfx-unwrap-internals dexed-getfx-unwrap)
      #/getfx-bind (getfx-unwrap-internals this dexed-getfx-unwrap)
      #/fn unwrapped
      #/getfx-call-furge unwrapped a b))
  ])

(define/contract (merge-fix dexed-getfx-unwrap)
  (-> (dexed-first-order/c #/-> merge? merge?) merge?)
  (internal:merge #/furge-internals-fix
    getfx-call-merge
    (fn internals dexed-getfx-unwrap
      (w- this (internal:merge internals)
      #/w- getfx-unwrap (dexed-get-value dexed-getfx-unwrap)
      #/w- getfx-result (getfx-unwrap this)
      #/expect (getfx? getfx-result) #t
        (getfx-err-unraise #/raise-arguments-error 'merge-fix
          "expected the pure result of dexed-getfx-unwrap to be a getfx effectful computation"
          "dexed-getfx-unwrap" dexed-getfx-unwrap
          "this" this
          "getfx-result" getfx-result)
      #/getfx-bind getfx-result #/fn result
      #/expect (merge? result) #t
        (getfx-err-unraise #/raise-arguments-error 'merge-fix
          "expected the result of dexed-getfx-unwrap to be a merge"
          "dexed-getfx-unwrap" dexed-getfx-unwrap
          "this" this
          "result" result)
      #/getfx-done result))
    dexed-getfx-unwrap))

(define/contract (fuse-fix dexed-getfx-unwrap)
  (-> (dexed-first-order/c #/-> fuse? fuse?) fuse?)
  (internal:fuse #/furge-internals-fix
    getfx-call-fuse
    (fn internals dexed-getfx-unwrap
      (w- this (internal:fuse internals)
      #/w- getfx-unwrap (dexed-get-value dexed-getfx-unwrap)
      #/w- getfx-result (getfx-unwrap this)
      #/expect (getfx? getfx-result) #t
        (getfx-err-unraise #/raise-arguments-error 'fuse-fix
          "expected the pure result of dexed-getfx-unwrap to be a getfx effectful computation"
          "dexed-getfx-unwrap" dexed-getfx-unwrap
          "this" this
          "getfx-result" getfx-result)
      #/getfx-bind getfx-result #/fn result
      #/expect (fuse? result) #t
        (getfx-err-unraise #/raise-arguments-error 'fuse-fix
          "expected the result of dexed-getfx-unwrap to be a fuse"
          "dexed-getfx-unwrap" dexed-getfx-unwrap
          "this" this
          "result" result)
      #/getfx-done result))
    dexed-getfx-unwrap))


(struct-easy
  (furge-internals-struct
    autoname-furge dex-furge getfx-call-furge
    descriptor constructor counts? fields)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:furge-struct-by-field-position)
    
    (define (furge-internals-autoname this)
      (dissect this
        (furge-internals-struct
          autoname-furge _ _ descriptor constructor counts? fields)
      #/list* 'tag:furge-struct-by-field-position descriptor
      #/list-map fields #/dissectfn (list getter position furge)
        (list position #/autoname-furge furge)))
    
    (define (furge-internals-autodex this other)
      (dissect this
        (furge-internals-struct
          _ dex-furge _ a-descriptor _ _ a-fields)
      #/dissect other
        (furge-internals-struct
          _ _ _ b-descriptor _ _ b-fields)
      #/maybe-ordering-or
        (just #/object-identities-autodex a-descriptor b-descriptor)
      #/maybe-ordering-or
        (maybe-compare-aligned-lists a-fields b-fields
        #/fn a-field b-field
          (dissect a-field (list a-getter a-position a-furge)
          #/dissect b-field (list b-getter b-position b-furge)
          #/just #/lt-autodex a-position b-position <))
        (maybe-compare-aligned-lists a-fields b-fields
        #/fn a-field b-field
          (dissect a-field (list a-getter a-position a-furge)
          #/dissect b-field (list b-getter b-position b-furge)
          #/compare-by-dex dex-furge a-furge b-furge))))
    
    (define (getfx-furge-internals-call this a b)
      (dissect this
        (furge-internals-struct
          _ _ getfx-call-furge descriptor constructor counts? fields)
      #/expect (counts? a) #t (getfx-done #/nothing)
      #/expect (counts? b) #t (getfx-done #/nothing)
      #/w- n (length fields)
      #/w-loop next fields fields args (hasheq)
        (expect fields (cons field fields)
          (getfx-done #/just
            (apply constructor
              (build-list n #/fn i #/hash-ref args i)))
        #/dissect field (list getter position furge)
        #/getmaybefx-bind
          (getfx-call-furge furge (getter a) (getter b))
        #/fn furged
        #/next fields (hash-set args position furged))))
  ])

(define-for-syntax
  (expand-furge-struct-by-field-position
    stx furges-message furge-encapsulated-id autoname-furge-id
    dex-furge-id getfx-call-furge-id)
  (syntax-parse stx #/
    (_ struct-tag:id [field-position:nat field-furge:expr] ...)
  #/dissect (get-immutable-root-ancestor-struct-info stx #'struct-tag)
    (list struct:foo make-foo foo? getters)
  #/w- fields (desyntax-list #'#/[field-position field-furge] ...)
  #/w- n (length getters)
  #/expect (= n (length fields)) #t
    (raise-syntax-error #f
      (format "expected ~s ~a, got ~s"
        n
        furges-message
        (length fields))
      stx)
  #/w- seen (make-hasheq)
  #/syntax-protect
    #`(#,furge-encapsulated-id
      #/furge-internals-struct
        #,autoname-furge-id #,dex-furge-id #,getfx-call-furge-id
        #,struct:foo #,make-foo #,foo?
      #/list
        #,@(list-map fields #/fn field
             (dissect (desyntax-list field)
               (list position-stx furge)
             #/w- position (syntax-e position-stx)
             #/expect (< position n) #t
               (raise-syntax-error #f
                 (format
                   "expected a field position less than ~s, got ~s"
                   n
                   position)
                 stx position-stx)
             #/expect (hash-has-key? seen position) #f
               (raise-syntax-error #f
                 "duplicate field position"
                 stx position-stx)
             #/begin (hash-set! seen position #t)
               #`(list
                   #,(list-ref getters position)
                   #,position-stx
                   #,furge))))))

(define-syntax (merge-struct-by-field-position stx)
  (expand-furge-struct-by-field-position stx "merges"
    #'internal:merge
    #'autoname-merge
    #'(dex-merge)
    #'getfx-call-merge))

(define-syntax (fuse-struct-by-field-position stx)
  (expand-furge-struct-by-field-position stx "fuses"
    #'internal:fuse
    #'autoname-fuse
    #'(dex-fuse)
    #'getfx-call-fuse))

(define-for-syntax
  (expand-furge-struct
    stx furges-message furge-encapsulated-id autoname-furge-id
    dex-furge-id getfx-call-furge-id)
  (syntax-parse stx #/ (_ struct-tag:id field-furge:expr ...)
  #/dissect (get-immutable-root-ancestor-struct-info stx #'struct-tag)
    (list struct:foo make-foo foo? getters)
  #/w- fields (desyntax-list #'#/field-furge ...)
  #/w- n (length getters)
  #/expect (= n (length fields)) #t
    (raise-syntax-error #f
      (format "expected ~s ~a, got ~s"
        n
        furges-message
        (length fields))
      stx)
  #/syntax-protect
    #`(#,furge-encapsulated-id
      #/furge-internals-struct
        #,autoname-furge-id #,dex-furge-id #,getfx-call-furge-id
        #,struct:foo #,make-foo #,foo?
      #/list
        #,@(list-kv-map (map list fields getters) #/fn position field
             (dissect field (list furge getter)
               #`(list #,getter #,position #,furge))))))

(define-syntax (merge-struct stx)
  (expand-furge-struct stx "merges"
    #'internal:merge
    #'autoname-merge
    #'(dex-merge)
    #'getfx-call-merge))

(define-syntax (fuse-struct stx)
  (expand-furge-struct stx "fuses"
    #'internal:fuse
    #'autoname-fuse
    #'(dex-fuse)
    #'getfx-call-fuse))



; ===== Tables =======================================================

(define/contract (table? x)
  (-> any/c boolean?)
  (internal:table? x))

(define/contract (table-empty? t)
  (-> table? boolean?)
  (dissect t (internal:table t)
  #/hash-empty? t))

(define/contract (table-get key table)
  (-> name? table? maybe?)
  (dissect key (internal:name key)
  #/dissect table (internal:table hash)
  #/hash-ref-maybe hash key))

(define/contract (table-empty)
  (-> table?)
  (internal:table #/hash))

(define/contract (table-shadow key maybe-val table)
  (-> name? maybe? table? table?)
  (dissect key (internal:name key)
  #/dissect table (internal:table hash)
  #/internal:table #/hash-set-maybe hash key maybe-val))

(define/contract
  (getfx-table-map-fuse table fuse getfx-key-to-operand)
  (-> table? fuse? (-> name? getfx?) #/getfx/c maybe?)
  (dissect table (internal:table hash)
  #/getfx-bind
    (getfx-list-map #/list-map (hash-keys hash) #/fn key
      (getfx-key-to-operand #/internal:name key))
  #/fn operands
  
  ; NOTE: We do a first pass over the operands to make sure they're
  ; all in the fuse's domain because otherwise a client could detect
  ; that their sometimes-non-terminating `fuse-by-own-method` wasn't
  ; called on certain operands.
  #/getmaybefx-bind
    (getmaybefx-list-map #/list-map operands #/fn operand
      (getfx-call-fuse fuse operand operand))
  #/dissectfn _
  
  #/expect operands (cons so-far operands) (getfx-done #/nothing)
  #/w-loop next so-far so-far operands operands
    (expect operands (cons operand operands)
      (getfx-done #/just so-far)
    #/getmaybefx-bind (getfx-call-fuse fuse so-far operand)
    #/fn so-far
    #/next so-far operands)))

(define/contract (assocs->table-if-mutually-unique assocs)
  (-> (listof #/cons/c name? any/c) #/maybe/c table?)
  (w-loop next assocs assocs result (table-empty)
    (expect assocs (cons (cons k v) assocs) (just result)
    #/expect (table-get k result) (nothing) (nothing)
    #/next assocs #/table-shadow k (just v) result)))

(define/contract (table-sort cline table)
  (-> cline? table? #/maybe/c #/listof table?)
  (dissect table (internal:table hash)
  #/w- unsorted (hash->list hash)
  
  ; NOTE: We do a first pass over the operands to make sure they're
  ; all in the cline's domain because otherwise a client could detect
  ; that their sometimes-non-terminating `cline-by-own-method` wasn't
  ; called on certain operands. This also makes it easy to take care
  ; of every condition where we need to return `(nothing)`.
  #/expect
    (list-all unsorted #/dissectfn (cons k v) #/in-cline? cline v)
    #t
    (nothing)
  #/just
  
  #/w- sorted-flat
    (sort (hash->list hash) #/fn a b
      (dissect a (cons ak av)
      #/dissect b (cons bk bv)
      #/dissect (compare-by-cline cline av bv) (just cline-result)
      #/mat cline-result (ordering-lt) #t
        #f))
  #/w- sorted-flat
    (list-map sorted-flat #/dissectfn (cons k v)
      (cons (internal:name k) v))
  #/w-loop next
    sorted-flat sorted-flat
    current-group (list)
    rev-sorted-grouped (list)
    
    (expect sorted-flat (cons entry sorted-flat)
      (mat current-group (list)
        (reverse rev-sorted-grouped)
        (dissect (assocs->table-if-mutually-unique current-group)
          (just current-group)
        #/reverse #/cons current-group rev-sorted-grouped))
    #/dissect entry (cons k v)
    #/expect current-group (cons existing-v _)
      (next sorted-flat (list entry) rev-sorted-grouped)
    #/dissect (compare-by-cline cline existing-v v)
      (just cline-result)
    #/mat cline-result (ordering-lt)
      ; The element we're on is part of a new group, so we commit the
      ; current group to the result.
      (dissect (assocs->table-if-mutually-unique current-group)
        (just current-group)
      #/next sorted-flat (list entry)
        (cons current-group rev-sorted-grouped))
      ; The element we're on is part of the current group.
      (next sorted-flat (cons entry current-group)
        rev-sorted-grouped))))

; TODO: See if we should export this. It can be implemented in
; `effection/order` using `fuse-exact-rational-by-plus`.
(define/contract (table-count tab)
  (-> table? natural?)
  (dissect tab (internal:table hash)
  #/hash-count hash))

(define/contract (table->sorted-list tab)
  (-> table? #/listof #/list/c name? any/c)
  (dissect tab (internal:table hash)
  #/list-map
    (sort (hash->list hash) #/fn a b
      (dissect a (cons ak av)
      #/dissect b (cons bk bv)
      #/w- cline-result
        (names-autocline-candid (internal:name ak) (internal:name bk))
      #/mat cline-result (ordering-lt) #t
        #f))
  #/dissectfn (cons k v)
    (list (internal:name k) v)))

; TODO: See if we should export this from somewhere.
(define/contract (monad-table-each fx-done fx-bind table-of-fx)
  (-> (-> any/c any/c) (-> any/c (-> any/c any/c) any/c) table? any/c)
  (monad-map fx-done fx-bind
    (monad-list-map fx-done fx-bind
      (list-map (table->sorted-list table-of-fx)
      #/dissectfn (list k fx-v)
        (monad-map fx-done fx-bind fx-v #/fn v #/list k v)))
  #/fn assocs
    (dissect (assocs->table-if-mutually-unique assocs) (just result)
      result)))

; TODO: See if we should export this.
(define/contract (getmaybefx-table-each table-of-getmaybefx)
  (-> table? #/getfx/c #/maybe/c table?)
  ; TODO: If we export this, export it under this contract instead.
  ; The problem with using this contract internally is that the
  ; implementation of `table-v-of` depends on `merge-table`, which
  ; depends on this.
;  (-> (table-v-of #/getfx/c maybe?) #/getfx/c #/maybe/c table?)
  (monad-table-each
    (fn result #/getfx-done #/just result)
    (fn effects then #/getmaybefx-bind effects then)
    table-of-getmaybefx))

(struct-easy (dex-internals-table dex-val)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-table)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-table dex-val)
      #/list 'tag:dex-table #/autoname-dex dex-val))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-table a-dex-val)
      #/dissect other (dex-internals-table b-dex-val)
      #/compare-by-dex (dex-dex) a-dex-val b-dex-val))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-table dex-val)
      #/expect x (internal:table x) #f
      #/hash-v-all x #/fn val
        (in-dex? dex-val val)))
    
    ; TODO: See if we should have the ordering of the
    ; `internal:dex-internals-name-of` names of tables be consistent
    ; with the `internal:dex-internals-compare` ordering of the tables
    ; themselves.
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-table dex-val)
      ; TODO: Currently, this calls `in-dex?` on each value of the
      ; table (indirectly via this one `internal:dex-internals-in?`
      ; call), and then if they all succeed, it calls `name-of`. This
      ; could be doing some redundant computation. See if we can
      ; optimize this.
      #/if (not #/internal:dex-internals-in? this x) (nothing)
      #/just #/internal:name #/cons 'name:table
        (list-bind (table->sorted-list x)
        #/dissectfn (list (internal:name k-rep) v)
          (dissect (name-of dex-val v) (just #/internal:name v-rep)
          #/list k-rep v-rep))))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-table dex-val)
      ; TODO: Currently, this calls `in-dex?` on each value of the
      ; table (indirectly via this one `internal:dex-internals-in?`
      ; call), and then if they all succeed, it calls `dexed-of`. This
      ; could be doing some redundant computation. See if we can
      ; optimize this.
      #/if (not #/internal:dex-internals-in? this x) (nothing)
      #/w- dexeds
        (list-map (table->sorted-list x) #/dissectfn (list k v)
          (dissect (dexed-of dex-val v) (just dexed)
          #/list k dexed))
      #/just #/dexed
        ; TODO: Move the definition of `dex-internals-table-ordered`
        ; before this.
        (internal:dex #/dex-internals-table-ordered
          (list-map dexeds #/dissectfn (list k (dexed dex name v))
            (list k dex)))
        (internal:name #/cons 'name:table
          (list-bind dexeds
          #/dissectfn
            (list
              (internal:name k-rep)
              (dexed dex (internal:name v-rep) v))
            (list k-rep v-rep)))
        x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-table dex-val)
      ; TODO: Currently, this calls `in-dex?` on each value of each
      ; table (indirectly via these two `internal:dex-internals-in?`
      ; calls), and then if they all succeed, it calls
      ; `compare-by-dex` on each one too (up to the point, if any,
      ; where it exits early due to a nonequal result). This could be
      ; doing some redundant computation. See if we can optimize this.
      #/if (not #/internal:dex-internals-in? this a) (nothing)
      #/if (not #/internal:dex-internals-in? this b) (nothing)
      #/maybe-ordering-or
        (just #/lt-autodex (table-count a) (table-count b) <)
      #/w- a (table->sorted-list a)
      #/w- b (table->sorted-list b)
      #/maybe-ordering-or
        (w-loop next
          keys
          (map list
            (list-map a #/dissectfn (list k v) k)
            (list-map b #/dissectfn (list k v) k))
          (expect keys (cons entry keys) (just #/ordering-eq)
          #/dissect entry (list a b)
          #/maybe-ordering-or (just #/names-autodex a b)
          #/next keys))
        (w-loop next
          vals
          ; NOTE: If we get to this point, we compare *every* pair of
          ; corresponding values. That's because if we short-circuit
          ; before comparing certain values, then a client can deduce
          ; that their sometimes-non-terminating `dex-by-own-method`
          ; hasn't been called on certain values in the table.
          (list-map
            (map list
              (list-map a #/dissectfn (list k v) v)
              (list-map b #/dissectfn (list k v) v))
          #/dissectfn (list a b)
            (compare-by-dex dex-val a b))
          (expect vals (cons maybe-dex-result vals)
            (just #/ordering-eq)
          #/maybe-ordering-or maybe-dex-result
          #/next vals))))
  ])

(define/contract (dex-table dex-val)
  (-> dex? dex?)
  (internal:dex #/dex-internals-table dex-val))

(define/contract (table-ordered-counts? assoc v)
  (-> (listof #/list/c name? dex?) any/c boolean?)
  (and (table? v) (= (length assoc) (table-count v))
  #/list-all assoc #/dissectfn (list k dex-v)
    (just? #/table-get k v)))

(struct-easy (dex-internals-table-ordered assoc)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-table-ordered)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-table-ordered assoc)
      #/list* 'tag:dex-table-ordered
      #/list-map assoc #/dissectfn (list (internal:name k) dex-v)
        (list k #/autoname-dex dex-v)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-table-ordered a-assoc)
      #/dissect other (dex-internals-table-ordered b-assoc)
      #/maybe-ordering-or
        (just #/lt-autodex (length a-assoc) (length b-assoc) <)
      #/maybe-ordering-or
        (maybe-compare-aligned-lists a-assoc b-assoc
        #/fn a-entry b-entry
          (dissect a-entry (list a-k a-dex-v)
          #/dissect b-entry (list b-k b-dex-v)
          #/just #/names-autodex a-k b-k))
        (maybe-compare-aligned-lists a-assoc b-assoc
        #/fn a-entry b-entry
          (dissect a-entry (list a-k a-dex-v)
          #/dissect b-entry (list b-k b-dex-v)
          #/compare-by-dex (dex-dex) a-dex-v b-dex-v))))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-table-ordered assoc)
      #/and (table-ordered-counts? assoc x)
      #/w-loop next assoc assoc
        (expect assoc (cons entry assoc) #t
        #/dissect entry (list k dex-v)
        #/dissect (table-get k x) (just v)
        
        ; We do a tail call if we can.
        #/mat assoc (list) (in-dex? dex-v v)
        
        #/and (in-dex? dex-v v)
        #/next assoc)))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-table-ordered assoc)
      #/expect (table-ordered-counts? assoc x) #t (nothing)
      #/w-loop next assoc assoc reps (table-empty)
        (expect assoc (cons entry assoc)
          (just #/internal:name #/cons 'name:table
            (list-bind (table->sorted-list reps)
            #/dissectfn (list (internal:name k-rep) v-rep)
              (list k-rep v-rep)))
        #/dissect entry (list k dex-v)
        #/dissect (table-get k x) (just v)
        #/maybe-bind (name-of dex-v v) #/dissectfn (internal:name rep)
        #/next assoc #/table-shadow k (just rep) reps)))
    
    (define (dex-internals-dexed-of this x)
      (dissect this (dex-internals-table-ordered assoc)
      #/expect (table-ordered-counts? assoc x) #t (nothing)
      #/w-loop next assoc assoc dexeds (table-empty)
        (expect assoc (cons entry assoc)
          (w- dexeds (table->sorted-list dexeds)
          #/just #/dexed
            (internal:dex #/dex-internals-table-ordered
              (list-map dexeds #/dissectfn (list k (dexed dex name v))
                (list k dex)))
            (internal:name #/cons 'name:table
              (list-bind dexeds
              #/dissectfn
                (list
                  (internal:name k-rep)
                  (dexed dex (internal:name v-rep) v))
                (list k-rep v-rep)))
            x)
        #/dissect entry (list k dex-v)
        #/dissect (table-get k x) (just v)
        #/maybe-bind (dexed-of dex-v v) #/fn dexed
        #/next assoc #/table-shadow k (just dexed) dexeds)))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-table-ordered assoc)
      #/expect (table-ordered-counts? assoc a) #t (nothing)
      #/expect (table-ordered-counts? assoc b) #t (nothing)
      #/w-loop next assoc assoc
        (expect assoc (cons entry assoc) (just #/ordering-eq)
        #/dissect entry (list k dex-v)
        #/dissect (table-get k a) (just a-v)
        #/dissect (table-get k b) (just b-v)
        
        ; We do a tail call if we can.
        #/mat assoc (list) (compare-by-dex dex-v a-v b-v)
        
        #/w- result (compare-by-dex dex-v a-v b-v)
        #/expect result (just #/ordering-eq)
          (mat result (nothing) (nothing)
          ; We have a potential result to use, but first we check that
          ; the rest of the field values belong to their respective
          ; dexes' domains. If they don't, this structure instance is
          ; not part part of this dex's domain, so the result is
          ; `(nothing)`.
          #/w-loop next assoc assoc
            (expect assoc (cons entry assoc) result
            #/dissect entry (list k dex-v)
            #/dissect (table-get k a) (just a-v)
            #/dissect (table-get k b) (just b-v)
            #/expect (and (in-dex? dex-v a-v) (in-dex? dex-v b-v)) #t
              (nothing)
            #/next assoc))
        #/next assoc)))
  ])

(define/contract (dex-table-ordered assoc)
  (-> (listof #/list/c name? dex?) dex?)
  (expect (assocs->table-if-mutually-unique assoc) (just _)
    (raise-arguments-error 'dex-table-ordered
      "expected the keys to be mutually unique"
      "assoc" assoc)
  #/internal:dex #/dex-internals-table-ordered assoc))

(struct-easy (cline-internals-table-ordered assoc)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-table-ordered)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-table-ordered assoc)
      #/list* 'tag:cline-table-ordered
      #/list-map assoc #/dissectfn (list (internal:name k) cline-v)
        (list k #/autoname-cline cline-v)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-table-ordered a-assoc)
      #/dissect other (cline-internals-table-ordered b-assoc)
      #/maybe-ordering-or
        (just #/lt-autodex (length a-assoc) (length b-assoc) <)
      #/maybe-ordering-or
        (maybe-compare-aligned-lists a-assoc b-assoc
        #/fn a-entry b-entry
          (dissect a-entry (list a-k a-cline-v)
          #/dissect b-entry (list b-k b-cline-v)
          #/just #/names-autodex a-k b-k))
        (maybe-compare-aligned-lists a-assoc b-assoc
        #/fn a-entry b-entry
          (dissect a-entry (list a-k a-cline-v)
          #/dissect b-entry (list b-k b-cline-v)
          #/compare-by-dex (dex-cline) a-cline-v b-cline-v))))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-table-ordered assoc)
      ; NOTE: We inline the call to `dex-table-ordered` here because
      ; we don't need to check that the keys are mutually unique.
      #/internal:dex #/dex-internals-table-ordered
        (list-map assoc #/dissectfn (list k cline-v)
          (list k (get-dex-from-cline cline-v)))))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-table-ordered assoc)
      #/and (table-ordered-counts? assoc x)
      #/w-loop next assoc assoc
        (expect assoc (cons entry assoc) #t
        #/dissect entry (list k cline-v)
        #/dissect (table-get k x) (just v)
        
        ; We do a tail call if we can.
        #/mat assoc (list) (in-cline? cline-v v)
        
        #/and (in-cline? cline-v v)
        #/next assoc)))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-table-ordered assoc)
      #/expect (table-ordered-counts? assoc a) #t (nothing)
      #/expect (table-ordered-counts? assoc b) #t (nothing)
      #/w-loop next assoc assoc
        (expect assoc (cons entry assoc) (just #/ordering-eq)
        #/dissect entry (list k cline-v)
        #/dissect (table-get k a) (just a-v)
        #/dissect (table-get k b) (just b-v)
        
        ; We do a tail call if we can.
        #/mat assoc (list) (compare-by-cline cline-v a-v b-v)
        
        #/w- result (compare-by-cline cline-v a-v b-v)
        #/expect result (just #/ordering-eq)
          (mat result (nothing) (nothing)
          ; We have a potential result to use, but first we check that
          ; the rest of the field values belong to their respective
          ; clines' domains. If they don't, this structure instance is
          ; not part part of this cline's domain, so the result is
          ; `(nothing)`.
          #/w-loop next assoc assoc
            (expect assoc (cons entry assoc) result
            #/dissect entry (list k cline-v)
            #/dissect (table-get k a) (just a-v)
            #/dissect (table-get k b) (just b-v)
            #/expect
              (and (in-cline? cline-v a-v) (in-cline? cline-v b-v))
              #t
              (nothing)
            #/next assoc))
        #/next assoc)))
  ])

(define/contract (cline-table-ordered assoc)
  (-> (listof #/list/c name? cline?) cline?)
  (expect (assocs->table-if-mutually-unique assoc) (just _)
    (raise-arguments-error 'cline-table-ordered
      "expected the keys to be mutually unique"
      "assoc" assoc)
  #/internal:cline #/cline-internals-table-ordered assoc))

(struct-easy
  (furge-internals-table
    autoname-furge dex-furge getfx-call-furge furge-val)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:furge-table)
    
    (define (furge-internals-autoname this)
      (dissect this
        (furge-internals-table autoname-furge _ _ furge-val)
      #/list 'tag:furge-table #/autoname-furge furge-val))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-table _ dex-furge _ a)
      #/dissect other (furge-internals-table _ _ _ b)
      #/compare-by-dex dex-furge a b))
    
    (define (getfx-furge-internals-call this a b)
      (dissect this
        (furge-internals-table _ _ getfx-call-furge furge-val)
      #/expect a (internal:table a) (getfx-done #/nothing)
      #/expect b (internal:table b) (getfx-done #/nothing)
      ; NOTE: We run `getfx-call-furge` on all the entries to detect
      ; if any of them is outside the furge's domain.
      #/getmaybefx-bind
        (getmaybefx-table-each #/internal:table #/hash-v-map a #/fn v
          (getfx-call-furge furge-val v v))
      #/dissectfn (internal:table a)
      #/getmaybefx-bind
        (getmaybefx-table-each #/internal:table #/hash-v-map b #/fn v
          (getfx-call-furge furge-val v v))
      #/dissectfn (internal:table b)
      #/getmaybefx-table-each #/internal:table
        (hash-union a b #:combine #/fn a b
          (getfx-call-furge furge-val a b))))
  ])

(define/contract (merge-table merge-val)
  (-> merge? merge?)
  (internal:merge #/furge-internals-table
    autoname-merge (dex-merge) getfx-call-merge merge-val))

(define/contract (fuse-table fuse-val)
  (-> fuse? fuse?)
  (internal:fuse #/furge-internals-table
    autoname-fuse (dex-fuse) getfx-call-fuse fuse-val))



; ===== Fusable functions ============================================


(define/contract (fusable-function? v)
  (-> any/c boolean?)
  (internal:fusable-function? v))

(define/contract (make-fusable-function proc)
  (-> (-> any/c getfx?) fusable-function?)
  (if (fusable-function? proc) proc
  #/internal:fusable-function proc))



(struct-easy
  (fuse-fusable-function::getfx-err-cannot-combine-results
    method a b a-result b-result))
(struct-easy
  (fuse-fusable-function::getfx-arg-to-method arg))

(define/contract fuse-fusable-function-delegate/c
  contract?
  (case->
    (->
      (match/c
        fuse-fusable-function::getfx-err-cannot-combine-results
        fuse? any/c any/c any/c any/c)
      (getfx/c none/c))
    (-> (match/c fuse-fusable-function::getfx-arg-to-method any/c)
      (getfx/c fuse?))))

(define/contract
  (getfx-err-furge-internals-fusable-function-delegate-cannot-combine-results
    dexed-delegate method a b a-result b-result)
  (->
    (dexed-first-order/c fuse-fusable-function-delegate/c)
    fuse?
    any/c
    any/c
    any/c
    any/c
    (getfx/c none/c))
  (w- delegate (dexed-get-value dexed-delegate)
  #/w- getfx-delegate-result
    (delegate #/fuse-fusable-function::getfx-err-cannot-combine-results
      method a b a-result b-result)
  #/expect (getfx? getfx-delegate-result) #t
    (getfx-err-unraise #/raise-arguments-error
      'fuse-fusable-function-thorough
      "expected the pure result of dexed-delegate for fuse-fusable-function::getfx-err-cannot-combine-results to be a getfx effectful computation"
      "dexed-delegate" dexed-delegate
      "method" method
      "a" a
      "b" b
      "a-result" a-result
      "b-result" b-result
      "getfx-delegate-result" getfx-delegate-result)
  #/getfx-bind getfx-delegate-result #/fn delegate-result
  #/getfx-err-unraise #/raise-arguments-error 'fuse-fusable-function-thorough
    "expected dexed-delegate not to have a result for fuse-fusable-function::getfx-err-cannot-combine-results"
    "dexed-delegate" dexed-delegate
    "method" method
    "a" a
    "b" b
    "a-result" a-result
    "b-result" b-result
    "delegate-result" delegate-result))

(define/contract
  (getfx-furge-internals-fusable-function-delegate-arg-to-method
    dexed-delegate arg)
  (-> (dexed-first-order/c fuse-fusable-function-delegate/c) any/c
    (getfx/c fuse?))
  (w- delegate (dexed-get-value dexed-delegate)
  #/w- getfx-result
    (delegate #/fuse-fusable-function::getfx-arg-to-method arg)
  #/expect (getfx? getfx-result) #t
    (getfx-err-unraise #/raise-arguments-error
      'fuse-fusable-function-thorough
      "expected the pure result of dexed-delegate for fuse-fusable-function::getfx-arg-to-method to be a getfx effectful computation"
      "dexed-delegate" dexed-delegate
      "arg" arg
      "getfx-result" getfx-result)
  #/getfx-bind getfx-result #/fn result
  #/expect (fuse? result) #t
    (getfx-err-unraise #/raise-arguments-error
      'fuse-fusable-function-thorough
      "expected the result of dexed-delegate for fuse-fusable-function::getfx-arg-to-method to be a fuse"
      "dexed-delegate" dexed-delegate
      "arg" arg
      "result" result)
  #/getfx-done result))

(struct-easy (furge-internals-fusable-function dexed-delegate)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-fusable-function)
    
    (define (furge-internals-autoname this)
      (dissect this (furge-internals-fusable-function dexed-delegate)
      #/dissect (dexed-get-name dexed-delegate) (internal:name rep)
      #/list 'tag:furge-fusable-function rep))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-fusable-function a)
      #/dissect other (furge-internals-fusable-function b)
      #/compare-by-dex (dex-dexed) a b))
    
    (define (getfx-furge-internals-call this a b)
      (dissect this (furge-internals-fusable-function dexed-delegate)
      #/getfx-done
        (expect a (internal:fusable-function a) (nothing)
        #/expect b (internal:fusable-function b) (nothing)
        #/just #/internal:fusable-function #/fn arg
          (getfx-bind
            (getfx-furge-internals-fusable-function-delegate-arg-to-method
              dexed-delegate arg)
          #/fn method
          #/getfx-bind (a arg) #/fn a-result
          #/getfx-bind (b arg) #/fn b-result
          #/getfx-bind (getfx-call-fuse method a-result b-result)
          #/fn maybe-result
          #/expect maybe-result (just result)
            (getfx-err-furge-internals-fusable-function-delegate-cannot-combine-results
              dexed-delegate method a b a-result b-result)
          #/getfx-done result))))
  ])

(define/contract (fuse-fusable-function-thorough dexed-delegate)
  (-> (dexed-first-order/c fuse-fusable-function-delegate/c) fuse?)
  (internal:fuse #/furge-internals-fusable-function dexed-delegate))

(struct-easy
  (fuse-fusable-function-unthorough dexed-getfx-arg-to-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this
      (fuse-fusable-function-unthorough dexed-getfx-arg-to-method)
    #/mat command
      (fuse-fusable-function::getfx-err-cannot-combine-results
        method a b a-result b-result)
      (getfx-err-unraise #/raise-arguments-error
        'fuse-fusable-function
        "could not combine the result values"
        "dexed-getfx-arg-to-method" dexed-getfx-arg-to-method
        "method" method
        "a" a
        "b" b
        "a-result" a-result
        "b-result" b-result)
    #/dissect command (fuse-fusable-function::getfx-arg-to-method arg)
    #/w- getfx-arg-to-method
      (dexed-get-value dexed-getfx-arg-to-method)
    #/w- getfx-result (getfx-arg-to-method arg)
    #/expect (getfx? getfx-result) #t
      (getfx-err-unraise #/raise-arguments-error
        'fuse-fusable-function
        "expected the pure result of dexed-getfx-arg-to-method to be a getfx effectful computation"
        "dexed-getfx-arg-to-method" dexed-getfx-arg-to-method
        "arg" arg
        "getfx-result" getfx-result)
    #/getfx-bind getfx-result #/fn result
    #/expect (fuse? result) #t
      (getfx-err-unraise #/raise-arguments-error
        'fuse-fusable-function
        "expected the result of dexed-getfx-arg-to-method to be a fuse"
        "dexed-getfx-arg-to-method" dexed-getfx-arg-to-method
        "arg" arg
        "result" result)
    #/getfx-done result)))

(define/contract (fuse-fusable-function dexed-arg-to-method)
  (-> (dexed-first-order/c #/-> any/c #/getfx/c fuse?) fuse?)
  (fuse-fusable-function-thorough #/just-value #/dexed-of
    (dex-struct fuse-fusable-function-unthorough #/dex-dexed)
    (fuse-fusable-function-unthorough dexed-arg-to-method)))



; ===== Boolean clines and contracts =================================
;
; NOTE: These are meant to be part of `table/order`, except for
; `table-v-of`, a contract combinator which will be handy for the
; contracts of operations in this module. (TODO: Actually use it for
; that.)

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
        
        (define (dex-internals-dexed-of this x)
          (dex-internals-simple-dexed-of this x))
        
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


; TODO: Come up with a better name for this, and export it from
; `effection/order`.
(define/contract
  (pure-table-kv-map-fuse-default table fuse-0 fuse-2 kv-to-operand)
  (-> table? any/c fuse? (-> name? any/c any/c) any/c)
  (mat
    (pure-run-getfx #/getfx-table-map-fuse table fuse-2 #/fn k
      (dissect (table-get k table) (just v)
      #/getfx-done #/kv-to-operand k v))
    (just result)
    result
    fuse-0))

(define/contract (table-kv-map table kv-to-v)
  (-> table? (-> name? any/c any/c) table?)
  (pure-table-kv-map-fuse-default table (table-empty)
    (fuse-by-merge #/merge-table #/merge-by-dex #/dex-give-up)
  #/fn k v
    (table-shadow k (just #/kv-to-v k v) #/table-empty)))

(define/contract (table-kv-all? table kv-accepted?)
  (-> table? (-> name? any/c boolean?) table?)
  (pure-table-kv-map-fuse-default table #t
    (fuse-by-merge #/merge-boolean-by-and)
    kv-accepted?))

(define/contract (table-kv-any? table kv-accepted?)
  (-> table? (-> name? any/c boolean?) table?)
  (pure-table-kv-map-fuse-default table #f
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
