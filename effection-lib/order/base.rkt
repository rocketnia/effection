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
  contract-out hash/c list/c listof none/c)
(require #/only-in racket/contract/combinator
  blame-add-context coerce-contract contract-first-order-passes?
  raise-blame-error)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/hash hash-union)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts
  dissect dissectfn expect fn mat w- w-loop)
(require #/only-in lathe-comforts/hash
  hash-ref-maybe hash-set-maybe hash-v-all hash-v-any hash-v-map)
(require #/only-in lathe-comforts/list
  list-all list-any list-bind list-map)
(require #/only-in lathe-comforts/match match/c)
(require #/only-in lathe-comforts/maybe
  just maybe? maybe-bind maybe/c nothing nothing?)
(require #/only-in lathe-comforts/struct
  auto-write define-imitation-simple-struct struct-easy)

(require #/only-in effection/order/private
  dex-result? lt-autodex names-autodex
  make-appropriate-non-chaperone-contract make-ordering-private-gt
  make-ordering-private-lt name? object-identities-autodex
  ordering-private?
  
  ordering-eq ordering-eq?
  
  ordering-gt ordering-gt?
  
  ordering-lt ordering-lt?
  )
(require #/prefix-in internal: #/only-in
  effection/order/private/unsafe
  
  cline cline? cline-internals-autodex cline-internals-autoname
  cline-internals-compare cline-internals-dex cline-internals-in?
  cline-internals-tag dex dex? dex-internals-autodex
  dex-internals-autoname dex-internals-compare dex-internals-in?
  dex-internals-tag dex-internals-name-of furge-internals-autodex
  furge-internals-autoname furge-internals-call furge-internals-tag
  fusable-function fusable-function? fuse fuse? gen:cline-internals
  gen:dex-internals gen:furge-internals merge merge? name
  ordering-private table table?)


; ==== Orderings ====

(provide ordering-lt ordering-eq ordering-gt)
(provide #/contract-out
  [ordering-lt? (-> any/c boolean?)]
  [ordering-eq? (-> any/c boolean?)]
  [ordering-gt? (-> any/c boolean?)])
(provide ordering-private?)
(provide dex-result? cline-result?)
(provide make-ordering-private-lt make-ordering-private-gt)


; ==== Names, dexes, and dexables ====

(provide name?)

(provide dex?)
(module+ private/unsafe #/provide
  autoname-dex)
(provide in-dex? name-of compare-by-dex)

(provide dexable)
(provide #/contract-out
  [dexable? (-> any/c boolean?)]
  [dexable-dex (-> dexable? any/c)]
  [dexable-value (-> dexable? any/c)])
(provide valid-dexable? dexableof)
(provide compare-dexables name-of-dexable)
(provide dex-name dex-dex)

(provide
  dex-give-up
  dex-default
  dex-opaque
  dex-by-own-method
  dex-fix
  dex-struct-by-field-position
  dex-struct)

(module+ private/unsafe #/provide
  dexableof-unchecked)
(module+ private/unsafe #/provide
  (struct-out dex-by-own-method::raise-different-methods-error)
  (struct-out dex-by-own-method::get-method)
  dex-by-own-method-delegate/c
  dex-by-own-method-thorough-unchecked
  dex-by-own-method-thorough
  dex-by-own-method-unchecked
  dex-fix-unchecked)


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
  cline-by-own-method-thorough-unchecked
  cline-by-own-method-thorough
  cline-by-own-method-unchecked
  cline-fix-unchecked)


; ==== Merges and fuses ====

(provide
  merge?
  fuse?)
(module+ private/unsafe #/provide
  autoname-merge
  autoname-fuse)
(provide
  call-merge
  call-fuse)
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
  (struct-out merge-by-own-method::raise-different-input-methods-error)
  (struct-out merge-by-own-method::raise-cannot-get-output-method-error)
  (struct-out merge-by-own-method::raise-different-output-method-error)
  (struct-out merge-by-own-method::get-method)
  merge-by-own-method-delegate/c
  merge-by-own-method-thorough-unchecked
  merge-by-own-method-thorough
  merge-by-own-method-unchecked
  merge-fix-unchecked)
(provide
  fuse-opaque
  fuse-by-own-method
  fuse-fix
  fuse-struct-by-field-position
  fuse-struct)
(module+ private/unsafe #/provide
  (struct-out fuse-by-own-method::raise-different-input-methods-error)
  (struct-out fuse-by-own-method::raise-cannot-get-output-method-error)
  (struct-out fuse-by-own-method::raise-different-output-method-error)
  (struct-out fuse-by-own-method::get-method)
  fuse-by-own-method-delegate/c
  fuse-by-own-method-thorough-unchecked
  fuse-by-own-method-thorough
  fuse-by-own-method-unchecked
  fuse-fix-unchecked)


; ==== Tables ====

(provide
  table? table-get table-empty table-shadow table-map-fuse table-sort)
(module+ private/order #/provide
  assocs->table-if-mutually-unique)
(module+ private/unsafe #/provide
  table->sorted-list)
(provide dex-table merge-table fuse-table)


; ==== Fusable functions ====

(provide
  fusable-function? make-fusable-function fuse-fusable-function)
(module+ private/unsafe #/provide
  (struct-out fuse-fusable-function::raise-cannot-combine-results-error)
  (struct-out fuse-fusable-function::arg-to-method)
  fuse-fusable-function-delegate/c
  fuse-fusable-function-thorough-unchecked
  fuse-fusable-function-thorough
  fuse-fusable-function-unchecked)



; ===== Miscellaneous utilities ======================================

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

(define/contract (cline-result? x)
  (-> any/c boolean?)
  (or (dex-result? x) (ordering-lt? x) (ordering-gt? x)))

(define-simple-macro (maybe-ordering-or first:expr second:expr)
  (w- result first
  #/expect result (just #/ordering-eq) result
    second))

(define (maybe-compare-aligned-lists as bs maybe-compare-elems)
  (expect (list as bs) (list (cons a as) (cons b bs))
    (just #/ordering-eq)
  #/maybe-ordering-or (maybe-compare-elems a b)
  #/maybe-compare-aligned-lists as bs maybe-compare-elems))


; ===== Names, dexes, and dexables ===================================

(define/contract (dex? x)
  (-> any/c boolean?)
  (internal:dex? x))

(define/contract (autoname-dex x)
  (-> dex? any)
  (dissect x (internal:dex internals)
  #/cons 'name:dex #/internal:dex-internals-autoname internals))

(define/contract (in-dex? dex x)
  (-> dex? any/c boolean?)
  (dissect dex (internal:dex internals)
  #/internal:dex-internals-in? internals x))

(define/contract (name-of dex x)
  (-> dex? any/c #/maybe/c name?)
  (dissect dex (internal:dex internals)
  #/internal:dex-internals-name-of internals x))

(define/contract (compare-by-dex dex a b)
  (-> dex? any/c any/c #/maybe/c dex-result?)
  (dissect dex (internal:dex internals)
  #/internal:dex-internals-compare internals a b))


(define-imitation-simple-struct (dexable? dexable-dex dexable-value)
  dexable 'dexable (current-inspector) (auto-write))

(define/contract (valid-dexable? x)
  (-> any/c boolean?)
  (expect x (dexable dex value) #f
  #/and (dex? dex) (in-dex? dex value)))

(define/contract (dexableof-unchecked c)
  (-> contract? contract?)
  (w- c (coerce-contract 'dexableof-unchecked c)
  #/ (make-appropriate-non-chaperone-contract c)
    
    #:name `(dexableof-unchecked ,(contract-name c))
    
    #:first-order
    (fn v
      (contract-first-order-passes? (match/c dexable dex? c) v))
    
    #:late-neg-projection
    (fn blame
      (w- c-late-neg-projection
        ( (get/build-late-neg-projection c)
          (blame-add-context blame "the value of"))
      #/fn v missing-party
        (expect v (dexable dex x)
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a dexable" given: "~e")
            v)
        #/dexable dex #/c-late-neg-projection x missing-party)))))

(define/contract (dexableof c)
  (-> contract? contract?)
  (w- c (coerce-contract 'dexableof c)
  #/ (make-appropriate-non-chaperone-contract c)
    
    #:name `(dexableof ,(contract-name c))
    
    #:first-order
    (fn v
      (contract-first-order-passes?
        (and/c valid-dexable? #/match/c dexable dex? c)
        v))
    
    #:late-neg-projection
    (fn blame
      (w- c-late-neg-projection
        ( (get/build-late-neg-projection c)
          (blame-add-context blame "the value of"))
      #/fn v missing-party
        (expect (valid-dexable? v) #t
          (raise-blame-error blame #:missing-party missing-party v
            '(expected: "a valid dexable" given: "~e")
            v)
        #/dissect v (dexable dex x)
        #/dexable dex #/c-late-neg-projection x missing-party)))))

(define/contract (compare-dexables a b)
  (-> valid-dexable? valid-dexable? #/maybe/c dex-result?)
  (dissect a (dexable a-dex a)
  #/dissect b (dexable b-dex b)
  #/expect (compare-by-dex (dex-dex) a-dex b-dex) (just #/ordering-eq)
    (nothing)
  #/compare-by-dex a-dex a b))

(define/contract (name-of-dexable x)
  (-> valid-dexable? name?)
  (dissect x (dexable dex x)
  #/dissect (name-of dex x) (just result)
    result))



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
    
    (define (dex-internals-compare this a b)
      (if (and (name? a) (name? b))
        (just #/names-autodex a b)
        (nothing)))
  ])

(define/contract (dex-name) (-> dex?)
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
    
    (define (dex-internals-compare this a b)
      (expect a (internal:dex a) (nothing)
      #/expect b (internal:dex b) (nothing)
      #/w- tag internal:dex-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:dex-internals-autodex a b))
  ])

(define/contract (dex-dex) (-> dex?)
  (internal:dex #/dex-internals-dex))


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
    
    (define (dex-internals-compare this a b)
      (nothing))
  ])

(define/contract (dex-give-up) (-> dex?)
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
    cmp-internals-by-own-method
    cmp-by-own-method-thorough-unchecked
    cmp-by-own-method-thorough
    cmp-by-own-method-unthorough
    cmp-by-own-method-unchecked
    cmp-by-own-method)
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
    
    ; NOTE: If we weren't using this macro, we'd write the
    ; (struct-easy (cmp-internals-by-own-method ...) ...) declaration
    ; here. However, this definition substantially differs between the
    ; dex and the cline definitions, and the mutual recursion works
    ; out fine this way in Racket, so we leave it out of this and
    ; define `dex-internals-by-own-method` and
    ; `cline-internals-by-own-method` separately from this macro call.
    
    (define/contract
      (cmp-by-own-method-thorough-unchecked dexable-delegate)
      (-> (dexableof-unchecked cmp-by-own-method-delegate/c) cmp?)
      (internal:cmp #/cmp-internals-by-own-method dexable-delegate))
    
    (define/contract (cmp-by-own-method-thorough dexable-delegate)
      (-> (dexableof cmp-by-own-method-delegate/c) cmp?)
      (cmp-by-own-method-thorough-unchecked dexable-delegate))
    
    (struct-easy (cmp-by-own-method-unthorough get-method)
      #:other
      
      #:property prop:procedure
      (fn this command
        (dissect this (cmp-by-own-method-unthorough get-method)
        #/mat command
          (cmp-by-own-method::raise-different-methods-error
            a b a-method b-method)
          (raise-arguments-error 'cmp-by-own-method
            "obtained two different methods from the two values being compared"
            "get-method" get-method
            "a" a
            "b" b
            "a-method" a-method
            "b-method" b-method)
        #/dissect command (cmp-by-own-method::get-method source)
        #/get-method source)))
    
    (define/contract (cmp-by-own-method-unchecked dexable-get-method)
      (-> (dexableof-unchecked #/-> any/c #/maybe/c cmp?) cmp?)
      (dissect dexable-get-method (dexable dex get-method)
      #/cmp-by-own-method-thorough-unchecked #/dexable
        (dex-struct cmp-by-own-method-unthorough dex)
        (cmp-by-own-method-unthorough get-method)))
    
    (define/contract (cmp-by-own-method dexable-get-method)
      (-> (dexableof #/-> any/c #/maybe/c cmp?) cmp?)
      (cmp-by-own-method-unchecked dexable-get-method))
  ))

(define-cmp-by-own-method
  internal:dex
  dex?
  dex-by-own-method::raise-different-methods-error
  dex-by-own-method::get-method
  dex-by-own-method-delegate/c
  dex-internals-by-own-method
  dex-by-own-method-thorough-unchecked
  dex-by-own-method-thorough
  dex-by-own-method-unthorough
  dex-by-own-method-unchecked
  dex-by-own-method)

(struct-easy (dex-internals-by-own-method dexable-delegate)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-by-own-method)
    
    (define (dex-internals-autoname this)
      (dissect this
        (dex-internals-by-own-method #/dexable dex delegate)
      #/list 'tag:dex-by-own-method
        (autoname-dex dex)
        (name-of dex delegate)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-by-own-method a)
      #/dissect other (dex-internals-by-own-method b)
      #/compare-dexables a b))
    
    (define (dex-internals-in? this x)
      (dissect this
        (dex-internals-by-own-method #/dexable dex delegate)
      #/expect (delegate #/dex-by-own-method::get-method x)
        (just method)
        #f
      #/in-dex? method x))
    
    (define (dex-internals-name-of this x)
      (dissect this
        (dex-internals-by-own-method #/dexable dex delegate)
      #/maybe-bind (delegate #/dex-by-own-method::get-method x)
      #/fn method
      #/name-of method x))
    
    (define (dex-internals-compare this a b)
      (dissect this
        (dex-internals-by-own-method #/dexable dex delegate)
      #/maybe-bind (delegate #/dex-by-own-method::get-method a)
      #/fn a-method
      #/maybe-bind (delegate #/dex-by-own-method::get-method b)
      #/fn b-method
      #/expect (compare-by-dex (dex-dex) a-method b-method)
        (just #/ordering-eq)
        (delegate #/dex-by-own-method::raise-different-methods-error
          a b a-method b-method)
      #/compare-by-dex a-method a b))
  ])


(struct-easy (dex-internals-fix dexable-unwrap)
  #:other
  
  #:methods internal:gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'tag:dex-fix)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/list 'tag:dex-fix (autoname-dex dex) (name-of dex unwrap)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-fix a)
      #/dissect other (dex-internals-fix b)
      #/compare-dexables a b))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/in-dex? (unwrap #/internal:dex this) x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/name-of (unwrap #/internal:dex this) x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/compare-by-dex (unwrap #/internal:dex this) a b))
  ])

(define/contract (dex-fix-unchecked dexable-unwrap)
  (-> (dexableof-unchecked #/-> dex? dex?) dex?)
  (internal:dex #/dex-internals-fix dexable-unwrap))

(define/contract (dex-fix dexable-unwrap)
  (-> (dexableof #/-> dex? dex?) dex?)
  (internal:dex #/dex-internals-fix dexable-unwrap))


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
      #/maybe-compare-aligned-lists a-fields b-fields
      #/fn a-field b-field
        (dissect a-field (list a-getter a-position a-dex)
        #/dissect b-field (list b-getter b-position b-dex)
        #/maybe-ordering-or
          (just #/lt-autodex a-position b-position <)
        #/compare-by-dex (dex-dex) a-dex b-dex)))
    
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
      #/w-loop next fields fields rev-result (list)
        (expect fields (cons field fields)
          (just #/internal:name
          #/list* 'name:struct descriptor #/reverse rev-result)
        #/dissect field (list getter position dex)
        #/expect (name-of dex #/getter x) (just name) (nothing)
        #/dissect name (internal:name rep)
        #/next fields #/cons rep rev-result)))
    
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
    
    (define (dex-internals-compare this a b)
      (expect a (internal:cline a) (nothing)
      #/expect b (internal:cline b) (nothing)
      #/w- tag internal:cline-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:cline-internals-autodex a b))
  ])

(define/contract (dex-cline) (-> dex?)
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

(define/contract (cline-give-up) (-> cline?)
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
  cline-internals-by-own-method
  cline-by-own-method-thorough-unchecked
  cline-by-own-method-thorough
  cline-by-own-method-unthorough
  cline-by-own-method-unchecked
  cline-by-own-method)

(struct-easy (convert-dex-from-cline-by-own-method delegate)
  #:other
  
  #:property prop:procedure
  (fn this x
    (dissect this (convert-dex-from-cline-by-own-method delegate)
    #/maybe-bind (delegate #/cline-by-own-method::get-method x)
    #/fn method
    #/just #/get-dex-from-cline method)))

(struct-easy (cline-internals-by-own-method dexable-delegate)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-by-own-method)
    
    (define (cline-internals-autoname this)
      (dissect this
        (cline-internals-by-own-method #/dexable dex delegate)
      #/list 'tag:cline-by-own-method
        (autoname-dex dex)
        (name-of dex delegate)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-own-method a)
      #/dissect other (cline-internals-by-own-method b)
      #/compare-dexables a b))
    
    (define (cline-internals-dex this)
      (dissect this
        (cline-internals-by-own-method #/dexable dex delegate)
      #/dex-by-own-method #/dexable
        (dex-struct convert-dex-from-cline-by-own-method dex)
        (convert-dex-from-cline-by-own-method delegate)))
    
    (define (cline-internals-in? this x)
      (dissect this
        (cline-internals-by-own-method #/dexable dex delegate)
      #/expect (delegate #/cline-by-own-method::get-method x)
        (just method)
        #f
      #/in-cline? method x))
    
    (define (cline-internals-compare this a b)
      (dissect this
        (cline-internals-by-own-method #/dexable dex delegate)
      #/maybe-bind (delegate #/cline-by-own-method::get-method a)
      #/fn a-method
      #/maybe-bind (delegate #/cline-by-own-method::get-method b)
      #/fn b-method
      #/expect (compare-by-dex (dex-cline) a-method b-method)
        (just #/ordering-eq)
        (delegate #/cline-by-own-method::raise-different-methods-error
          a b a-method b-method)
      #/compare-by-cline a-method a b))
  ])


(struct-easy (convert-dex-from-cline-fix unwrap)
  #:other
  
  #:property prop:procedure
  (fn this dex
    (dissect this (convert-dex-from-cline-fix unwrap)
    #/get-dex-from-cline #/unwrap #/cline-by-dex dex)))

(struct-easy (cline-internals-fix dexable-unwrap)
  #:other
  
  #:methods internal:gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'tag:cline-fix)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/list 'tag:cline-fix (autoname-dex dex) (name-of dex unwrap)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-fix a)
      #/dissect other (cline-internals-fix b)
      #/compare-dexables a b))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/dex-fix #/dexable
        (dex-struct convert-dex-from-cline-fix dex)
        (convert-dex-from-cline-fix unwrap)))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/in-cline? (unwrap #/internal:cline this) x))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/compare-by-cline (unwrap #/internal:cline this) a b))
  ])

(define/contract (cline-fix-unchecked dexable-unwrap)
  (-> (dexableof-unchecked #/-> cline? cline?) cline?)
  (internal:cline #/cline-internals-fix dexable-unwrap))

(define/contract (cline-fix dexable-unwrap)
  (-> (dexableof #/-> cline? cline?) cline?)
  (internal:cline #/cline-internals-fix dexable-unwrap))


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
      #/maybe-compare-aligned-lists a-fields b-fields
      #/fn a-field b-field
        (dissect a-field (list a-getter a-position a-cline)
        #/dissect b-field (list b-getter b-position b-cline)
        #/maybe-ordering-or
          (just #/lt-autodex a-position b-position <)
        #/compare-by-dex (dex-cline) a-cline b-cline)))
    
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

(define/contract (call-merge merge a b)
  (-> merge? any/c any/c maybe?)
  (dissect merge (internal:merge internals)
  #/internal:furge-internals-call internals a b))

(define/contract (fuse? x)
  (-> any/c boolean?)
  (internal:fuse? x))

(define/contract (autoname-fuse x)
  (-> fuse? any)
  (dissect x (internal:fuse internals)
  #/cons 'name:fuse #/internal:furge-internals-autoname internals))

(define/contract (call-fuse fuse a b)
  (-> fuse? any/c any/c maybe?)
  (dissect fuse (internal:fuse internals)
  #/internal:furge-internals-call internals a b))


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
    
    (define (dex-internals-compare this a b)
      (expect a (internal:merge a) (nothing)
      #/expect b (internal:merge b) (nothing)
      #/w- tag internal:furge-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:furge-internals-autodex a b))
  ])

(define/contract (dex-merge) (-> dex?)
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
    
    (define (dex-internals-compare this a b)
      (expect a (internal:fuse a) (nothing)
      #/expect b (internal:fuse b) (nothing)
      #/w- tag internal:furge-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/internal:furge-internals-autodex a b))
  ])

(define/contract (dex-fuse) (-> dex?)
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
    
    (define (furge-internals-call this a b)
      (dissect this (fuse-internals-by-merge merge)
      #/call-merge merge a b))
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
    
    (define (furge-internals-call this a b)
      (dissect this (furge-internals-by-dex dex)
      #/mat (compare-by-dex dex a b) (just #/ordering-eq)
        (just a)
        (nothing)))
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
    
    (define (furge-internals-call this a b)
      (dissect this (furge-internals-by-cline-min cline)
      #/expect (compare-by-cline cline a b) (just cline-result)
        (nothing)
      #/just
      #/mat cline-result (ordering-gt) b
        a))
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
    call-furge
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
        
        (define (furge-internals-call this a b)
          (dissect this (furge-internals-opaque name furge)
          #/call-furge furge a b))
      ])
    
    (define/contract (furge-opaque name furge)
      (-> name? furge? furge?)
      (internal:furge #/furge-internals-opaque name furge))
  ))

(define-furge-opaque
  internal:merge
  merge?
  autoname-merge
  call-merge
  dex-merge
  merge-internals-opaque
  tag:merge-opaque
  merge-opaque)

(define-furge-opaque
  internal:fuse
  fuse?
  autoname-fuse
  call-fuse
  dex-fuse
  fuse-internals-opaque
  tag:fuse-opaque
  fuse-opaque)


(define-syntax-rule
  (define-furge-by-own-method
    internal:furge
    furge?
    call-furge
    dex-furge
    furge-by-own-method::raise-different-input-methods-error
    furge-by-own-method::raise-cannot-get-output-method-error
    furge-by-own-method::raise-different-output-method-error
    furge-by-own-method::get-method
    furge-by-own-method-delegate/c
    furge-internals-by-own-method
    tag:furge-by-own-method
    furge-by-own-method-thorough-unchecked
    furge-by-own-method-thorough
    furge-by-own-method-unthorough
    furge-by-own-method-unchecked
    furge-by-own-method)
  (begin
    
    (struct-easy
      (furge-by-own-method::raise-different-input-methods-error
        a b a-method b-method))
    (struct-easy
      (furge-by-own-method::raise-cannot-get-output-method-error
        a b result input-method))
    (struct-easy
      (furge-by-own-method::raise-different-output-method-error
        a b result input-method output-method))
    (struct-easy
      (furge-by-own-method::get-method source))
    
    (define/contract furge-by-own-method-delegate/c
      contract?
      (case->
        (->
          (match/c
            furge-by-own-method::raise-different-input-methods-error
            any/c any/c furge? furge?)
          none/c)
        (->
          (match/c
            furge-by-own-method::raise-cannot-get-output-method-error
            any/c any/c any/c furge?)
          none/c)
        (->
          (match/c
            furge-by-own-method::raise-different-output-method-error
            any/c any/c any/c furge? furge?)
          none/c)
        (->
          (match/c furge-by-own-method::get-method any/c)
          (maybe/c furge?))))
    
    (struct-easy (furge-internals-by-own-method dexable-delegate)
      #:other
      
      #:methods internal:gen:furge-internals
      [
        
        (define (furge-internals-tag this)
          'tag:furge-by-own-method)
        
        (define (furge-internals-autoname this)
          (dissect this
            (furge-internals-by-own-method #/dexable dex delegate)
          #/list 'tag:furge-by-own-method
            (autoname-dex dex)
            (name-of dex delegate)))
        
        (define (furge-internals-autodex this other)
          (dissect this (furge-internals-by-own-method a)
          #/dissect other (furge-internals-by-own-method b)
          #/compare-dexables a b))
        
        (define (furge-internals-call this a b)
          (dissect this
            (furge-internals-by-own-method #/dexable dex delegate)
          #/maybe-bind (delegate #/furge-by-own-method::get-method a)
          #/fn a-method
          #/maybe-bind (delegate #/furge-by-own-method::get-method b)
          #/fn b-method
          #/expect (compare-by-dex (dex-furge) a-method b-method)
            (just #/ordering-eq)
            (delegate
            #/furge-by-own-method::raise-different-input-methods-error
              a b a-method b-method)
          #/expect (call-furge a-method a b) (just result) (nothing)
          #/expect
            (delegate #/furge-by-own-method::get-method result)
            (just result-method)
            (delegate
            #/furge-by-own-method::raise-cannot-get-output-method-error
              a b result a-method)
          #/expect (compare-by-dex (dex-furge) a-method result-method)
            (just #/ordering-eq)
            (delegate
            #/furge-by-own-method::raise-different-output-method-error
              a b result a-method result-method)
          #/just result))
      ])
    
    (define/contract
      (furge-by-own-method-thorough-unchecked dexable-delegate)
      (-> (dexableof-unchecked furge-by-own-method-delegate/c) furge?)
      (internal:furge
      #/furge-internals-by-own-method dexable-delegate))
    
    (define/contract (furge-by-own-method-thorough dexable-delegate)
      (-> (dexableof furge-by-own-method-delegate/c) furge?)
      (furge-by-own-method-thorough-unchecked dexable-delegate))
    
    (struct-easy (furge-by-own-method-unthorough get-method)
      #:other
      
      #:property prop:procedure
      (fn this command
        (dissect this (furge-by-own-method-unthorough get-method)
        #/mat command
          (furge-by-own-method::raise-different-input-methods-error
            a b a-method b-method)
          (raise-arguments-error 'furge-by-own-method
            "obtained two different methods from the two input values"
            "get-method" get-method
            "a" a
            "b" b
            "a-method" a-method
            "b-method" b-method)
        #/mat command
          (furge-by-own-method::raise-cannot-get-output-method-error
            a b result input-method)
          (raise-arguments-error 'furge-by-own-method
            "could not obtain a method from the result value"
            "get-method" get-method
            "a" a
            "b" b
            "result" result
            "input-method" input-method)
        #/mat command
          (furge-by-own-method::raise-different-output-method-error
            a b result input-method output-method)
          (raise-arguments-error 'furge-by-own-method
            "obtained two different methods from the input and the output"
            "get-method" get-method
            "a" a
            "b" b
            "result" result
            "input-method" input-method
            "output-method" output-method)
        #/dissect command (furge-by-own-method::get-method source)
        #/get-method source)))
    
    (define/contract
      (furge-by-own-method-unchecked dexable-get-method)
      (-> (dexableof-unchecked #/-> any/c #/maybe/c furge?) furge?)
      (dissect dexable-get-method (dexable dex get-method)
      #/furge-by-own-method-thorough-unchecked #/dexable
        (dex-struct furge-by-own-method-unthorough dex)
        (furge-by-own-method-unthorough get-method)))
    
    (define/contract (furge-by-own-method dexable-get-method)
      (-> (dexableof #/-> any/c #/maybe/c furge?) furge?)
      (furge-by-own-method-unchecked dexable-get-method))
  ))

(define-furge-by-own-method
  internal:merge
  merge?
  call-merge
  dex-merge
  merge-by-own-method::raise-different-input-methods-error
  merge-by-own-method::raise-cannot-get-output-method-error
  merge-by-own-method::raise-different-output-method-error
  merge-by-own-method::get-method
  merge-by-own-method-delegate/c
  merge-internals-by-own-method
  tag:merge-by-own-method
  merge-by-own-method-thorough-unchecked
  merge-by-own-method-thorough
  merge-by-own-method-unthorough
  merge-by-own-method-unchecked
  merge-by-own-method)

(define-furge-by-own-method
  internal:fuse
  fuse?
  call-fuse
  dex-fuse
  fuse-by-own-method::raise-different-input-methods-error
  fuse-by-own-method::raise-cannot-get-output-method-error
  fuse-by-own-method::raise-different-output-method-error
  fuse-by-own-method::get-method
  fuse-by-own-method-delegate/c
  fuse-internals-by-own-method
  tag:fuse-by-own-method
  fuse-by-own-method-thorough-unchecked
  fuse-by-own-method-thorough
  fuse-by-own-method-unthorough
  fuse-by-own-method-unchecked
  fuse-by-own-method)


(struct-easy
  (furge-internals-fix call-furge furge-encapsulated dexable-unwrap)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:furge-fix)
    
    (define (furge-internals-autoname this)
      (dissect this (furge-internals-fix _ _ #/dexable dex unwrap)
      #/list 'tag:furge-fix (autoname-dex dex) (name-of dex unwrap)))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-fix _ _ a)
      #/dissect other (furge-internals-fix _ _ b)
      #/compare-dexables a b))
    
    (define (furge-internals-call this a b)
      (dissect this
        (furge-internals-fix call-furge furge-encapsulated
        #/dexable dex unwrap)
      #/call-furge (unwrap #/furge-encapsulated this) a b))
  ])

(define/contract (merge-fix-unchecked dexable-unwrap)
  (-> (dexableof-unchecked #/-> merge? merge?) merge?)
  (internal:merge
  #/furge-internals-fix call-merge internal:merge dexable-unwrap))

(define/contract (fuse-fix-unchecked dexable-unwrap)
  (-> (dexableof-unchecked #/-> fuse? fuse?) fuse?)
  (internal:fuse
  #/furge-internals-fix call-fuse internal:fuse dexable-unwrap))

(define/contract (merge-fix dexable-unwrap)
  (-> (dexableof #/-> merge? merge?) merge?)
  (internal:merge
  #/furge-internals-fix call-merge internal:merge dexable-unwrap))

(define/contract (fuse-fix dexable-unwrap)
  (-> (dexableof #/-> fuse? fuse?) fuse?)
  (internal:fuse
  #/furge-internals-fix call-fuse internal:fuse dexable-unwrap))


(struct-easy
  (furge-internals-struct
    autoname-furge dex-furge call-furge
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
      #/maybe-compare-aligned-lists a-fields b-fields
      #/fn a-field b-field
        (dissect a-field (list a-getter a-position a-furge)
        #/dissect b-field (list b-getter b-position b-furge)
        #/maybe-ordering-or
          (just #/lt-autodex a-position b-position <)
        #/compare-by-dex dex-furge a-furge b-furge)))
    
    (define (furge-internals-call this a b)
      (dissect this
        (furge-internals-struct
          _ _ call-furge descriptor constructor counts? fields)
      #/expect (counts? a) #t (nothing)
      #/expect (counts? b) #t (nothing)
      #/w- n (length fields)
      #/w-loop next fields fields args (hasheq)
        (expect fields (cons field fields)
          (just
          #/apply constructor #/build-list n #/fn i #/hash-ref args i)
        #/dissect field (list getter position furge)
        #/maybe-bind (call-furge furge (getter a) (getter b))
        #/fn furged
        #/next fields #/hash-set args position furged)))
  ])

(define-for-syntax
  (expand-furge-struct-by-field-position
    stx furges-message furge-encapsulated-id autoname-furge-id
    dex-furge-id call-furge-id)
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
        #,autoname-furge-id #,dex-furge-id #,call-furge-id
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
    #'internal:merge #'autoname-merge #'(dex-merge) #'call-merge))

(define-syntax (fuse-struct-by-field-position stx)
  (expand-furge-struct-by-field-position stx "fuses"
    #'internal:fuse #'autoname-fuse #'(dex-fuse) #'call-fuse))

(define-for-syntax
  (expand-furge-struct
    stx furges-message furge-encapsulated-id autoname-furge-id
    dex-furge-id call-furge-id)
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
        #,autoname-furge-id #,dex-furge-id #,call-furge-id
        #,struct:foo #,make-foo #,foo?
      #/list
        #,@(list-kv-map (map list fields getters) #/fn position field
             (dissect field (list furge getter)
               #`(list #,getter #,position #,furge))))))

(define-syntax (merge-struct stx)
  (expand-furge-struct stx "merges"
    #'internal:merge #'autoname-merge #'(dex-merge) #'call-merge))

(define-syntax (fuse-struct stx)
  (expand-furge-struct stx "fuses"
    #'internal:fuse #'autoname-fuse #'(dex-fuse) #'call-fuse))



; ===== Tables =======================================================

(define/contract (table? x)
  (-> any/c boolean?)
  (internal:table? x))

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

(define/contract (table-map-fuse table fuse key-to-operand)
  (-> table? fuse? (-> name? any/c) maybe?)
  (dissect table (internal:table hash)
  #/w- operands
    (list-map (hash-keys hash) #/fn key
      (key-to-operand #/internal:name key))
  
  ; NOTE: We do a first pass over the operands to make sure they're
  ; all in the fuse's domain because otherwise a client could detect
  ; that their sometimes-non-terminating `fuse-by-own-method` wasn't
  ; called on certain operands.
  #/if
    (list-any operands #/fn operand
      (nothing? #/call-fuse fuse operand operand))
    (nothing)
  
  #/expect operands (cons so-far operands) (nothing)
  #/w-loop next so-far so-far operands operands
    (expect operands (cons operand operands) (just so-far)
    #/maybe-bind (call-fuse fuse so-far operand) #/fn so-far
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

(define/contract (table->sorted-list tab)
  (-> table? #/listof #/list/c name? any/c)
  (dissect tab (internal:table hash)
  #/list-map
    (sort (hash->list hash) #/fn a b
      (dissect a (cons ak av)
      #/dissect b (cons bk bv)
      #/w- dex-result
        (names-autodex (internal:name ak) (internal:name bk))
      #/mat dex-result (internal:ordering-private #/ordering-lt) #t
        #f))
  #/dissectfn (cons k v)
    (list (internal:name k) v)))

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
      #/list-bind (table->sorted-list x)
      #/dissectfn (list k v)
        (dissect (name-of dex-val v) (just v)
        #/list k v)))
    
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
        (just #/lt-autodex (hash-count a) (hash-count b) <)
      #/w- a (table->sorted-list #/internal:table a)
      #/w- b (table->sorted-list #/internal:table b)
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

(define/contract (dex-table dex-val) (-> dex? dex?)
  (internal:dex #/dex-internals-table dex-val))

(struct-easy
  (furge-internals-table
    autoname-furge dex-furge call-furge furge-val)
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
    
    (define (furge-internals-call this a b)
      (dissect this (furge-internals-table _ _ call-furge furge-val)
      #/expect a (internal:table a) (nothing)
      #/expect b (internal:table b) (nothing)
      ; NOTE: We run `call-furge` on all the entries to detect if any
      ; of them is outside the furge's domain.
      #/w- a (hash-v-map a #/fn v #/call-furge furge-val v v)
      #/w- b (hash-v-map b #/fn v #/call-furge furge-val v v)
      #/if (hash-v-any a #/fn v #/nothing? v) (nothing)
      #/if (hash-v-any b #/fn v #/nothing? v) (nothing)
      #/w- furged
        (hash-union a b #:combine #/fn a b
          (maybe-bind a #/fn a
          #/maybe-bind b #/fn b
          #/call-furge furge-val a b))
      #/if (hash-v-any furged #/fn v #/nothing? v) (nothing)
      #/just #/internal:table
      #/hash-v-map furged #/dissectfn (just v) v))
  ])

(define/contract (merge-table merge-val)
  (-> merge? merge?)
  (internal:merge #/furge-internals-table
    autoname-merge (dex-merge) call-merge merge-val))

(define/contract (fuse-table fuse-val)
  (-> fuse? fuse?)
  (internal:fuse #/furge-internals-table
    autoname-fuse (dex-fuse) call-fuse fuse-val))



; ===== Fusable functions ============================================


(define/contract (fusable-function? v)
  (-> any/c boolean?)
  (internal:fusable-function? v))

(define/contract (make-fusable-function proc)
  (-> (-> any/c any/c) fusable-function?)
  (if (fusable-function? proc) proc
  #/internal:fusable-function proc))



(struct-easy
  (fuse-fusable-function::raise-cannot-combine-results-error
    method a b a-result b-result))
(struct-easy
  (fuse-fusable-function::arg-to-method arg))

(define/contract fuse-fusable-function-delegate/c
  contract?
  (case->
    (->
      (match/c
        fuse-fusable-function::raise-cannot-combine-results-error
        fuse? any/c any/c any/c any/c)
      none/c)
    (->
      (match/c fuse-fusable-function::arg-to-method any/c)
      (maybe/c fuse?))))

(struct-easy (furge-internals-fusable-function dexable-delegate)
  #:other
  
  #:methods internal:gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'tag:fuse-fusable-function)
    
    (define (furge-internals-autoname this)
      (dissect this
        (furge-internals-fusable-function #/dexable dex delegate)
      #/list 'tag:furge-fusable-function
        (autoname-dex dex)
        (name-of dex delegate)))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-fusable-function a)
      #/dissect other (furge-internals-fusable-function b)
      #/compare-dexables a b))
    
    (define (furge-internals-call this a b)
      (dissect this
        (furge-internals-fusable-function #/dexable dex delegate)
      #/expect (procedure? a) #t (nothing)
      #/expect (procedure? b) #t (nothing)
      #/fn arg
        (w- method
          (delegate #/fuse-fusable-function::arg-to-method arg)
        #/w- a-result (a arg)
        #/w- b-result (b arg)
        #/expect (call-fuse method a-result b-result) (just result)
          (delegate
          #/fuse-fusable-function::raise-cannot-combine-results-error
            method a b a-result b-result)
          result)))
  ])

(define/contract
  (fuse-fusable-function-thorough-unchecked dexable-delegate)
  (-> (dexableof-unchecked fuse-fusable-function-delegate/c) fuse?)
  (internal:fuse #/furge-internals-fusable-function dexable-delegate))

(define/contract (fuse-fusable-function-thorough dexable-delegate)
  (-> (dexableof fuse-fusable-function-delegate/c) fuse?)
  (fuse-fusable-function-thorough-unchecked dexable-delegate))

(struct-easy (fuse-fusable-function-unthorough arg-to-method)
  #:other
  
  #:property prop:procedure
  (fn this command
    (dissect this (fuse-fusable-function-unthorough arg-to-method)
    #/mat command
      (fuse-fusable-function::raise-cannot-combine-results-error
        method a b a-result b-result)
      (raise-arguments-error 'fuse-fusable-function
        "could not combine the result values"
        "arg-to-method" arg-to-method
        "method" method
        "a" a
        "b" b
        "a-result" a-result
        "b-result" b-result)
    #/dissect command (fuse-fusable-function::arg-to-method arg)
    #/arg-to-method arg)))

(define/contract
  (fuse-fusable-function-unchecked dexable-arg-to-method)
  (-> (dexableof-unchecked #/-> any/c fuse?) fuse?)
  (dissect dexable-arg-to-method (dexable dex arg-to-method)
  #/fuse-fusable-function-thorough-unchecked #/dexable
    (dex-struct fuse-fusable-function-unthorough dex)
    (fuse-fusable-function-unthorough arg-to-method)))

(define/contract (fuse-fusable-function dexable-arg-to-method)
  (-> (dexableof #/-> any/c fuse?) fuse?)
  (fuse-fusable-function-unchecked dexable-arg-to-method))
