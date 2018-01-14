#lang parendown racket/base


(require #/for-syntax racket/base)
(require #/for-syntax #/only-in racket/struct-info
  extract-struct-info struct-info?)
(require #/for-syntax #/only-in racket/contract/base -> any/c listof)
(require #/for-syntax #/only-in racket/contract/region
  define/contract)
(require #/for-syntax #/only-in syntax/parse expr id nat syntax-parse)

(require #/for-syntax #/only-in lathe
  dissect expect mat next nextlet w-)

(require #/for-syntax "../private/util.rkt")


(require #/only-in racket/contract/base
  -> and/c any any/c chaperone-contract? contract? contract-projection
  struct/c)
(require #/only-in racket/contract/combinator
  contract-first-order-passes? make-contract)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define/generic define-generics)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe dissect dissectfn expect mat next nextlet w-)

(require #/only-in effection/maybe/base just nothing maybe/c maybe?)

(require "../private/util.rkt")


; ==== Orderings ====

(provide #/struct-out ordering-lt)
(provide #/struct-out ordering-eq)
(provide #/struct-out ordering-gt)
(provide ordering-private? dex-result? cline-result?)
(provide make-ordering-private-lt make-ordering-private-gt)


; ==== Names, dexes, and dexables ====

(provide name?)

(provide dex?)
(provide in-dex? name-of compare-by-dex)

(provide #/struct-out dexable)
(provide valid-dexable? dexableof)
(provide compare-dexables name-of-dexable)
(provide dex-name dex-dex)

(provide
  dex-give-up
  dex-default
  dex-by-own-method
  dex-fix
  dex-struct-by-field-position
  dex-struct)


; ==== Clines ====

(provide cline?)
(provide get-dex-from-cline in-cline? compare-by-cline)
(provide dex-cline)

(provide
  cline-by-dex
  cline-give-up
  cline-default
  cline-by-own-method
  cline-fix
  cline-struct-by-field-position
  cline-struct)


; ==== Merges and fuses ====

(provide merge?)
(provide fuse?)
(provide call-merge)
(provide call-fuse)
(provide dex-merge)
(provide dex-fuse)

(provide fuse-by-merge)

(provide merge-by-dex)

(provide
  merge-by-own-method
  merge-fix
  merge-struct-by-field-position
  merge-struct)
(provide
  fuse-by-own-method
  fuse-fix
  fuse-struct-by-field-position
  fuse-struct)



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
    (nextlet syntax-list syntax-list
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
      (nextlet super super
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
  #/expect
    (list-all getters #/lambda (getter) #/not #/eq? #f getter)
    #t
    (raise-syntax-error #f
      "not a structure type with all of its getters available"
      stx id)
  #/expect (list-all setters #/lambda (setter) #/eq? #f setter) #t
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

(struct-easy "an ordering-lt" (ordering-lt) #:equal)
(struct-easy "an ordering-eq" (ordering-eq) #:equal)
(struct-easy "an ordering-gt" (ordering-gt) #:equal)

; NOTE: We used to expose this as two structs, namely
; `ordering-private-lt` and `ordering-private-gt`, but that approach
; had a problem: Using `struct->vector`, Racket code can detect which
; is which without even going to the trouble of writing the values to
; a text stream.
(struct-easy "an ordering-private-encapsulated"
  (ordering-private-encapsulated ordering))

(define/contract (ordering-private? x)
  (-> any/c boolean?)
  (ordering-private-encapsulated? x))

(define/contract (dex-result? x)
  (-> any/c boolean?)
  (or (ordering-private? x) (ordering-eq? x)))

(define/contract (cline-result? x)
  (-> any/c boolean?)
  (or (dex-result? x) (ordering-lt? x) (ordering-gt? x)))

; NOTE: We make these procedures because if we provided them as bare
; values, we would encourage people to write code that appeared to
; keep the ordering private but actually exposed it to a simple `eq?`
; check. Of course, Effection-unsafe Racket code can still compare
; these values by writing them to streams and observing the data
; that's written this way, but at least that's harder to do by
; accident.
(define/contract (make-ordering-private-lt)
  (-> ordering-private?)
  (ordering-private-encapsulated #/ordering-lt))
(define/contract (make-ordering-private-gt)
  (-> ordering-private?)
  (ordering-private-encapsulated #/ordering-gt))

(define-simple-macro (ordering-or first:expr second:expr)
  (w- result first
  #/expect result (ordering-eq) result
    second))

(define-simple-macro (maybe-ordering-or first:expr second:expr)
  (w- result first
  #/expect result (just #/ordering-eq) result
    second))

(define/contract (lt-autodex a b <?)
  (-> any/c any/c (-> any/c any/c boolean?) dex-result?)
  (if (<? a b) (make-ordering-private-lt)
  #/if (<? b a) (make-ordering-private-gt)
  #/ordering-eq))

(define/contract (lt-autocline a b <?)
  (-> any/c any/c (-> any/c any/c boolean?) dex-result?)
  (if (<? a b) (ordering-lt)
  #/if (<? b a) (ordering-gt)
  #/ordering-eq))

(define (maybe-compare-aligned-lists as bs maybe-compare-elems)
  (expect (list as bs) (list (cons a as) (cons b bs))
    (just #/ordering-eq)
  #/maybe-ordering-or (maybe-compare-elems a b)
  #/maybe-compare-aligned-lists as bs maybe-compare-elems))


; ===== Names, dexes, and dexables ===================================

; Internally, we represent name values as data made of structure type
; descriptors, exact nonnegative integers, symbols, empty lists, and
; cons cells, and for sorting purposes, we consider them to ascend in
; that order.
(struct-easy "a name" (name-internal rep))

(define/contract (name? x)
  (-> any/c boolean?)
  (name-internal? x))

; TODO: Test this implementation very thoroughly. We need to know what
; happens when this module is used through diamond dependencies, what
; happens if this module is somehow visited more than once, and that
; kind of thing. We probably won't be able to guarantee struct type
; descriptors will be sorted the same way in every phase, but we can
; at least document why not.
(define descriptors-semaphore* (make-semaphore 1))
(define descriptors-to-ranks* (make-weak-hasheq))
(define descriptors-next-rank* 0)
(define/contract (descriptor-rank descriptor)
  (-> struct-type? exact-nonnegative-integer?)
  (call-with-semaphore descriptors-semaphore* #/lambda ()
    (if (hash-has-key? descriptors-to-ranks* descriptor)
      (hash-ref descriptors-to-ranks* descriptor)
    #/w- rank descriptors-next-rank*
      (hash-set! descriptors-to-ranks* descriptor rank)
      (set! descriptors-next-rank* (add1 rank))
      rank)))
(define/contract (struct-type-descriptors-autodex a b)
  (-> struct-type? struct-type? dex-result?)
  (lt-autodex (descriptor-rank a) (descriptor-rank b) <))

(define/contract (names-autodex a b)
  (-> name? name? dex-result?)
  (dissect a (name-internal a)
  #/dissect b (name-internal b)
  #/nextlet a a b b
    
    ; Handle the cons cells.
    (mat a (cons a-first a-rest)
      (mat b (cons b-first b-rest)
        (ordering-or (next a-first b-first) (next a-rest b-rest))
      #/make-ordering-private-gt)
    #/mat b (cons b-first b-rest) (make-ordering-private-lt)
    
    ; Handle the empty lists.
    #/mat a (list)
      (mat b (list) (ordering-eq)
      #/make-ordering-private-gt)
    #/mat b (list) (make-ordering-private-lt)
    
    ; Handle the symbols.
    #/if (symbol? a)
      (if (symbol? b) (lt-autodex a b symbol<?)
      #/make-ordering-private-gt)
    #/if (symbol? b) (make-ordering-private-lt)
    
    ; Handle the exact nonnegative integers.
    #/if (exact-nonnegative-integer? a)
      (if (exact-nonnegative-integer? b) (lt-autodex a b <)
      #/make-ordering-private-gt)
    #/if (exact-nonnegative-integer? b) (make-ordering-private-lt)
    
    ; Handle the structure type descriptors.
    #/struct-type-descriptors-autodex a b)))


(define-generics dex-internals
  (dex-internals-tag dex-internals)
  (dex-internals-autoname dex-internals)
  (dex-internals-autodex dex-internals other)
  (dex-internals-in? dex-internals x)
  (dex-internals-name-of dex-internals x)
  (dex-internals-compare dex-internals a b))

(struct-easy "a dex-encapsulated" (dex-encapsulated internals))

(define/contract (dex? x)
  (-> any/c boolean?)
  (dex-encapsulated? x))

(define/contract (autoname-dex dex)
  (-> dex? any)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-autoname internals))

(define/contract (in-dex? dex x)
  (-> dex? any/c boolean?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-in? internals x))

(define/contract (name-of dex x)
  (-> dex? any/c #/maybe/c name?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-name-of internals x))

(define/contract (compare-by-dex dex a b)
  (-> dex? any/c any/c #/maybe/c dex-result?)
  (dissect dex (dex-encapsulated internals)
  #/dex-internals-compare internals a b))


(struct-easy "a dexable" (dexable dex value))

(define/contract (valid-dexable? x)
  (-> any/c boolean?)
  (expect x (dexable dex value) #f
  #/and (dex? dex) (in-dex? dex value)))

(define/contract (dexableof c)
  (-> contract? contract?)
  (and/c valid-dexable?
  #/if (chaperone-contract? c)
    (struct/c dexable any/c c)
    (make-contract
      
      #:name 'dexableof
      
      #:first-order
      (lambda (x)
        (contract-first-order-passes?
          (struct/c dexable any/c #/lambda (x)
            (contract-first-order-passes? c x))
          x))
      
      #:projection
      (lambda (b)
        (w- c-projection ((contract-projection c) b)
        #/lambda (x)
          (dissect x (dexable dex x)
          #/dexable dex #/c-projection x))))))

(define/contract (compare-dexables a b)
  (-> valid-dexable? valid-dexable? #/maybe/c dex-result?)
  (dissect a (dexable a-dex a)
  #/dissect b (dexable b-dex b)
  #/expect (compare-by-dex dex-dex a-dex b-dex) (just #/ordering-eq)
    (nothing)
  #/compare-by-dex a-dex a b))

(define/contract (name-of-dexable x)
  (-> valid-dexable? name?)
  (dissect x (dexable dex x)
  #/dissect (name-of dex x) (just result)
    result))



(struct-easy "a dex-internals-name" (dex-internals-name)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-name)
    
    (define (dex-internals-autoname this)
      'dex-name)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (name? x))
    
    (define (dex-internals-name-of this x)
      (expect x (name-internal rep) (nothing)
      #/just #/name-internal #/list 'name rep))
    
    (define (dex-internals-compare this a b)
      (if (and (name? a) (name? b))
        (just #/names-autodex a b)
        (nothing)))
  ])

(define/contract dex-name dex?
  (dex-encapsulated #/dex-internals-name))


(struct-easy "a dex-internals-dex" (dex-internals-dex)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define/generic super-tag dex-internals-tag)
    (define/generic super-autodex dex-internals-autodex)
    
    
    (define (dex-internals-tag this)
      'dex-dex)
    
    (define (dex-internals-autoname this)
      'dex-dex)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (dex? x))
    
    (define (dex-internals-name-of this x)
      (if (dex? x)
        (just #/name-internal #/autoname-dex x)
        (nothing)))
    
    (define (dex-internals-compare this a b)
      (expect a (dex-encapsulated a) (nothing)
      #/expect b (dex-encapsulated b) (nothing)
      #/w- tag super-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/super-autodex a b))
  ])

(define/contract dex-dex dex? #/dex-encapsulated #/dex-internals-dex)


(struct-easy "a dex-internals-give-up" (dex-internals-give-up)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-give-up)
    
    (define (dex-internals-autoname this)
      'dex-give-up)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      #f)
    
    (define (dex-internals-name-of this x)
      (nothing))
    
    (define (dex-internals-compare this a b)
      (nothing))
  ])

(define/contract dex-give-up dex?
  (dex-encapsulated #/dex-internals-give-up))


(struct-easy "a dex-internals-default"
  (dex-internals-default
    dex-for-trying-first
    dex-for-trying-second)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-default)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-default first second)
      #/list 'dex-default (autoname-dex first) (autoname-dex second)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-default a1 a2)
      #/dissect other (dex-internals-default b1 b2)
      #/maybe-ordering-or
        (compare-by-dex dex-dex a1 b1)
        (compare-by-dex dex-dex a2 b2)))
    
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
  (dex-encapsulated
  #/dex-internals-default dex-for-trying-first dex-for-trying-second))


(struct-easy "a dex-internals-by-own-method"
  (dex-internals-by-own-method dexable-get-method)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-by-own-method)
    
    (define (dex-internals-autoname this)
      (dissect this
        (dex-internals-by-own-method #/dexable dex get-method)
      #/list 'dex-by-own-method
        (autoname-dex dex)
        (name-of dex get-method)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-by-own-method a)
      #/dissect other (dex-internals-by-own-method b)
      #/compare-dexables a b))
    
    (define (dex-internals-in? this x)
      (dissect this
        (dex-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method x) (just method) #f
      #/in-dex? method x))
    
    (define (dex-internals-name-of this x)
      (dissect this
        (dex-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method x) (just method) (nothing)
      #/name-of method x))
    
    (define (dex-internals-compare this a b)
      (dissect this
        (dex-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method a) (just a-method) (nothing)
      #/expect (get-method b) (just b-method) (nothing)
      #/expect (compare-by-dex dex-dex a-method b-method)
        (just #/ordering-eq)
        (raise-arguments-error 'dex-by-own-method
          "obtained two different methods from the two values being compared"
          "get-method" get-method
          "a" a
          "b" b
          "a-method" a-method
          "b-method" b-method)
      #/compare-by-dex a-method a b))
  ])

(define/contract (dex-by-own-method dexable-get-method)
  (-> (dexableof #/-> any/c #/maybe/c dex?) dex?)
  (dex-encapsulated #/dex-internals-by-own-method dexable-get-method))


(struct-easy "a dex-internals-fix" (dex-internals-fix dexable-unwrap)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-fix)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/list 'dex-fix (autoname-dex dex) (name-of dex unwrap)))
    
    (define (dex-internals-autodex this other)
      (dissect this (dex-internals-fix a)
      #/dissect other (dex-internals-fix b)
      #/compare-dexables a b))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/in-dex? (unwrap #/dex-encapsulated this) x))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/name-of (unwrap #/dex-encapsulated this) x))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-fix #/dexable dex unwrap)
      #/compare-by-dex (unwrap #/dex-encapsulated this) a b))
  ])

(define/contract (dex-fix dexable-unwrap)
  (-> (dexableof #/-> dex? dex?) dex?)
  (dex-encapsulated #/dex-internals-fix dexable-unwrap))


(struct-easy "a dex-internals-struct"
  (dex-internals-struct descriptor counts? fields)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-struct-by-field-position)
    
    (define (dex-internals-autoname this)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/list* 'dex-struct-by-field-position descriptor
      #/list-fmap fields #/dissectfn (list getter position dex)
        (list position #/autoname-dex dex)))
    
    (define (dex-internals-autodex this other)
      (dissect this
        (dex-internals-struct a-descriptor a-counts? a-fields)
      #/dissect other
        (dex-internals-struct b-descriptor b-counts? b-fields)
      #/maybe-ordering-or
        (just
        #/struct-type-descriptors-autodex a-descriptor b-descriptor)
      #/maybe-compare-aligned-lists a-fields b-fields
      #/lambda (a-field b-field)
        (dissect a-field (list a-getter a-position a-dex)
        #/dissect b-field (list b-getter b-position b-dex)
        #/maybe-ordering-or
          (just #/lt-autodex a-position b-position <)
        #/compare-by-dex dex-dex a-dex b-dex)))
    
    (define (dex-internals-in? this x)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/and (counts? x)
      #/nextlet fields fields
        (expect fields (cons field fields) #t
        #/dissect field (list getter position dex)
        
        ; We do a tail call if we can.
        #/mat fields (list) (in-dex? dex #/getter x)
        
        #/and (in-dex? dex #/getter x)
        #/next fields)))
    
    (define (dex-internals-name-of this x)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/expect (counts? x) #t (nothing)
      #/nextlet fields fields rev-result (list)
        (expect fields (cons field fields)
          (just #/name-internal #/cons 'struct #/reverse rev-result)
        #/dissect field (list getter position dex)
        #/expect (name-of dex #/getter x) (just name) (nothing)
        #/dissect name (name-internal rep)
        #/next fields #/cons rep rev-result)))
    
    (define (dex-internals-compare this a b)
      (dissect this (dex-internals-struct descriptor counts? fields)
      #/expect (counts? a) #t (nothing)
      #/expect (counts? b) #t (nothing)
      #/nextlet fields fields
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
          #/nextlet fields fields
            (expect fields (cons field fields) result
            #/dissect field (list getter position dex)
            #/expect
              (and (in-dex? dex #/getter a) (in-dex? dex #/getter b))
              #t
              (nothing)
            #/next fields))
        #/next fields)))
  ])

(define-syntax dex-struct-by-field-position #/lambda (stx)
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
    #`(dex-encapsulated #/dex-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-fmap fields #/lambda (field)
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

(define-syntax dex-struct #/lambda (stx)
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
    #`(dex-encapsulated #/dex-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-kv-map (map list fields getters)
           #/lambda (position field)
             (dissect field (list dex getter)
               #`(list #,getter #,position #,dex))))))



; ===== Clines =======================================================

(define-generics cline-internals
  (cline-internals-tag cline-internals)
  (cline-internals-autoname cline-internals)
  (cline-internals-autodex cline-internals other)
  (cline-internals-dex cline-internals)
  (cline-internals-in? cline-internals x)
  (cline-internals-compare cline-internals a b))

(struct-easy "a cline-encapsulated" (cline-encapsulated internals))

(define/contract (cline? x)
  (-> any/c boolean?)
  (cline-encapsulated? x))

(define/contract (autoname-cline cline)
  (-> cline? any)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-autoname internals))

(define/contract (get-dex-from-cline cline)
  (-> cline? dex?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-dex internals))

(define/contract (in-cline? cline x)
  (-> cline? any/c boolean?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-in? internals x))

(define/contract (compare-by-cline cline a b)
  (-> cline? any/c any/c #/maybe/c cline-result?)
  (dissect cline (cline-encapsulated internals)
  #/cline-internals-compare internals a b))


(struct-easy "a dex-internals-cline" (dex-internals-cline)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-cline)
    
    (define (dex-internals-autoname this)
      'dex-cline)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (cline? x))
    
    (define (dex-internals-name-of this x)
      (if (cline? x)
        (just #/name-internal #/autoname-cline x)
        (nothing)))
    
    (define (dex-internals-compare this a b)
      (expect a (cline-encapsulated a) (nothing)
      #/expect b (cline-encapsulated b) (nothing)
      #/w- tag cline-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/cline-internals-autodex a b))
  ])

(define/contract dex-cline dex?
  (dex-encapsulated #/dex-internals-cline))


(struct-easy "a cline-internals-by-dex" (cline-internals-by-dex dex)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-by-dex)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-by-dex dex)
      #/list 'cline-by-dex #/autoname-dex dex))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-dex a)
      #/dissect other (cline-internals-by-dex b)
      #/compare-by-dex dex-dex a b))
    
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
  (cline-encapsulated #/cline-internals-by-dex dex))


(struct-easy "a cline-internals-give-up" (cline-internals-give-up)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-give-up)
    
    (define (cline-internals-autoname this)
      'cline-give-up)
    
    (define (cline-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (cline-internals-dex this)
      (dex-give-up))
    
    (define (cline-internals-in? this x)
      #f)
    
    (define (cline-internals-compare this a b)
      (nothing))
  ])

(define/contract cline-give-up cline?
  (cline-encapsulated #/cline-internals-give-up))


(struct-easy "a cline-internals-default"
  (cline-internals-default
    cline-for-trying-first
    cline-for-trying-second)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-default)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-default first second)
      #/list 'cline-default
        (autoname-cline first)
        (autoname-cline second)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-default a1 a2)
      #/dissect other (cline-internals-default b1 b2)
      #/maybe-ordering-or
        (compare-by-dex dex-cline a1 b1)
        (compare-by-dex dex-cline a2 b2)))
    
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
  (cline-encapsulated
  #/cline-internals-default
    cline-for-trying-first
    cline-for-trying-second))


(struct-easy "a convert-dex-from-cline-by-own-method"
  (convert-dex-from-cline-by-own-method get-method)
  #:other
  
  #:property prop:procedure
  (lambda (this x)
    (dissect this (convert-dex-from-cline-by-own-method get-method)
    #/expect (get-method x) (just result) (nothing)
    #/just #/get-dex-from-cline result)))

(struct-easy "a cline-internals-by-own-method"
  (cline-internals-by-own-method dexable-get-method)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-by-own-method)
    
    (define (cline-internals-autoname this)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/list 'cline-by-own-method
        (autoname-dex dex)
        (name-of dex get-method)))
    
    (define (cline-internals-autodex this other)
      (dissect this (cline-internals-by-own-method a)
      #/dissect other (cline-internals-by-own-method b)
      #/compare-dexables a b))
    
    (define (cline-internals-dex this)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/dex-by-own-method #/dexable
        (dex-struct convert-dex-from-cline-by-own-method dex)
        (convert-dex-from-cline-by-own-method get-method)))
    
    (define (cline-internals-in? this x)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method x) (just method) #f
      #/in-cline? method x))
    
    (define (cline-internals-compare this a b)
      (dissect this
        (cline-internals-by-own-method #/dexable dex get-method)
      #/expect (get-method a) (just a-method) (nothing)
      #/expect (get-method b) (just b-method) (nothing)
      #/expect (compare-by-dex dex-cline a-method b-method)
        (just #/ordering-eq)
        (raise-arguments-error 'cline-by-own-method
          "obtained two different methods from the two values being compared"
          "get-method" get-method
          "a" a
          "b" b
          "a-method" a-method
          "b-method" b-method)
      #/compare-by-cline a-method a b))
  ])

(define/contract (cline-by-own-method dexable-get-method)
  (-> (dexableof #/-> any/c #/maybe/c cline?) cline?)
  (cline-encapsulated
  #/cline-internals-by-own-method dexable-get-method))


(struct-easy "a convert-dex-from-cline-fix"
  (convert-dex-from-cline-fix unwrap)
  #:other
  
  #:property prop:procedure
  (lambda (this dex)
    (dissect this (convert-dex-from-cline-fix unwrap)
    #/get-dex-from-cline #/unwrap #/cline-by-dex dex)))

(struct-easy "a cline-internals-fix"
  (cline-internals-fix dexable-unwrap)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-fix)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/list 'cline-fix (autoname-dex dex) (name-of dex unwrap)))
    
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
      #/in-cline? (unwrap #/cline-encapsulated this) x))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-fix #/dexable dex unwrap)
      #/compare-by-cline (unwrap #/cline-encapsulated this) a b))
  ])

(define/contract (cline-fix dexable-unwrap)
  (-> (dexableof #/-> cline? cline?) cline?)
  (cline-encapsulated #/cline-internals-fix dexable-unwrap))


(struct-easy "a cline-internals-struct"
  (cline-internals-struct descriptor counts? fields)
  #:other
  
  #:methods gen:cline-internals
  [
    
    (define (cline-internals-tag this)
      'cline-struct-by-field-position)
    
    (define (cline-internals-autoname this)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/list* 'cline-struct-by-field-position descriptor
      #/list-fmap fields #/dissectfn (list getter position cline)
        (list position #/autoname-cline cline)))
    
    (define (cline-internals-autodex this other)
      (dissect this
        (cline-internals-struct a-descriptor a-counts? a-fields)
      #/dissect other
        (cline-internals-struct b-descriptor b-counts? b-fields)
      #/maybe-ordering-or
        (just
        #/struct-type-descriptors-autodex a-descriptor b-descriptor)
      #/maybe-compare-aligned-lists a-fields b-fields
      #/lambda (a-field b-field)
        (dissect a-field (list a-getter a-position a-cline)
        #/dissect b-field (list b-getter b-position b-cline)
        #/maybe-ordering-or
          (just #/lt-autodex a-position b-position <)
        #/compare-by-dex dex-cline a-cline b-cline)))
    
    (define (cline-internals-in? this x)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/and (counts? x)
      #/nextlet fields fields
        (expect fields (cons field fields) #t
        #/dissect field (list getter position cline)
        
        ; We do a tail call if we can.
        #/mat fields (list) (in-cline? cline #/getter x)
        
        #/and (in-cline? cline #/getter x)
        #/next fields)))
    
    (define (cline-internals-dex this)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/dex-internals-struct descriptor counts?
      #/list-fmap fields #/dissectfn (list getter position cline)
        (list getter position #/get-dex-from-cline cline)))
    
    (define (cline-internals-compare this a b)
      (dissect this (cline-internals-struct descriptor counts? fields)
      #/expect (counts? a) #t (nothing)
      #/expect (counts? b) #t (nothing)
      #/nextlet fields fields
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
          #/nextlet fields fields
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

(define-syntax cline-struct-by-field-position #/lambda (stx)
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
    #`(cline-encapsulated #/cline-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-fmap fields #/lambda (field)
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

(define-syntax cline-struct #/lambda (stx)
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
    #`(cline-encapsulated #/cline-internals-struct #,struct:foo #,foo?
      #/list
        #,@(list-kv-map (map list fields getters)
           #/lambda (position field)
             (dissect field (list cline getter)
               #`(list #,getter #,position #,cline))))))



; ===== Merges and fuses =============================================

(define-generics furge-internals
  (furge-internals-tag furge-internals)
  (furge-internals-autoname furge-internals)
  (furge-internals-autodex furge-internals other)
  (furge-internals-call furge-internals a b))

(struct-easy "a merge-encapsulated" (merge-encapsulated internals))

(define/contract (merge? x)
  (-> any/c boolean?)
  (merge-encapsulated? x))

(define/contract (autoname-merge merge)
  (-> merge? any)
  (dissect merge (merge-encapsulated internals)
  #/cons 'merge #/furge-internals-autoname internals))

(define/contract (call-merge merge a b)
  (-> merge? any/c any/c maybe?)
  (dissect merge (merge-encapsulated internals)
  #/furge-internals-call internals a b))

(struct-easy "a fuse-encapsulated" (fuse-encapsulated internals))

(define/contract (fuse? x)
  (-> any/c boolean?)
  (fuse-encapsulated? x))

(define/contract (autoname-fuse fuse)
  (-> fuse? any)
  (dissect fuse (fuse-encapsulated internals)
  #/cons 'fuse #/furge-internals-autoname internals))

(define/contract (call-fuse fuse a b)
  (-> fuse? any/c any/c maybe?)
  (dissect fuse (fuse-encapsulated internals)
  #/furge-internals-call internals a b))


(struct-easy "a dex-internals-merge" (dex-internals-merge)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-merge)
    
    (define (dex-internals-autoname this)
      'dex-merge)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (merge? x))
    
    (define (dex-internals-name-of this x)
      (if (merge? x)
        (just #/name-internal #/autoname-merge x)
        (nothing)))
    
    (define (dex-internals-compare this a b)
      (expect a (merge-encapsulated a) (nothing)
      #/expect b (merge-encapsulated b) (nothing)
      #/w- tag furge-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/furge-internals-autodex a b))
  ])

(define/contract dex-merge dex?
  (dex-encapsulated #/dex-internals-merge))


(struct-easy "a dex-internals-fuse" (dex-internals-fuse)
  #:other
  
  #:methods gen:dex-internals
  [
    
    (define (dex-internals-tag this)
      'dex-fuse)
    
    (define (dex-internals-autoname this)
      'dex-fuse)
    
    (define (dex-internals-autodex this other)
      (just #/ordering-eq))
    
    (define (dex-internals-in? this x)
      (fuse? x))
    
    (define (dex-internals-name-of this x)
      (if (fuse? x)
        (just #/name-internal #/autoname-fuse x)
        (nothing)))
    
    (define (dex-internals-compare this a b)
      (expect a (fuse-encapsulated a) (nothing)
      #/expect b (fuse-encapsulated b) (nothing)
      #/w- tag furge-internals-tag
      #/maybe-ordering-or (just #/lt-autodex (tag a) (tag b) symbol<?)
      #/furge-internals-autodex a b))
  ])

(define/contract dex-fuse dex?
  (dex-encapsulated #/dex-internals-fuse))


(struct-easy "a fuse-internals-by-merge"
  (fuse-internals-by-merge merge)
  #:other
  
  #:methods gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'fuse-by-merge)
    
    (define (furge-internals-autoname this)
      (dissect this (fuse-internals-by-merge merge)
      #/list 'fuse-by-merge #/autoname-merge merge))
    
    (define (furge-internals-autodex this other)
      (dissect this (fuse-internals-by-merge a)
      #/dissect other (fuse-internals-by-merge b)
      #/compare-by-dex dex-merge a b))
    
    (define (furge-internals-call this a b)
      (dissect this (fuse-internals-by-merge merge)
      #/call-merge merge a b))
  ])

(define/contract (fuse-by-merge merge)
  (-> merge? fuse?)
  (fuse-encapsulated #/fuse-internals-by-merge merge))


(struct-easy "a furge-internals-by-dex" (furge-internals-by-dex dex)
  #:other
  
  #:methods gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'furge-by-dex)
    
    (define (furge-internals-autoname this)
      (dissect this (furge-internals-by-dex dex)
      #/list 'furge-by-dex #/autoname-dex dex))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-by-dex a)
      #/dissect other (furge-internals-by-dex b)
      #/compare-by-dex dex-dex a b))
    
    (define (furge-internals-call this a b)
      (dissect this (furge-internals-by-dex dex)
      #/mat (compare-by-dex dex a b) (just #/ordering-eq)
        (just a)
        (nothing)))
  ])

(define/contract (merge-by-dex dex)
  (-> dex? merge?)
  (merge-encapsulated #/furge-internals-by-dex dex))

; TODO: See if we want to export this.
(define/contract (fuse-by-dex dex)
  (-> dex? fuse?)
  (fuse-encapsulated #/furge-internals-by-dex dex))


(struct-easy "a furge-internals-by-own-method"
  (furge-internals-by-own-method
    error-name dex-furge call-furge dexable-get-method)
  #:other
  
  #:methods gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'furge-by-own-method)
    
    (define (furge-internals-autoname this)
      (dissect this
        (furge-internals-by-own-method _ _ _ #/dexable dex get-method)
      #/list 'furge-by-own-method
        (autoname-dex dex)
        (name-of dex get-method)))
    
    (define (furge-internals-autodex this other)
      (dissect this (furge-internals-by-own-method _ _ _ a)
      #/dissect other (furge-internals-by-own-method _ _ _ b)
      #/compare-dexables a b))
    
    (define (furge-internals-call this a b)
      (dissect this
        (furge-internals-by-own-method error-name dex-furge call-furge
        #/dexable dex get-method)
      #/expect (get-method a) (just a-method) (nothing)
      #/expect (get-method b) (just b-method) (nothing)
      #/expect (compare-by-dex dex-furge a-method b-method)
        (just #/ordering-eq)
        (raise-arguments-error error-name
          "obtained two different methods from the two input values"
          "get-method" get-method
          "a" a
          "b" b
          "a-method" a-method
          "b-method" b-method)
      #/expect (call-furge a-method a b) (just result) (nothing)
      #/expect (get-method result) (just result-method)
        (raise-arguments-error error-name
          "could not obtain a method from the result value"
          "get-method" get-method
          "method" a-method
          "a" a
          "b" b
          "result" result)
      #/expect (compare-by-dex dex-furge a-method result-method)
        (just #/ordering-eq)
        (raise-arguments-error error-name
          "obtained two different methods from the input and the output"
          "get-method" get-method
          "a" a
          "b" b
          "result" result
          "a-and-b-method" a-method
          "result-method" result-method)
      #/just result))
  ])

(define/contract (merge-by-own-method dexable-get-method)
  (-> (dexableof #/-> any/c #/maybe/c merge?) merge?)
  (merge-encapsulated #/furge-internals-by-own-method
    'merge-by-own-method dex-merge call-merge dexable-get-method))

(define/contract (fuse-by-own-method dexable-get-method)
  (-> (dexableof #/-> any/c #/maybe/c fuse?) fuse?)
  (fuse-encapsulated #/furge-internals-by-own-method
    'fuse-by-own-method dex-fuse call-fuse dexable-get-method))


(struct-easy "a furge-internals-fix"
  (furge-internals-fix call-furge furge-encapsulated dexable-unwrap)
  #:other
  
  #:methods gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'furge-fix)
    
    (define (furge-internals-autoname this)
      (dissect this (furge-internals-fix _ _ #/dexable dex unwrap)
      #/list 'furge-fix (autoname-dex dex) (name-of dex unwrap)))
    
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

(define/contract (merge-fix dexable-unwrap)
  (-> (dexableof #/-> merge? merge?) merge?)
  (merge-encapsulated
  #/furge-internals-fix call-merge merge-encapsulated dexable-unwrap))

(define/contract (fuse-fix dexable-unwrap)
  (-> (dexableof #/-> fuse? fuse?) fuse?)
  (fuse-encapsulated
  #/furge-internals-fix call-fuse fuse-encapsulated dexable-unwrap))


(struct-easy "a furge-internals-struct"
  (furge-internals-struct
    autoname-furge dex-furge call-furge
    descriptor constructor counts? fields)
  #:other
  
  #:methods gen:furge-internals
  [
    
    (define (furge-internals-tag this)
      'furge-struct-by-field-position)
    
    (define (furge-internals-autoname this)
      (dissect this
        (furge-internals-struct
          autoname-furge _ _ descriptor constructor counts? fields)
      #/list* 'furge-struct-by-field-position descriptor
      #/list-fmap fields #/dissectfn (list getter position furge)
        (list position #/autoname-furge furge)))
    
    (define (furge-internals-autodex this other)
      (dissect this
        (furge-internals-struct
          _ dex-furge _ a-descriptor a-constructor a-counts? a-fields)
      #/dissect other
        (furge-internals-struct
          _ _ _ b-descriptor a-constructor b-counts? b-fields)
      #/maybe-ordering-or
        (just
        #/struct-type-descriptors-autodex a-descriptor b-descriptor)
      #/maybe-compare-aligned-lists a-fields b-fields
      #/lambda (a-field b-field)
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
      #/nextlet fields fields args (hasheq)
        (expect fields (cons field fields)
          (apply constructor #/build-list n #/lambda (i)
            (hash-ref args i))
        #/dissect field (list getter position furge)
        #/next fields
        #/hash-set args position
          (call-furge furge (getter a) (getter b)))))
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
        #,@(list-fmap fields #/lambda (field)
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

(define-syntax merge-struct-by-field-position #/lambda (stx)
  (expand-furge-struct-by-field-position stx "merges"
    #'merge-encapsulated #'autoname-merge #'dex-merge #'call-merge))

(define-syntax fuse-struct-by-field-position #/lambda (stx)
  (expand-furge-struct-by-field-position stx "fuses"
    #'fuse-encapsulated #'autoname-fuse #'dex-fuse #'call-fuse))

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
        #,@(list-kv-map (map list fields getters)
           #/lambda (position field)
             (dissect field (list furge getter)
               #`(list #,getter #,position #,furge))))))

(define-syntax merge-struct #/lambda (stx)
  (expand-furge-struct stx "merges"
    #'merge-encapsulated #'autoname-merge #'dex-merge #'call-merge))

(define-syntax fuse-struct #/lambda (stx)
  (expand-furge-struct stx "fuses"
    #'fuse-encapsulated #'autoname-fuse #'dex-fuse #'call-fuse))
