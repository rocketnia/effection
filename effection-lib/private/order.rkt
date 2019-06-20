#lang parendown racket/base


(require #/for-syntax #/only-in syntax/parse expr)


(require #/only-in racket/contract/base
  -> any/c flat-contract? list/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/contract/combinator
  make-contract make-flat-contract)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts expect dissect fn mat w- w-loop)
(require #/only-in lathe-comforts/struct
  auto-equal auto-write define-imitation-simple-struct)

(require #/prefix-in internal: #/only-in
  effection/private/order-unsafe
  
  name name?)


(provide #/all-defined-out)



; ===== Miscellaneous utilities ======================================

(define (make-appropriate-non-chaperone-contract c)
  (if (flat-contract? c)
    make-flat-contract
    make-contract))


; ===== Orderings ====================================================

(define-imitation-simple-struct (ordering-lt?) ordering-lt
  'ordering-lt (current-inspector) (auto-write) (auto-equal))
(define-imitation-simple-struct (ordering-eq?) ordering-eq
  'ordering-eq (current-inspector) (auto-write) (auto-equal))
(define-imitation-simple-struct (ordering-private?) ordering-private
  'ordering-private (current-inspector) (auto-write) (auto-equal))
(define-imitation-simple-struct (ordering-gt?) ordering-gt
  'ordering-gt (current-inspector) (auto-write) (auto-equal))

(define/contract (dex-result? x)
  (-> any/c boolean?)
  (or (ordering-private? x) (ordering-eq? x)))

(define/contract (cline-result? x)
  (-> any/c boolean?)
  (or (dex-result? x) (ordering-lt? x) (ordering-gt? x)))

(define-simple-macro (ordering-or first:expr second:expr)
  (w- result first
  #/expect result (ordering-eq) result
    second))

; TODO: See if we should export this.
(define/contract (hide-cline-result cline-result)
  (-> cline-result? dex-result?)
  (mat cline-result (ordering-eq)
    (ordering-eq)
    (ordering-private)))

(define/contract (lt-autocline a b <?)
  (-> any/c any/c (-> any/c any/c boolean?) cline-result?)
  (if (<? a b) (ordering-lt)
  #/if (<? b a) (ordering-gt)
  #/ordering-eq))

(define/contract (lt-autodex a b <?)
  (-> any/c any/c (-> any/c any/c boolean?) dex-result?)
  (hide-cline-result #/lt-autocline a b <?))


; ===== Names, dexes, and dexables ===================================

; TODO: Test this implementation very thoroughly. We need to know what
; happens when this module is used through diamond dependencies, what
; happens if this module is somehow visited more than once, and that
; kind of thing. We probably won't be able to guarantee uninterned
; symbols and struct type descriptors will be sorted the same way in
; every phase, but we can at least document why not.
(define object-identities-semaphore* (make-semaphore 1))
(define object-identities-to-ranks* (make-weak-hasheq))
(define object-identities-next-rank* 0)
(define/contract (object-identity-rank object-identity)
  (-> any/c natural?)
  (call-with-semaphore object-identities-semaphore* #/fn
    (if (hash-has-key? object-identities-to-ranks* object-identity)
      (hash-ref object-identities-to-ranks* object-identity)
    #/w- rank object-identities-next-rank*
      (hash-set! object-identities-to-ranks* object-identity rank)
      (set! object-identities-next-rank* (add1 rank))
      rank)))
(define/contract (object-identities-autodex a b)
  (-> any/c any/c dex-result?)
  (lt-autodex (object-identity-rank a) (object-identity-rank b) <))

(define/contract (exact-rational? v)
  (-> any/c boolean?)
  (and (rational? v) (exact? v)))

(define/contract (name? x)
  (-> any/c boolean?)
  (internal:name? x))

(define/contract (names-autocline-candid a b)
  (-> name? name? cline-result?)
  (dissect a (internal:name a)
  #/dissect b (internal:name b)
  #/w-loop next a a b b
    
    ; Handle the cons cells.
    (mat a (cons a-first a-rest)
      (mat b (cons b-first b-rest)
        (ordering-or (next a-first b-first) (next a-rest b-rest))
      #/ordering-gt)
    #/mat b (cons b-first b-rest) (ordering-lt)
    
    ; Handle the empty lists.
    #/mat a (list)
      (mat b (list) (ordering-eq)
      #/ordering-gt)
    #/mat b (list) (ordering-lt)
    
    ; Handle the interned symbols.
    #/w- interned-symbol?
      (fn x
        (and (symbol? x) (symbol-interned? x)))
    #/if (interned-symbol? a)
      (if (interned-symbol? b) (lt-autodex a b symbol<?)
      #/ordering-gt)
    #/if (interned-symbol? b) (ordering-lt)
    
    ; Handle the exact rational numbers.
    #/if (exact-rational? a)
      (if (exact-rational? b) (lt-autodex a b <)
      #/ordering-gt)
    #/if (exact-rational? b) (ordering-lt)
    
    ; Handle the uninterned symbols.
    #/w- uninterned-symbol?
      (fn x
        (and
          (symbol? x)
          (not #/symbol-interned? x)
          (not #/symbol-unreadable? x)))
    #/if (uninterned-symbol? a)
      (if (uninterned-symbol? b) (object-identities-autodex a b)
      #/ordering-gt)
    #/if (uninterned-symbol? b) (ordering-lt)
    
    ; Handle the structure type descriptors.
    #/object-identities-autodex a b)))

(define/contract (names-autodex a b)
  (-> name? name? dex-result?)
  (hide-cline-result #/names-autocline-candid a b))
