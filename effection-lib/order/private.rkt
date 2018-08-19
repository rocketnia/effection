#lang parendown racket/base


(require #/for-syntax #/only-in syntax/parse expr)


(require #/only-in racket/contract/base -> any/c list/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/math natural?)
(require #/only-in syntax/parse/define define-simple-macro)

(require #/only-in lathe-comforts expect dissect fn mat w- w-loop)
(require #/only-in lathe-comforts/struct struct-easy)

(require #/prefix-in internal: #/only-in
  effection/order/private/unsafe
  
  name name? ordering-private ordering-private?)


(provide #/all-defined-out)



; ===== Orderings ====================================================

(struct-easy (ordering-lt) #:equal)
(struct-easy (ordering-eq) #:equal)
(struct-easy (ordering-gt) #:equal)

(define/contract (ordering-private? x)
  (-> any/c boolean?)
  (internal:ordering-private? x))

(define/contract (dex-result? x)
  (-> any/c boolean?)
  (or (ordering-private? x) (ordering-eq? x)))

; NOTE: We make these procedures because if we provided them as bare
; values, we would encourage people to write code that appeared to
; keep the ordering private but actually exposed it to a simple `eq?`
; check. Of course, Effection-unsafe Racket code can still compare
; these values by writing them to streams and observing the data
; that's written this way, but at least that's harder to do by
; accident.
(define/contract (make-ordering-private-lt)
  (-> ordering-private?)
  (internal:ordering-private #/ordering-lt))
(define/contract (make-ordering-private-gt)
  (-> ordering-private?)
  (internal:ordering-private #/ordering-gt))

(define-simple-macro (ordering-or first:expr second:expr)
  (w- result first
  #/expect result (ordering-eq) result
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


; ===== Names, dexes, and dexables ===================================

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
  (-> struct-type? natural?)
  (call-with-semaphore descriptors-semaphore* #/fn
    (if (hash-has-key? descriptors-to-ranks* descriptor)
      (hash-ref descriptors-to-ranks* descriptor)
    #/w- rank descriptors-next-rank*
      (hash-set! descriptors-to-ranks* descriptor rank)
      (set! descriptors-next-rank* (add1 rank))
      rank)))
(define/contract (struct-type-descriptors-autodex a b)
  (-> struct-type? struct-type? dex-result?)
  (lt-autodex (descriptor-rank a) (descriptor-rank b) <))

(define/contract (exact-rational? v)
  (-> any/c boolean?)
  (and (rational? v) (exact? v)))

(define/contract (name? x)
  (-> any/c boolean?)
  (internal:name? x))

(define/contract (names-autodex a b)
  (-> name? name? dex-result?)
  (dissect a (internal:name a)
  #/dissect b (internal:name b)
  #/w-loop next a a b b
    
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
    
    ; Handle the interned symbols.
    #/w- interned-symbol?
      (fn x
        (and (symbol? x) (symbol-interned? x)))
    #/if (interned-symbol? a)
      (if (interned-symbol? b) (lt-autodex a b symbol<?)
      #/make-ordering-private-gt)
    #/if (interned-symbol? b) (make-ordering-private-lt)
    
    ; Handle the exact rational numbers.
    #/if (exact-rational? a)
      (if (exact-rational? b) (lt-autodex a b <)
      #/make-ordering-private-gt)
    #/if (exact-rational? b) (make-ordering-private-lt)
    
    ; Handle the structure type descriptors.
    #/struct-type-descriptors-autodex a b)))
