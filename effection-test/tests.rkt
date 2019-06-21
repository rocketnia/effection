#lang parendown racket/base

; main.rkt
;
; Unit tests of Effection.

(require rackunit)

(require #/only-in lathe-comforts expect fn w-)
(require #/only-in lathe-comforts/maybe just just-value)
(require #/only-in lathe-comforts/struct struct-easy)

(require effection/order)

; (We provide nothing from this module.)


(struct-easy (mk-just1)
  #:other
  
  #:property prop:procedure
  (fn this result
    (expect this (mk-just1)
      (error "Expected this to be a mk-just1")
    #/just result))
)
(struct-easy (mk-just2)
  #:other
  
  #:property prop:procedure
  (fn this result
    (expect this (mk-just2)
      (error "Expected this to be a mk-just2")
    #/just result))
)


(check-exn
  exn:fail:contract?
  (fn
    (compare-by-dex
      ; This dex compares any dex which has itself in its domain. The
      ; method of comparison (the dex) is obtained from the value by
      ; doing nothing; the method is the value.
      (dex-by-own-method
        (just-value #/dexed-of (dex-struct mk-just1) #/mk-just1))
      
      ; These two values are dexes, and they are each in their own
      ; domain, but they're different dexes. When they're compared,
      ; it will not be possible to decide upon a single method of
      ; comparison, so a dynamic error will be raised.
      (dex-by-own-method
        (just-value #/dexed-of (dex-struct mk-just1) #/mk-just1))
      (dex-by-own-method
        (just-value #/dexed-of (dex-struct mk-just2) #/mk-just2))))
  "Calling a `dex-by-own-method` on two values with different methods raises an error")

(check-exn
  exn:fail?
  (fn
    (compare-by-cline
      ; This cline compares any cline which has itself in its domain.
      ; The method of comparison (the cline) is obtained from the
      ; value by doing nothing; the method is the value.
      (cline-by-own-method
        (just-value #/dexed-of (dex-struct mk-just1) #/mk-just1))
      
      ; These two values are clines, and they are each in their own
      ; domain, but they're different clines. When they're compared,
      ; it will not be possible to decide upon a single method of
      ; comparison, so a dynamic error will be raised.
      (cline-by-own-method
        (just-value #/dexed-of (dex-struct mk-just1) #/mk-just1))
      (cline-by-own-method
        (just-value #/dexed-of (dex-struct mk-just2) #/mk-just2))))
  "Calling a `cline-by-own-method` on two values with different methods raises an error")


; TODO: We can no longer test with actual `nothing` and `just` values
; from Lathe Comforts because those identifiers don't carry struct
; information anymore. We should make a `dex-match` to test those
; with, and until we do, we test using these actual structs instead.
(struct-easy (s-nothing) #:equal)
(struct-easy (s-just value) #:equal)

(define (dex-maybe dex-elem)
  (dex-default (dex-struct s-nothing) (dex-struct s-just dex-elem)))

(check-equal?
  (compare-by-dex (dex-name)
    (just-value #/name-of (dex-struct s-nothing) (s-nothing))
    (just-value #/name-of (dex-maybe dex-give-up) (s-nothing)))
  (just #/ordering-eq)
  "Using `name-of` with different dexes gives the same name")


(struct-easy (custom-pair a b))

(check-equal?
  (compare-by-dex
    (dex-struct-by-field-position custom-pair
      [0 (dex-dex)]
      [1 (dex-cline)])
    (custom-pair (dex-give-up) (cline-give-up))
    (custom-pair (dex-give-up) (cline-give-up)))
  (just #/ordering-eq)
  "Specifying fields in order with `dex-struct-by-field-position` works")
(check-equal?
  (compare-by-dex
    (dex-struct-by-field-position custom-pair
      [1 (dex-cline)]
      [0 (dex-dex)])
    (custom-pair (dex-give-up) (cline-give-up))
    (custom-pair (dex-give-up) (cline-give-up)))
  (just #/ordering-eq)
  "Specifying fields out of order with `dex-struct-by-field-position` works")

(check-equal?
  (w- name
    (just-value #/name-of (dex-dex)
    #/dex-struct-by-field-position custom-pair
      [1 (dex-cline)]
      [0 (dex-dex)])
    (compare-by-dex (dex-name) name name))
  (just #/ordering-eq)
  "Names that internally contain structure type descriptors and exact nonnegative integers can be compared")
