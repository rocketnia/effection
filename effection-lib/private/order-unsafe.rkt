#lang parendown racket/base


(require #/only-in racket/generic define-generics)

(require #/only-in lathe-comforts expect fn)
(require #/only-in lathe-comforts/struct struct-easy)


(provide #/struct-out name)
(provide
  gen:dex-internals
  dex-internals-tag
  dex-internals-autoname
  dex-internals-autodex
  dex-internals-in?
  dex-internals-name-of
  dex-internals-compare)
(provide #/struct-out dex)

(provide
  gen:cline-internals
  cline-internals-tag
  cline-internals-autoname
  cline-internals-autodex
  cline-internals-dex
  cline-internals-in?
  cline-internals-compare)
(provide #/struct-out cline)

(provide
  gen:furge-internals
  furge-internals-tag
  furge-internals-autoname
  furge-internals-autodex
  furge-internals-call)
(provide #/struct-out merge)
(provide #/struct-out fuse)

(provide #/struct-out table)

(provide #/struct-out fusable-function)



; ===== Names, dexes, and dexables ===================================

; Internally, we represent name values as data made of structure type
; descriptors, uninterned symbols, exact rational numbers, interned
; symbols, empty lists, and cons cells. For sorting purposes, we
; consider them to ascend in that order.
;
; This is the struct type we "encapsulate" that in, but we offer it as
; an unsafe export.
;
(struct-easy (name rep)
  #:error-message-phrase "a name")

(define-generics dex-internals
  (dex-internals-tag dex-internals)
  (dex-internals-autoname dex-internals)
  (dex-internals-autodex dex-internals other)
  (dex-internals-in? dex-internals x)
  (dex-internals-name-of dex-internals x)
  (dex-internals-compare dex-internals a b))

(struct-easy (dex internals))


; ===== Clines =======================================================

(define-generics cline-internals
  (cline-internals-tag cline-internals)
  (cline-internals-autoname cline-internals)
  (cline-internals-autodex cline-internals other)
  (cline-internals-dex cline-internals)
  (cline-internals-in? cline-internals x)
  (cline-internals-compare cline-internals a b))

(struct-easy (cline internals))


; ===== Merges and fuses =============================================

(define-generics furge-internals
  (furge-internals-tag furge-internals)
  (furge-internals-autoname furge-internals)
  (furge-internals-autodex furge-internals other)
  (furge-internals-call furge-internals a b))

(struct-easy (merge internals))
(struct-easy (fuse internals))


; ===== Tables =======================================================

(struct-easy (table hash))


; ===== Fusable functions ============================================

(struct-easy (fusable-function proc)
  #:other
  
  #:property prop:procedure
  (fn this arg
    (expect this (fusable-function proc)
      (error "Expected this to be a fusable-function")
    #/proc arg)))
