#lang parendown racket/base


(require #/only-in lathe-comforts/struct struct-easy)

(provide #/struct-out name)


; Internally, we represent name values as data made of structure type
; descriptors, exact nonnegative integers, interned symbols, empty
; lists, and cons cells, and for sorting purposes, we consider them to
; ascend in that order.
;
; This is the struct type we "encapsulate" that in, but we offer it as
; an unsafe export.
;
(struct-easy (name rep)
  #:error-message-phrase "a name")
