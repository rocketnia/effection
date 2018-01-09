#lang parendown racket/base

; cline-by-own-method-error.rkt
;
; A function to cause the run time error of `cline-by-own-method`.

; TODO: Use this in a unit test.

(require rackunit)

(require #/only-in lathe expect)

(require effection/maybe)
(require effection/order)
(require effection/private/util)

(provide #/all-defined-out)


(struct-easy "a mk-just1" (mk-just1)
  #:other
  
  #:property prop:procedure
  (lambda (this result)
    (expect this (mk-just1)
      (error "Expected this to be a mk-just1")
    #/just result))
)
(struct-easy "a mk-just2" (mk-just2)
  #:other
  
  #:property prop:procedure
  (lambda (this result)
    (expect this (mk-just2)
      (error "Expected this to be a mk-just2")
    #/just result))
)

(define (cause-cline-by-own-method-error)
  (compare-by-cline
    ; This cline compares any cline which has itself in its domain.
    ; The method of comparison (the cline) is obtained from the value
    ; by doing nothing; the method is the value.
    (cline-by-own-method #/dexable (dex-struct mk-just1) #/mk-just1)
    
    ; These two values are clines, and they are each in their own
    ; domain, but they're different clines. When they're compared, it
    ; will not be possible to decide upon a single method of
    ; comparison, so a dynamic error will be raised.
    (cline-by-own-method #/dexable (dex-struct mk-just1) #/mk-just1)
    (cline-by-own-method #/dexable (dex-struct mk-just2) #/mk-just2)))
