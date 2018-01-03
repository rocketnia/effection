#lang parendown racket/base

(require #/only-in racket/contract/base
  -> any/c chaperone-contract? or/c struct/c)
(require #/only-in racket/contract/region define/contract)

(require "../private/util.rkt")


(provide #/struct-out nothing)
(provide #/struct-out just)
(provide maybe? maybe/c)


(struct-easy "a nothing" (nothing) #:equal)
(struct-easy "a just" (just value) #:equal)

(define/contract (maybe? c)
  (-> any/c boolean?)
  (or (nothing? c) (just? c)))

(define/contract (maybe/c c)
  (-> chaperone-contract? chaperone-contract?)
  (or/c nothing? #/struct/c just c))
