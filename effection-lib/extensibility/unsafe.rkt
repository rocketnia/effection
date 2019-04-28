#lang parendown racket/base

(require #/only-in racket/contract/base recontract-out)

(require #/submod effection/extensibility/base private/unsafe)

; TODO: See if we can use something more like this at some point. For
; now, `recontract-out` can't be combined with `all-from-out`.
#;
(provide #/all-from-out
  (submod effection/extensibility/base private/unsafe))

(provide #/recontract-out
  run-extfx!)
