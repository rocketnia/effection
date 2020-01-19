#lang info

(define collection "effection")

(define deps (list "base"))
(define build-deps
  (list
    "parendown-lib"
    "scribble-lib"))

(define scribblings
  (list (list "scribblings/effection.scrbl" (list))))
