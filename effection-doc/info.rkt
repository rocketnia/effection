#lang info

(define collection "effection")

(define deps (list "base"))
(define build-deps
  (list "effection-lib" "parendown-lib" "racket-doc" "scribble-lib"))

(define scribblings
  (list (list "scribblings/effection.scrbl" (list))))
