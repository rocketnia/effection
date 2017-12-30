#lang info

(define collection "effection")

(define deps (list))
(define build-deps
  (list "base" "effection-lib" "racket-doc" "scribble-lib"))

(define scribblings (list (list "scribblings/effection.scrbl" (list))))
