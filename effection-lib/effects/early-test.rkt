#lang parendown racket/base

(require #/for-syntax racket/base)
(require racket/stxparam)

(require rackunit)



(define-syntax-parameter is-lifted #f)

(define-syntax (lift stx) #/syntax-case stx () #/ (_ expr)
  (syntax-local-lift-expression
    #`(syntax-parameterize ([is-lifted #t])
        expr)))

(define-syntax (assert-is-lifted stx) #/syntax-case stx () #/ (_)
  (unless (syntax-parameter-value #'is-lifted)
    (error "Must be used in a lifted context"))
  #'(void))

(define-syntax (assert-is-not-lifted stx) #/syntax-case stx () #/ (_)
  (when (syntax-parameter-value #'is-lifted)
    (error "Must be used in a non-lifted context"))
  #'(void))

(define-syntax (lift-gensym stx)
  #'(lift #/begin (assert-is-lifted) #/gensym))

(assert-is-not-lifted)

(define (foo)
  (lift-gensym))

(check-equal? (foo) (foo))


(define (foo2)
  (define (foo3)
    (lift-gensym))
  (foo3))

(check-equal? (foo2) (foo2))
