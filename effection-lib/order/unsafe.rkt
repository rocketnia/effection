#lang parendown racket/base


(require #/only-in racket/contract/base -> any any/c list/c listof)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/generic define-generics)

(require #/only-in lathe-comforts dissect dissectfn fn mat w-)
(require #/only-in lathe-comforts/list list-map)
(require #/only-in lathe-comforts/struct struct-easy)

; TODO: Uncomment this and the `all-from-out` line once we have this
; rearranged a bit. This use of a submodule seems a lot better for
; code organization than moving things between files like we've been
; doing.
;
;(require #/submod effection/order/base private/unsafe)
(require #/only-in effection/order/private
  names-autodex ordering-lt
  
  struct:name name name? name-rep
  
  struct:ordering-private ordering-private ordering-private?
  ordering-private-ordering)


;(provide #/all-from-out #/submod effection/order/base private/unsafe)

(provide #/struct-out ordering-private)

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
(provide autoname-dex)

(provide
  gen:cline-internals
  cline-internals-tag
  cline-internals-autoname
  cline-internals-autodex
  cline-internals-dex
  cline-internals-in?
  cline-internals-compare)
(provide #/struct-out cline)
(provide autoname-cline)

(provide
  gen:furge-internals
  furge-internals-tag
  furge-internals-autoname
  furge-internals-autodex
  furge-internals-call)
(provide #/struct-out merge)
(provide #/struct-out fuse)
(provide autoname-merge)
(provide autoname-fuse)

(provide #/struct-out table)

(provide table->sorted-list)



; ===== Orderings ====================================================

; NOTE: We define `ordering-private` in `effection/order/private`, but
; we re-export it here.


; ===== Names, dexes, and dexables ===================================

(define-generics dex-internals
  (dex-internals-tag dex-internals)
  (dex-internals-autoname dex-internals)
  (dex-internals-autodex dex-internals other)
  (dex-internals-in? dex-internals x)
  (dex-internals-name-of dex-internals x)
  (dex-internals-compare dex-internals a b))

(struct-easy (dex internals))

(define/contract (autoname-dex x)
  (-> dex? any)
  (dissect x (dex internals)
  #/cons 'name:dex #/dex-internals-autoname internals))


; ===== Clines =======================================================

(define-generics cline-internals
  (cline-internals-tag cline-internals)
  (cline-internals-autoname cline-internals)
  (cline-internals-autodex cline-internals other)
  (cline-internals-dex cline-internals)
  (cline-internals-in? cline-internals x)
  (cline-internals-compare cline-internals a b))

(struct-easy (cline internals))

(define/contract (autoname-cline x)
  (-> cline? any)
  (dissect x (cline internals)
  #/cons 'name:cline #/cline-internals-autoname internals))


; ===== Merges and fuses =============================================

(define-generics furge-internals
  (furge-internals-tag furge-internals)
  (furge-internals-autoname furge-internals)
  (furge-internals-autodex furge-internals other)
  (furge-internals-call furge-internals a b))

(struct-easy (merge internals))
(struct-easy (fuse internals))

(define/contract (autoname-merge x)
  (-> merge? any)
  (dissect x (merge internals)
  #/cons 'name:merge #/furge-internals-autoname internals))

(define/contract (autoname-fuse x)
  (-> fuse? any)
  (dissect x (fuse internals)
  #/cons 'name:fuse #/furge-internals-autoname internals))


; ===== Tables =======================================================

(struct-easy (table hash))

(define/contract (table->sorted-list tab)
  (-> table? #/listof #/list/c name? any/c)
  (dissect tab (table hash)
  #/list-map
    (sort (hash->list hash) #/fn a b
      (dissect a (cons ak av)
      #/dissect b (cons bk bv)
      #/w- dex-result (names-autodex (name ak) (name bk))
      #/mat dex-result (ordering-lt) #t
      #/mat dex-result (ordering-private #/ordering-lt) #t
        #f))
  #/dissectfn (cons k v)
    (list (name k) v)))
