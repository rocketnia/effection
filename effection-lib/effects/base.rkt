#lang parendown racket/base


(require #/only-in racket/contract/base
  -> ->i =/c and/c any any/c contract? listof struct/dc)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe expect mat next nextlet w-)

(require #/only-in effection/maybe/base just nothing maybe/c)
(require #/only-in effection/order/base fuse? name?)

(require "../private/util.rkt")


; TODO: Implement, document, and provide these.

;(provide pure/c!r!)
;(provide #/struct-out leaf-r)
;(provide holes-r/c leaf-r/c computation-r/c)
;(provide return!r bind!r)
;(provide run!r! purely!r)

;(provide
;  read-value!r with-fusable-value-reader!r fusing-value-reader!r)
;(provide handler? handle!r with-first-handler!r)
;(provide gensym!r)

; TODO: Implement more effects than this. We have a partial rationale
; and partial plan laid out in notes/todo.txt, and the file
; notes/original-notes.txt contains some earlier notes on the subject.
; These should guide the design and could serve as a starting point
; for the documentation.



; TODO: Implement this. Given a contract that accepts procedure
; values, it should return a contract that wraps a procedure so it
; opens a degree-1 `purely!r` region, calls the original procedure,
; then closes the region (by opening a degree-0 hole in it) before
; returning.
(define/contract (pure/c!r! c)
  (-> contract? contract?)
  c)

(struct-easy "a leaf-r" (leaf-r degree holes-to-value))

; TODO: Implement, document, and provide versions of `computation-r?`
; and `computation-r-degree` that do not satisfy
; `struct-predicate-procedure?` or `struct-accessor-procedure?`.
(struct-easy "a computation-r"
  (computation-r degree unsafe-holes-to-value!r!))

(define/contract (holes-r/c degree)
  (-> exact-nonnegative-integer? contract?)
  (and/c (listof computation-r?)
    (lambda (holes)
      (and (= degree #/length holes)
      #/nextlet holes holes i 0
        (expect holes (cons hole holes) #t
        #/expect hole (computation-r degree _) #f
        #/and (= degree i)
        #/next holes #/add1 i)))))

(define/contract (leaf-r/c degree/c value/c-maybe)
  (-> contract? (maybe/c contract?) contract?)
  (struct/dc leaf-r
    [degree (and/c exact-nonnegative-integer? degree/c)]
    [holes-to-value (degree)
      (mat value/c-maybe (just value/c)
        (pure/c!r! #/-> (holes-r/c degree) value/c)
        (pure/c!r! #/-> (holes-r/c degree) any))]))

(define/contract (computation-r/c degree/c value/c-maybe)
  (-> contract? (maybe/c contract?) contract?)
  (struct/dc computation-r
    [degree (and/c exact-nonnegative-integer? degree/c)]
    [unsafe-holes-to-value!r! (degree)
      (mat value/c-maybe (just value/c)
        (-> (holes-r/c degree) value/c)
        (-> (holes-r/c degree) any))]))

(define/contract (return!r leaf)
  (->i ([leaf (leaf-r/c any/c #/nothing)])
    [_ (leaf) (computation-r/c (=/c #/leaf-r-degree leaf) #/nothing)])
  'TODO)

(define/contract (bind!r prefix leaf-to-suffix)
  (->i
    (
      [prefix (computation-r/c any/c #/nothing)]
      [leaf-to-suffix (prefix)
        (w- d (=/c #/computation-r-degree prefix)
        #/pure/c!r! #/->
          (leaf-r/c d #/nothing)
          (computation-r/c d #/nothing))])
  #/_ (prefix)
    (computation-r/c (=/c #/computation-r-degree prefix)
    #/nothing))
  'TODO)

(define/contract (run!r! computation)
  (->i ([computation (computation-r/c any/c #/nothing)])
  #/_ (computation)
    (leaf-r/c (=/c #/computation-r-degree computation) #/nothing))
  'TODO)

; A degree-N indeterminism effect which opens a degree-N hole in every
; instance of almost any effect that actually uses indeterminism, even
; the ones that have degree N or less, which ostensibly wouldn't
; permit degree-N holes. (The rationale is that all our indeterminism
; effects actually have degree greater than the program could ever
; detect, essentially infinite, but clients are required to discard
; all but a client-specified finite number of the hole effects, using
; the other hole effects only indirectly via a built-in operation like
; this one.)
;
; Almost all effects that are designed to be handled in custom ways
; use indeterminism to look up those handlers, so this will open holes
; in them.
;
; In particular, this opens holes in the following effects, as well as
; the effects which open their holes, the effects which open those
; effects' holes, and so on:
;
;   * `purely!r`
;   * `with-fusable-value-reader!r`
;   * `fusing-value-reader!r`
;   * (TODO: Add to this list as appropriate.)
;
; (The rationale is that each of these effects actually aggregates
; metadata about itself in a dynamically scoped binding that this can
; look up. The metadata contains the hole effects this needs in order
; to open its holes. If there were another effect in progress that did
; not aggregate its information this way, this would attempt to open
; those holes anyway, and that attempt would cause an error.)
;
; The place this ends up after opening holes in all those regions is a
; region that has essentially no custom effects at all, just the
; built-in effects like `purely!r`, `with-fusable-value-reader!r`, and
; `fusing-value-reader!r` themselves.
(define/contract (purely!r degree)
  (->i ([degree exact-nonnegative-integer?])
    [_ (degree) (computation-r/c (=/c degree) #/nothing)])
  'TODO)

; NOTE: We could directly take a second-class approach to effect
; handlers, where we bind not a first-class value but instead a
; function that can return one. However, by doing it this way, we can
; do some fusing computation when a new value is bound, not just when
; a value is retrieved.
;
; (TODO: Implement `opaque-fn`, an encapsulated struct that represents
; functions which cannot be inspected with `dex-struct` but which can
; be fused by specifying a fuse to apply to their return values.)

; A degree-0 indeterminism effect that looks up a dynamic binding
; established by degree-N uses of `with-fusable-value-reader!r` and
; `fusing-value-reader!r`.
(define/contract (read-value!r degree key)
  (-> exact-nonnegative-integer? name?
    (computation-r/c (=/c 0) #/nothing))
  'TODO)

; A degree-N indeterminism effect which makes it so that except within
; its holes, any `read-value!r` effects taking place that specify
; degree N will return a certain value for the given key. If there's
; already a binding of this key, its fuse must be equal by `dex-fuse`
; to the given fuse, and the given value is fused into the existing
; one to get the value seen by `read-value!r`. Otherwise, the new
; binding's fuse is the given fuse, and its value is the given value.
;
; This actually opens a region of degree greater than N, but only N
; degrees of holes can be manually opened. If this call is made in the
; context of another similar effect of degree M greater than N, then
; degree-N-or-greater holes will be opened in this region when they're
; opened in that one. An effect is "similar" for these purposes if
; it's any of the effects `purely!r` can open a hole in. This
; operation's hole effectss behave accordingly so that they can open
; holes in similar effects of ostensibly lower degree.
;
(define/contract (with-fusable-value-reader!r degree key fuse val)
  (->i
    (
      [degree exact-nonnegative-integer?]
      [key name?]
      [fuse fuse?]
      [val any/c])
    [_ (degree) (computation-r/c (=/c degree) #/nothing)])
  'TODO)

; A degree-N indeterminism effect which makes it so that except within
; its holes, any `read-value!r` effects taking place that specify
; degree N will return a certain value for the given key. There must
; already be a binding of this key. The given value is fused into the
; existing one, via the binding's fuse value, to get the value seen by
; `read-value!r`.
;
; This actually opens a region of degree greater than N, but only N
; degrees of holes can be manually opened. If this call is made in the
; context of another similar effect of degree M greater than N, then
; degree-N-or-greater holes will be opened in this region when they're
; opened in that one. An effect is "similar" for these purposes if
; it's any of the effects `purely!r` can open a hole in. This
; operation's hole effectss behave accordingly so that they can open
; holes in similar effects of ostensibly lower degree.
;
(define/contract (fusing-value-reader!r degree key val)
  (->i ([degree exact-nonnegative-integer?] [key name?] [val any/c])
    [_ (degree) (computation-r/c (=/c #/add1 degree) #/nothing)])
  'TODO)



; In order to let programmers expose indeterminism effect handlers
; which can only be consulted as long as they're in scope (i.e.
; impossible to capture as a first-class procedure value), we define a
; system of effect handlers based on storing all the handlers in a big
; `fusing-value-reader!r` fusion under an obscure key.

; TODO: Figure out how to represent handlers so we can verify they
; don't overlap with existing handlers. We probably need a handler
; construction DSL that looks like a pattern-matching branch.
(define/contract (handler? x)
  (-> any/c boolean?)
  'TODO)

(define/contract (handle!r degree custom-computation)
  (->i
    ([degree exact-nonnegative-integer?] [custom-computation any/c])
    [_ (degree) (computation-r/c (=/c degree) #/nothing)])
  'TODO)

(define/contract (with-first-handler!r degree handler)
  (->i ([degree exact-nonnegative-integer?] [handler handler?])
    [_ (degree) (computation-r/c (=/c #/add1 degree) #/nothing)])
  'TODO)


; TODO: Implement and uncomment this.
#;(define/contract gensym!r (computation-r/c (=/c 0) #/just symbol?)
  ; NOTE: Don't implement this as an ambient capability by defining
  ; it as part of the effect system. Just have this return something
  ; that uses `handle!r` to use a gensym operation if one is in the
  ; dynamic scope.
  'TODO)
