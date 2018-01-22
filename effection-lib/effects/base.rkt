#lang parendown racket/base


(require #/only-in racket/contract/base
  -> ->i =/c and/c any any/c contract? listof struct/c struct/dc)
(require #/only-in racket/contract/combinator
  blame? make-chaperone-contract)
(require #/only-in racket/contract/region define/contract)

(require #/only-in lathe dissect dissectfn expect mat next nextlet w-)

(require #/only-in effection/maybe/base just nothing maybe/c)
(require #/only-in effection/order/base fuse? name?)

(require "../private/util.rkt")


; TODO:
;
; Whoops. Big whoops. What we've implmented in this file at the moment
; is an attempt at "indeterminism effects," which were supposed to be
; restricted to reading data from the outside world. They actually
; rely on writing data to a concurrent outside world as well as
; reading from it.
;
; In particular, our effects for opening degree-0 holes definitely
; count as writing to a concurrent outside world because we positively
; expect to observe different effect behaviors before and after them,
; where "before" and "after" are based on Racket's left-to-right
; evaluation strategy.
;
; All our degree-greater-than-0 effects count as writing to a
; concurrent outside world too, for the same reason. This could be
; ameliorated by merely having `run!h!` take a thunk to call, call it
; inside the newly established region, and close the region again
; afterward.
;
; However, if we permit degree-1 regions but not degree-0 ones, we are
; just be pushing off the problem to a higher degree where it can be
; more poorly understood. The impurity we've accidentally introduced
; to these "indeterminism effects" is that they now depend on Racket's
; left-to-right evaluation strategy. If we redesign the API to avoid
; having any degree-0 hole-opening effects, then we no longer depend
; on Racket's left-to-right evaluation strategy, but we may still rely
; on a subtler analogue, Racket's root-to-leaf-and-back evaluation
; strategy. In particular, if a variant of Racket were to use an
; evaluation strategy that systematically evaluated certain
; expressions under lambdas or conditionals before calling the lambda
; or testing the condition, then naturally any side effects of those
; expressions which interacted with a concurrent outside world would
; interact at that time. The active call stack (the ledger of ongoing
; degree-1 regions) at that time would be much different than in
; standard Racket, causing different degree-1 effects to be in force,
; so our degree-1 effects do in fact cause the program's behavior to
; depend on the evaluation strategy.
;
; The effect systems are the feature of the Effection library that
; everything revolves around, the main thing (if anything) that makes
; the Effection library a self-contained project with a well-defined
; scope. The baseline purity layer of quasi-determinism is also pretty
; well-defined. If indeterminism effects are too pure an effect model
; to permit the use of dynamically scoped effect handlers, then our
; clearest option is to find an effect model impure enough to let us
; continue.
;
; Nevertheless, it does seem like a shame to take on even a little bit
; of dependence on the evaluation strategy. Maybe we can compromise
; by offering a way to mark out a pure region where evaluation
; strategy dependence is forbidden for degrees lower than a specified
; number, but is still permitted for higher degrees. But what would it
; even look like to forbid degree-1 evaluation strategy sensitivity,
; and what would it even look like to have degree-2 evaluation
; strategy sensitivity? Could we generalize the degree-0 sensitivity
; prohibition strategy by using hypersnippet-shaped syntax (Punctaffy)
; somehow?
;
; Another dimension to this is that perhaps the interpreter isn't the
; only place that can determine the evaluation strategy. Perhaps some
; kind of lexical syntax can make explicit the dependencies between
; its parts, even though it does not make explicit the dependencies
; between any of the function implementation internals it calls at run
; time. But... regular old lexical scope is pretty much that, right?
;
; ~~ more thoughts ~~
;
; We were calling our effects "r" (e.g. `run!r!`) for "read," as
; though we could separate indeterminism effects ("read") from "write
; to a concurrent outside world" effects ("write"), but for now, we're
; calling them "h" for "handler effects." It's a little bit
; self-referential, but it's not quite clear what kind of effects
; they'll need to be yet, and the ability to implement effect handlers
; is their motivating feature, so the name is apt.
;
; To establish a degree-1 dynamic extent that binds a dynamically
; scoped effect of degree 0, we merely need to pass in a state value;
; pass in an effect handler that will receive a state value and return
; a state value and a result value; and receive a final state value as
; the result.
;
;   (struct my-degree-0-effect-struct (a b c))
;   
;   (dissect
;     (run!h! #/with-effect!h
;       "fa"
;       (handle-lambda state (my-degree-0-effect-struct a b c)
;         (handler-result (string-append state "la") (+ a b c)))
;     (holes-h-and-value (list h0) (list))
;   #/begin (run!h! #/handle!h 0 #/my-degree-0-effect-struct 1 2 3)
;   #/begin (run!h! #/handle!h 0 #/my-degree-0-effect-struct 1 2 3)
;   #/dissect (run!h! h0) (holes-h-and-value (list) "falala")
;     ...)
;
; To do the same thing at one degree higher -- a custom degree-1
; effect available in a degree-2 region -- we need to pass in a state
; value; pass in a degree-1 effect handler that will receive a state
; value and return a state value, a result value, and a degree-0
; effect handler that will receive a state value and return a state
; value and a result value; pass in a degree-0 effect handler; and
; receive a final state value as the result.
;
; Er... something like that. It'll be easier to figure it out by
; writing the code:
;
;   ; This pseudocode doesn't use quite the same hypothetical language
;   ; features as the previous pseudocode does. For instance,
;   ; `handle-lambda` no longer takes the state parameter itself;
;   ; instead it returns a list of functions that act as state
;   ; transformers. And the `handler-result` struct contains an extra
;   ; field to contain a list of state transformers to run when holes
;   ; are opened in that hole.
;   
;   (struct my-degree-0-effect ())
;   (struct my-degree-1-effect ())
;   
;   (dissect
;     (run!h! #/with-effect!h
;       
;       ; The initial state value.
;       "fa"
;       
;       ; The immediate result value of this effect
;       "FA"
;       
;       ; Handlers for the holes of this region.
;       (list
;         (lambda (state)
;           (handler-result (list) (string-append state "mi") #/list))
;         (lambda (state)
;           (handler-result (string-append state "ti") "TI" #/list
;             (lambda (state)
;               (handler-result (string-append state "do") "DO"
;               #/list)))))
;       
;       ; Handlers for custom effects.
;       (list
;         (handle-lambda (my-degree-0-effect)
;           (lambda (state)
;             (handler-result (string-append state "re") "RE"
;             #/list)))
;         (handle-lambda (my-degree-1-effect)
;           (lambda (state)
;             (handler-result (string-append state "so") "SO" #/list
;               (lambda (state)
;                 (handler-result (string-append state "la") "LA"
;                 #/list))))))
;     (holes-h-and-value (list h0a h1a) "FA")
;   #/dissect (run!h! #/handle!h 1 #/my-degree-1-effect)
;     (holes-h-and-value (list h0b) "SO")
;   #/dissect (run!h! #/handle!h 1 #/my-degree-1-effect)
;     (holes-h-and-value (list h0c) "SO")
;   #/dissect (run!h! h1a) (holes-h-and-value (list h0d) "TI")
;   #/dissect (run!h! h0d) (holes-h-and-value (list h0d) "DO")
;   #/dissect (run!h! h0c) (holes-h-and-value (list) "LA")
;   #/dissect (run!h! h0b) (holes-h-and-value (list) "LA")
;   #/dissect (run!h! #/handle!h 1 #/my-degree-1-effect)
;     (holes-h-and-value (list h0e) "SO")
;   #/dissect (run!h! #/handle!h 0 #/my-degree-0-effect)
;     (holes-h-and-value (list) "RE")
;   #/dissect (run!h! h0d) (holes-h-and-value (list) "LA")
;   #/dissect (run!h! h0a)
;     (holes-h-and-value (list) "fasosotidolasorelami")
;     ...)
;
; Pretty wild.
;
; Thinking about that compromise again -- the "pure region where
; evaluation strategy dependence is forbidden for degrees lower than a
; specified number"... It seems like forbidding degree-0 evaluation
; strategy sensitivity would mean that we have to regard degree-0
; effects as orderless, like a set. Forbidding degree-1 sensitivity
; would probably make degree-0 effects even more trivial; here are a
; few incomplete rationales as to why:
;
;   * It's ambiguous whether the code should evaluate expressions
;     under lambdas, and Racket in practice does not, so we will not
;     know if we missed any degree-0 effects that could have been
;     observed that way. Even if we could know, any code that relies
;     on that knowledge is degree-1-sensitive.
;
;   * It's ambiguous whether degree-0 effects whose results are
;     ignored should really execute at all. Code that relies on this
;     assumption either way is degree-1-sensitive.
;
;   * Perhaps most decisively of all, once we forbid degree-1
;     sensitivity, it's ambiguous whether the body of a degree-1
;     region should be evaluated before or after figuring out what
;     the boundary of the region means. So if the boundary tries to
;     introduce a degree-1 dynamic extent for a degree-0 effect
;     handler, we might have already evaluated the body without that
;     effect handler installed.
;
; This does leave behind a possible kind of degree-0 effect: The kind
; that reads a value from the outside world, blocking until the answer
; is available. When the result is not needed, it's okay not to run
; the effect at all. When it is needed, some hypothetical evaluation
; strategies would get stuck and error out, and some would continue
; evaluating elsewhere until they discovered a handler for that effect
; which they could invoke to proceed, so in particular, the third
; point above ("body ... evaluated before or after figuring out what
; the boundary of the region means") would not apply. Strangely, we
; seem to have wound up at something almost exactly like Cene's
; `follow-heart` effects, with the possible difference/innovation that
; it might still make sense to have effect handlers which handle
; these.
;
; The behavior of degree-1 effects once we ban degree-1-sensitivity
; seems similarly trivial: We can't be sure the evaluation strategy
; even executes any of the degree-1 effects in a computation before it
; gets what it needs, and we can't have their effects depend on their
; degree-0 or degree-1 juxtaposition on the timeline with respect to
; each other (or with respect to degree-0 effects). Altogether they
; form an orderless set where any of the elements (degree-1 regions)
; may be undiscovered, and each of them has a trivial degree-0 hole
; that may be undiscovered itself. I think what this means is that the
; operation to begin the degree-1 region must do nothing but read from
; the outside world, and the operation to open its degree-0 hole...
; must do nothing but read from the outside world, possibly depending
; on information from the first operation (but that information could
; be propagated equivalently using lexical scope and lambda
; encapsulation anyway). Hmm, do degree-1 effects serve the same
; purpose as degree-0 effects then, when degree-1-sensitivity is
; banned? Is it possible degree-1-insensitivity trivializes degree-2
; effects as well, meaning we really only have to deal with
; degree-0-sensitive code; degree-0-insensitive but degree-1-sensitive
; code; and pure code?
;
; If we really have only those three levels of purity, then we already
; have the impurest and purest covered, and we're not going to find
; any effect handlers for `follow-heart`-style pure side effects. We
; might be done once we add the middle kind of purity.
;
; How about some actionables in this TODO?
;
;   * Implement `with-effect!h` as explored above.
;
;   * See if we can design operations like `with-effect!h` and
;     `purely!h`, but for the case where the pure code is fully pure
;     and the impure code is only degree-1-sensitive.
;
;   * See if we can design operations like `with-effect!h` and
;     `purely!h`, but for the case where the pure code is
;     only-degree-1-sensitive code and the impure code is
;     degree-0-sensitive code.

; TODO: Implement, document, and provide these.

;(provide #/struct-out holes-h-and-value)
;(provide #/rename-out
;  [-computation-h? computation-h?]
;  [-computation-h-degree computation-h-degree])
;(provide pure/c!h!)
;(provide holes-h/c computation-h/c)
;(provide return!h bind!h)
;(provide run!h!)
;(provide purely!h)

;(provide
;  read-value!h with-fusable-value-reader!h fusing-value-reader!h)
;(provide handler? handle!h with-first-handler!h)
;(provide gensym!h)

; TODO: Implement more effects than this. We have a partial rationale
; and partial plan laid out in notes/todo.txt, and the file
; notes/original-notes.txt contains some earlier notes on the subject.
; These should guide the design and could serve as a starting point
; for the documentation.



(struct-easy "a holes-h-and-value" (holes-h-and-value holes value))

(struct-easy "a computation-h"
  (computation-h degree unsafe-run!h!))

; A version of `computation-h?` that does not satisfy
; `struct-predicate-procedure?`.
(define/contract (-computation-h? x)
  (-> any/c boolean?)
  (computation-h? x))

; A version of `computation-h-degree` that does not satisfy
; `struct-accessor-procedure?`.
(define/contract (-computation-h-degree computation)
  (-> -computation-h? exact-nonnegative-integer?)
  (computation-h-degree computation))

(define/contract (before-and-after-projection before after)
  (-> (-> any) (-> any) #/-> blame? #/-> procedure?
    (and/c chaperone? procedure?))
  (lambda (b) #/lambda (proc)
    (chaperone-procedure proc
    #/make-keyword-procedure
    #/lambda (kw-key-list kw-val-list . positional-args)
      (w- state (before)
      #/w- wrap-results
        (lambda results
          (after state)
          (apply values results))
      #/mat kw-val-list (list)
        (apply values wrap-results positional-args)
        (apply values wrap-results kw-val-list positional-args)))))

; Given a contract that accepts procedure values, this returns a
; contract that wraps a procedure so it opens a degree-1 `purely!h`
; region, calls the original procedure, then closes the region (by
; opening a degree-0 hole in it) before returning.
(define/contract (pure/c!h! c)
  (-> contract? contract?)
  (and/c c
  #/make-chaperone-contract
    #:name 'pure/c!h!
    #:first-order procedure?
    #:projection
    (before-and-after-projection
      (lambda () #/run!h! #/purely!h 1)
      (dissectfn (holes-h-and-value (list hole) _) #/run!h! hole))))

(define/contract (holes-h/c degree)
  (-> exact-nonnegative-integer? contract?)
  (and/c (listof computation-h?)
    (lambda (holes)
      (and (= degree #/length holes)
      #/nextlet holes holes i 0
        (expect holes (cons hole holes) #t
        #/expect hole (computation-h degree _) #f
        #/and (= degree i)
        #/next holes #/add1 i)))))

(define/contract (computation-h/c degree/c value/c-maybe)
  (-> contract? (maybe/c contract?) contract?)
  (struct/dc computation-h
    [degree (and/c exact-nonnegative-integer? degree/c)]
    [unsafe-run!h! (degree)
      (mat value/c-maybe (just value/c)
        (-> #/struct/c holes-h-and-value any/c value/c)
        (-> any))]))

(define/contract (return!h degree leaf)
  (->i ([degree exact-nonnegative-integer?] [leaf any/c])
    [_ (degree) (computation-h/c (=/c degree) #/nothing)])
  (computation-h degree #/lambda ()
    (w- holes
      ; NOTE: These hole effects do nothing but return `(void)`. They
      ; don't even verify that they're used in the right context.
      ; That's intentional; it helps make sure `return!h` doesn't add
      ; any effects beyond whatever is already there.
      (build-list degree #/lambda (i)
        (return!h i #/void))
    #/holes-h-and-value holes leaf)))

(define/contract (bind!h prefix holes-and-leaf-to-suffix)
  (->i
    (
      [prefix (computation-h/c any/c #/nothing)]
      [holes-and-leaf-to-suffix (prefix)
        (w- d (computation-h-degree prefix)
        #/pure/c!h! #/->
          (struct/c holes-h-and-value (holes-h/c d) any/c)
          (computation-h/c (=/c d) #/nothing))])
  #/_ (prefix)
    (computation-h/c (=/c #/computation-h-degree prefix) #/nothing))
  (dissect prefix (computation-h degree unsafe-run!h!)
  #/computation-h degree #/lambda ()
    (run!h! #/holes-and-leaf-to-suffix #/unsafe-run!h!)))

(define/contract (run!h! computation)
  (-> (computation-h/c any/c #/nothing) any)
  (dissect computation (computation-h degree unsafe-run!h!)
  #/unsafe-run!h!))

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
;   * `purely!h`
;   * `with-fusable-value-reader!h`
;   * `fusing-value-reader!h`
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
; built-in effects like `purely!h`, `with-fusable-value-reader!h`, and
; `fusing-value-reader!h` themselves.
(define/contract (purely!h degree)
  (->i ([degree exact-nonnegative-integer?])
    [_ (degree) (computation-h/c (=/c degree) #/nothing)])
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
; established by degree-N uses of `with-fusable-value-reader!h` and
; `fusing-value-reader!h`.
(define/contract (read-value!h degree key)
  (-> exact-nonnegative-integer? name?
    (computation-h/c (=/c 0) #/nothing))
  'TODO)

; A degree-N indeterminism effect which makes it so that except within
; its holes, any `read-value!h` effects taking place that specify
; degree N will return a certain value for the given key. If there's
; already a binding of this key, its fuse must be equal by `dex-fuse`
; to the given fuse, and the given value is fused into the existing
; one to get the value seen by `read-value!h`. Otherwise, the new
; binding's fuse is the given fuse, and its value is the given value.
;
; This actually opens a region of degree greater than N, but only N
; degrees of holes can be manually opened. If this call is made in the
; context of another similar effect of degree M greater than N, then
; degree-N-or-greater holes will be opened in this region when they're
; opened in that one. An effect is "similar" for these purposes if
; it's any of the effects `purely!h` can open a hole in. This
; operation's hole effectss behave accordingly so that they can open
; holes in similar effects of ostensibly lower degree.
;
(define/contract (with-fusable-value-reader!h degree key fuse val)
  (->i
    (
      [degree exact-nonnegative-integer?]
      [key name?]
      [fuse fuse?]
      [val any/c])
    [_ (degree) (computation-h/c (=/c degree) #/nothing)])
  'TODO)

; A degree-N indeterminism effect which makes it so that except within
; its holes, any `read-value!h` effects taking place that specify
; degree N will return a certain value for the given key. There must
; already be a binding of this key. The given value is fused into the
; existing one, via the binding's fuse value, to get the value seen by
; `read-value!h`.
;
; This actually opens a region of degree greater than N, but only N
; degrees of holes can be manually opened. If this call is made in the
; context of another similar effect of degree M greater than N, then
; degree-N-or-greater holes will be opened in this region when they're
; opened in that one. An effect is "similar" for these purposes if
; it's any of the effects `purely!h` can open a hole in. This
; operation's hole effectss behave accordingly so that they can open
; holes in similar effects of ostensibly lower degree.
;
(define/contract (fusing-value-reader!h degree key val)
  (->i ([degree exact-nonnegative-integer?] [key name?] [val any/c])
    [_ (degree) (computation-h/c (=/c #/add1 degree) #/nothing)])
  'TODO)



; In order to let programmers expose indeterminism effect handlers
; which can only be consulted as long as they're in scope (i.e.
; impossible to capture as a first-class procedure value), we define a
; system of effect handlers based on storing all the handlers in a big
; `fusing-value-reader!h` fusion under an obscure key.

; TODO: Figure out how to represent handlers so we can verify they
; don't overlap with existing handlers. We probably need a handler
; construction DSL that looks like a pattern-matching branch.
(define/contract (handler? x)
  (-> any/c boolean?)
  'TODO)

(define/contract (handle!h degree custom-computation)
  (->i
    ([degree exact-nonnegative-integer?] [custom-computation any/c])
    [_ (degree) (computation-h/c (=/c degree) #/nothing)])
  'TODO)

(define/contract (with-first-handler!h degree handler)
  (->i ([degree exact-nonnegative-integer?] [handler handler?])
    [_ (degree) (computation-h/c (=/c #/add1 degree) #/nothing)])
  'TODO)


; TODO: Implement and uncomment this.
#;(define/contract gensym!h (computation-h/c (=/c 0) #/just symbol?)
  ; NOTE: Don't implement this as an ambient capability by defining
  ; it as part of the effect system. Just have this return something
  ; that uses `handle!h` to use a gensym operation if one is in the
  ; dynamic scope.
  'TODO)
