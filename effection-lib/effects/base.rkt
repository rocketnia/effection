#lang parendown racket/base


(require #/only-in racket/contract/base
  -> ->* ->i =/c >=/c and/c any any/c contract? listof or/c struct/c
  struct/dc unconstrained-domain->)
(require #/only-in racket/contract/combinator make-chaperone-contract)
(require #/only-in racket/contract/region define/contract)
(require #/only-in racket/list split-at)

(require #/only-in lathe dissect dissectfn expect mat next nextlet w-)

(require #/only-in effection/maybe/base just nothing maybe/c)
(require #/only-in effection/order/base fuse? name?)

(require "../private/util.rkt")


; TODO: Many of the issues raised in the following comment have begun
; to be addressed now. Update the comment to reflect that. Some of the
; rationales given in this comment should be adapted into
; documentation.
;
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
; We were calling our effects "r" (e.g. `pure!r`) for "read," as
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
;     (run0!h! #/with-effect!h
;       "fa"
;       (handle-lambda state (my-degree-0-effect-struct a b c)
;         (handler-result (string-append state "la") (+ a b c)))
;     (holes-h-and-value (list h0) (list))
;   #/begin (run0!h! #/handle!h 1 0 #/my-degree-0-effect-struct 1 2 3)
;   #/begin (run0!h! #/handle!h 1 0 #/my-degree-0-effect-struct 1 2 3)
;   #/dissect (run0!h! h0) (holes-h-and-value (list) "falala")
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
;     (run0!h! #/with-effect!h
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
;   #/dissect (run0!h! #/handle!h 2 1 #/my-degree-1-effect)
;     (holes-h-and-value (list h0b) "SO")
;   #/dissect (run0!h! #/handle!h 2 1 #/my-degree-1-effect)
;     (holes-h-and-value (list h0c) "SO")
;   #/dissect (run0!h! h1a) (holes-h-and-value (list h0d) "TI")
;   #/dissect (run0!h! h0d) (holes-h-and-value (list h0d) "DO")
;   #/dissect (run0!h! h0c) (holes-h-and-value (list) "LA")
;   #/dissect (run0!h! h0b) (holes-h-and-value (list) "LA")
;   #/dissect (run0!h! #/handle!h 2 1 #/my-degree-1-effect)
;     (holes-h-and-value (list h0e) "SO")
;   #/dissect (run0!h! #/handle!h 2 0 #/my-degree-0-effect)
;     (holes-h-and-value (list) "RE")
;   #/dissect (run0!h! h0d) (holes-h-and-value (list) "LA")
;   #/dissect (run0!h! h0a)
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
;  [-computation-h-sensitivity-degree
;    computation-h-sensitivity-degree]
;  [-computation-h-usage-degree computation-h-usage-degree])
;(provide chaperone-procedure*-run1!h! chaperone-procedure-run1!h!)
;(provide pure/c!h!)
;(provide holes-h/c computation-h/c)
;(provide return!h bind!h)
;(provide run0!h! (struct-out run1-result) run1!h!)
;(provide purely!h with-strategy-sensitivity!h)
;(provide strategy-insensitively!h)

;(provide read-fusable!h init-fusable!h write-fusable!h)
;(provide handler? handle!h with-first-handler!h)
;(provide gensym!h)

; TODO: Implement more effects than this. We have a partial rationale
; and partial plan laid out in notes/todo.txt, and the file
; notes/original-notes.txt contains some earlier notes on the subject.
; These should guide the design and could serve as a starting point
; for the documentation.



(define effects-state-semaphore* (make-semaphore 1))
(define current-sensitivity-degree (make-parameter 0))



(struct-easy "a holes-h-and-value" (holes-h-and-value holes value)
  #:equal)

(struct-easy "a computation-h"
  (computation-h sensitivity-degree usage-degree unsafe-run0!h!)
  #:equal)

; A version of `computation-h?` that does not satisfy
; `struct-predicate-procedure?`.
(define/contract (-computation-h? x)
  (-> any/c boolean?)
  (computation-h? x))

; A version of `computation-h-sensitivity-degree` that does not
; satisfy `struct-accessor-procedure?`.
(define/contract (-computation-h-sensitivity-degree computation)
  (-> -computation-h? exact-nonnegative-integer?)
  (computation-h-sensitivity-degree computation))

; A version of `computation-h-usage-degree` that does not satisfy
; `struct-accessor-procedure?`.
(define/contract (-computation-h-usage-degree computation)
  (-> -computation-h? exact-nonnegative-integer?)
  (computation-h-usage-degree computation))

(define (assert-current-sensitivity-allows degree)
  (unless (<= (current-sensitivity-degree) degree)
    ; TODO: See if we can improve these error messages.
    (mat degree 0
      (error "requires evaluation strategy sensitivity of 0, i.e. a fully linearized control flow")
    #/mat degree 1
      (error "requires evaluation strategy sensitivity of 1, i.e. varying behavior based on the call stack")
      (error "requires slightly more evaluation strategy sensitivity"))))

(struct-easy "a computation-in-progress"
  (computation-in-progress
    holes unsafe-big-hole-start!h! unsafe-big-hole-stop!h! value))

(define/contract
  (chaperone-procedure*-run1!h! proc wrapper-proc . props)
  (->*
    (
      procedure?
      (or/c
        (unconstrained-domain->
          ; TODO: Move the definition of
          ; `chaperone-procedure*-run1!h!` to after `computation-h/c`
          ; and `holes-h/c` if possible.
          (computation-h/c any/c (>=/c 1) #/nothing)
          (-> (holes-h/c any/c 1 any/c) any/c any))
        #f))
    #:rest
    (and/c list? #/lambda (props)
      (and (even? (length props))
      #/let loop ([props props])
      #/mat props (list) #t
      #/dissect props (list* prop-key prop-val props)
      #/and (impersonator-property? prop-key) #/loop props))
    (and procedure? chaperone?))
  (apply chaperone-procedure* proc
    (make-keyword-procedure
    #/lambda (kw-key-list kw-val-list orig-proc . positional-args)
      (define-values (computation holes-and-value-to-wrapped)
        (keyword-apply wrapper-proc kw-key-list kw-val-list orig-proc
          positional-args))
      (dissect computation
        (computation-h sensitivity-degree usage-degree unsafe-run0!h!)
      #/begin (assert-current-sensitivity-allows sensitivity-degree)
      #/dissect (unsafe-run0!h!)
        (computation-in-progress
          (cons (computation-h 0 0 hole0-unsafe-run0!h!) hole1+)
          unsafe-big-hole-start!h! unsafe-big-hole-stop!h! value)
      #/w- wrap-wrap-results
        (lambda (wrap-results)
          (lambda results
            (dissect (hole0-unsafe-run0!h!)
              (computation-in-progress (list)
                unsafe-big-hole-start!h!
                unsafe-big-hole-stop!h!
                value)
            #/apply wrap-results value results)))
      #/mat kw-val-list (list)
        (let ()
          (call-with-values (holes-and-value-to-wrapped hole1+ value)
          #/lambda (wrap-results . wrapped-positional-args)
          #/apply values (wrap-wrap-results wrap-results)
            wrapped-positional-args))
        (let ()
          (call-with-values (holes-and-value-to-wrapped hole1+ value)
          #/lambda
            ( wrap-results wrapped-kw-val-list
              . wrapped-positional-args)
          #/apply values (wrap-wrap-results wrap-results)
            wrapped-kw-val-list
            wrapped-positional-args))))
    props))

(define/contract
  (chaperone-procedure-run1!h! proc wrapper-proc . props)
  (->* (procedure? (or/c procedure? #f))
    #:rest
    (and/c list? #/lambda (props)
      (and (even? (length props))
      #/let loop ([props props])
      #/mat props (list) #t
      #/dissect props (list* prop-key prop-val props)
      #/and (impersonator-property? prop-key) #/loop props))
    (and procedure? chaperone?))
  (apply chaperone-procedure*-run1!h! proc
    (make-keyword-procedure
    #/lambda (kw-key-list kw-val-list orig-proc . positional-args)
      (keyword-apply
        wrapper-proc kw-key-list kw-val-list positional-args))
    props))

; Given a contract that accepts procedure values, this returns a
; contract that wraps a procedure so it calls the original procedure
; inside a degree-1 `purely!h` region.
(define/contract (pure/c!h! c)
  (-> contract? contract?)
  (and/c c
  #/make-chaperone-contract
    #:name 'pure/c!h!
    #:first-order procedure?
    #:projection
    (lambda (b) #/lambda (proc)
      (make-keyword-procedure
      #/lambda (kw-key-list kw-val-list . positional-args)
        (values (purely!h 1) #/lambda (hole1+ value)
          (define (wrap-results hole0-value . results)
            (apply values results))
          (mat kw-val-list (list)
            (apply values wrap-results positional-args)
            (apply values wrap-results kw-val-list
              positional-args)))))))

(define/contract
  (holes-h/c sensitivity-degree usage-start-degree usage-stop-degree)
  (->
    exact-nonnegative-integer?
    exact-nonnegative-integer?
    exact-nonnegative-integer?
    contract?)
  (and/c (listof computation-h?)
    (lambda (holes)
      (and (= (- usage-stop-degree usage-start-degree) #/length holes)
      #/nextlet holes holes i usage-start-degree
        (expect holes (cons hole holes) #t
        #/expect hole
          (computation-h hole-sensitivity-degree hole-usage-degree _)
          #f
        #/and (= hole-sensitivity-degree #/min i sensitivity-degree)
        #/and (= hole-usage-degree i)
        #/next holes #/add1 i)))))

(define/contract
  (computation-h/c sensitivity-degree/c usage-degree/c value/c-maybe)
  (-> contract? contract? (maybe/c contract?) contract?)
  (struct/dc computation-h
    [sensitivity-degree
      (and/c exact-nonnegative-integer? sensitivity-degree/c)]
    [usage-degree (and/c exact-nonnegative-integer? usage-degree/c)]
    [unsafe-run0!h! (sensitivity-degree usage-degree)
      (mat value/c-maybe (just value/c)
        (-> #/struct/c computation-in-progress
          (holes-h/c sensitivity-degree 0 usage-degree)
          (-> void?)
          (-> void?)
          value/c)
        (-> any))]))

(define/contract
  (return!h sensitivity-degree usage-degree holes leaf)
  (->i
    (
      [sensitivity-degree exact-nonnegative-integer?]
      [usage-degree exact-nonnegative-integer?]
      [holes (sensitivity-degree usage-degree)
        (holes-h/c
          sensitivity-degree sensitivity-degree usage-degree)]
      [leaf any/c])
    [_ (sensitivity-degree usage-degree)
      (computation-h/c (=/c sensitivity-degree) (=/c usage-degree)
      #/nothing)])
  (computation-h sensitivity-degree usage-degree #/lambda ()
    (computation-in-progress
      ; NOTE: These hole effects do nothing but what the client has
      ; them do. They don't even verify that they're used in the right
      ; context, e.g. that a hole isn't opened while another hole is
      ; in progress. That's intentional; it helps make sure `return!h`
      ; doesn't add any effects of its own, so that it's a proper
      ; identity element.
      (append
        (build-list sensitivity-degree #/lambda (i)
          (return!h i i (list) #/void))
        holes)
      (lambda () #/void)
      (lambda () #/void)
      leaf)))

(define/contract (bind!h prefix holes-and-leaf-to-suffix)
  (->i
    (
      [prefix (computation-h/c any/c any/c #/nothing)]
      [holes-and-leaf-to-suffix (prefix)
        (w- ds (computation-h-sensitivity-degree prefix)
        #/w- du (computation-h-usage-degree prefix)
        #/-> (holes-h/c ds ds du) any/c
          (computation-h/c (=/c ds) (=/c du) #/nothing))])
    [_ (prefix)
      (w- ds (computation-h-sensitivity-degree prefix)
      #/w- du (computation-h-usage-degree prefix)
      #/computation-h/c (=/c ds) (=/c du) #/nothing)])
  (dissect prefix
    (computation-h sensitivity-degree usage-degree unsafe-run0!h!)
  #/computation-h sensitivity-degree usage-degree #/lambda ()
    (dissect (unsafe-run0!h!)
      (computation-in-progress
        holes
        unsafe-prefix-big-hole-start!h!
        unsafe-prefix-big-hole-stop!h!
        leaf)
    #/begin
      (define-values
        (low-degree-prefix-holes high-degree-prefix-holes)
        (split-at holes sensitivity-degree))
    #/dissect
      (holes-and-leaf-to-suffix high-degree-prefix-holes leaf)
      (computation-h sensitivity-degree usage-degree unsafe-run0!h!)
    
    ; TODO: Since we don't tail-call `unsafe-run0!h!` here, we
    ; essentially have no tail calls at all in this monad. If it turns
    ; out that our low-degree holes and our
    ; `big-hole-start`/`big-hole-stop` procedures don't need to
    ; perform any side effects, we should stop manipulating them here
    ; and just make this a tail call.
    ;
    ; TODO: While we're at it, we should see if we can change the
    ; effect continuations into efficient queues as seen in the "freer
    ; monads" papers. That's probably a separate task, but hey, we
    ; might notice a way to accomplish it earlier when we take care of
    ; this tail call.
    ;
    #/dissect (unsafe-run0!h!)
      (computation-in-progress
        holes
        unsafe-suffix-big-hole-start!h!
        unsafe-suffix-big-hole-stop!h!
        leaf)
    #/begin
      (define-values
        (low-degree-suffix-holes high-degree-suffix-holes)
        (split-at holes sensitivity-degree))
    #/computation-in-progress
      (append
        (list-zip-map low-degree-prefix-holes low-degree-suffix-holes
        #/lambda (prefix-hole suffix-hole)
          (bind!h suffix-hole #/lambda (holes leaf) prefix-hole))
        high-degree-suffix-holes)
      (lambda ()
        (unsafe-suffix-big-hole-start!h!)
        (unsafe-prefix-big-hole-start!h!))
      (lambda ()
        (unsafe-prefix-big-hole-stop!h!)
        (unsafe-suffix-big-hole-stop!h!))
      leaf)))

(define/contract (run0!h! computation)
  (->i ([computation (computation-h/c any/c any/c #/nothing)])
    [_ (computation)
      (w- ds (computation-h-sensitivity-degree computation)
      #/w- du (computation-h-usage-degree computation)
      #/struct/c holes-h-and-value (holes-h/c ds 0 du) any/c)])
  (assert-current-sensitivity-allows 0)
  (dissect computation
    (computation-h start-degree stop-degree unsafe-run0!h!)
  #/dissect (unsafe-run0!h!)
    (computation-in-progress
      holes
      unsafe-suffix-big-hole-start!h!
      unsafe-suffix-big-hole-stop!h!
      value)
  #/holes-h-and-value holes value))

(struct-easy "a run1-result" (run1-result hole0-result body-result)
  #:equal)

(define/contract (run1!h! computation body)
  (->i
    (
      [computation (computation-h/c any/c (>=/c 1) #/nothing)]
      [body (computation)
        (w- ds (computation-h-sensitivity-degree computation)
        #/w- du (computation-h-usage-degree computation)
        #/-> (holes-h/c ds 1 du) any/c any/c)])
    [_ (struct/c run1-result any/c any/c)])
  (assert-current-sensitivity-allows 0)
  (dissect computation
    (computation-h start-degree stop-degree unsafe-run0!h!)
  #/dissect (unsafe-run0!h!)
    (computation-in-progress
      (cons hole0 hole1+)
      unsafe-suffix-big-hole-start!h!
      unsafe-suffix-big-hole-stop!h!
      value)
  #/w- body-result (body hole1+ value)
  #/dissect (run0!h! hole0) (holes-h-and-value (list) hole0-result)
  #/run1-result hole0-result body-result))

(define/contract
  (unsafe-nontrivial-computation-1!h degree unsafe-run0!h!)
  (->i ([degree exact-nonnegative-integer?] [unsafe-run0!h! (-> any)])
    [_ (degree)
      (computation-h/c (=/c #/min 1 degree) (=/c degree) #/nothing)])
  (computation-h (min 1 degree) degree unsafe-run0!h!))

; A degree-N handler effect which opens a degree-N hole in every
; instance of almost any effect that actually uses handler effects,
; even the ones that have degree N or less, which ostensibly wouldn't
; permit degree-N holes. (The rationale is that all our handler
; effects actually have degree greater than the program could ever
; detect, essentially infinite, but clients are required to discard
; all but a client-specified finite number of the hole effects, using
; the other hole effects only indirectly via a built-in operation like
; this one.)
;
; Almost all "custom" effects use indeterminism to look up their
; user-defined event handlers, which is a nontrivial effect. This will
; open holes in those effects.
;
; In particular, this opens holes in the following effects, as well as
; the effects which open their holes, the effects which open those
; effects' holes, and so on:
;
;   * `purely!h`
;   * `with-strategy-sensitivity!h`
;   * `init-fusable!h`
;   * `write-fusable!h`
;   * `with-first-handler!h`
;   * (TODO: Add to this list as appropriate.)
;
; (The rationale is that each of these effects actually aggregates
; metadata about itself in a dynamically scoped binding that this can
; look up. The metadata contains the hole effects this needs to use in
; order to open its holes. If there were another effect in progress
; that did not aggregate its information this way, this would attempt
; to open those holes anyway, and that attempt would cause an error.)
;
; The place this ends up after opening holes in all those regions is a
; region that has essentially no custom effects at all, just the
; built-in effects like `purely!h`, `init-fusable!h`, and
; `write-fusable!h` themselves.
;
(define/contract (purely!h degree)
  (->i ([degree exact-nonnegative-integer?])
    [_ (degree)
      (computation-h/c (=/c #/min 1 degree) (=/c degree) #/nothing)])
  (unsafe-nontrivial-computation-1!h degree #/lambda ()
    'TODO))

; A degree-N handler effect which allows evaluation strategy
; sensitivity of the given degree inside its region.
;
; If the region is degree 0, this effect depends on degree-0
; sensitivity to open the region (so that the expressions inside the
; region aren't evaluated prematurely). The same is true for degree 1.
; This means that in practice, this is useful in the context of
; degree-0- or degree-1-sensitive code, but degree-1-insensitive code
; cannot use this, and hence cannot reintroduce degree 1 sensitivity
; or degree 0 sensitivity this way.
;
(define/contract
  (with-strategy-sensitivity!h
    usage-degree allowed-sensitivity-degree)
  (->i
    (
      [usage-degree exact-nonnegative-integer?]
      [allowed-sensitivity-degree exact-nonnegative-integer?])
    
    ; We require either that there are no degree-1-or-greater holes in
    ; the affected dynamic extent, or that if there are, there is
    ; still degree-1-sensitivity so that their contents are not
    ; evaluated prematurely.
    #:pre (usage-degree allowed-sensitivity-degree)
    (or
      (<= usage-degree 1)
      (<= allowed-sensitivity-degree 1))
    
    [_ (usage-degree)
      (computation-h/c (=/c #/min 1 usage-degree) (=/c usage-degree)
      #/nothing)])
  (unsafe-nontrivial-computation-1!h usage-degree #/lambda ()
    (w- old-sensitivity-degree (current-sensitivity-degree)
    #/w- start
      (lambda ()
        (current-sensitivity-degree allowed-sensitivity-degree))
    #/w- stop
      (lambda ()
        (current-sensitivity-degree old-sensitivity-degree))
    'TODO)))

; We supply a kind of dynamic scope system in the form of the
; `read-fusable!h` family of operations and a kind of effect handler
; system in the form of the `handle!h` family.
;
; Their relationship is something like data and codata. While
; `read-fusable!h` looks up a first-class value and has no other
; effect, `handle!h` immediately interacts with a handler and offers
; no opportinuty to suspend this interaction as a first-class value.
; (More to the point, it offers no opportunity to engage in the
; interaction outside the dynamic extent where its handler is
; defined -- along with all the handlers its handler depends on
; itself!) It's conceivable to implement either one of these behaviors
; in terms of the other, but we supply both.

; TODO: Implement `opaque-fn`, an encapsulated struct that represents
; functions which cannot be inspected with `dex-struct` but which can
; be fused by specifying a fuse to apply to their return values. This
; will be useful for an implementation of `handle!h` where handlers
; are stored in a `read-fusable!h` variable.

; A degree-0 handler effect that looks up a dynamically bound value
; established by uses of `init-fusable!h` and `write-fusable!h` given
; the sensitivity degree and usage degree that was used in those
; operations.
;
; This effect has the same degree of sensitivity to the evaluation
; strategy that the corresponding binding operation does.
;
(define/contract
  (read-fusable!h binding-sensitivity-degree binding-usage-degree key)
  (->i
    (
      [binding-sensitivity-degree exact-nonnegative-integer?]
      [binding-usage-degree exact-nonnegative-integer?]
      [key name?])
    [_ (binding-sensitivity-degree)
      (computation-h/c (=/c binding-sensitivity-degree) (=/c 0)
      #/nothing)])
  'TODO)

; A degree-M-sensitive, usage-degree-N indeterminism effect which
; makes it so that any `read-fusable!h` effects taking place within
; its degree-N dynamic extent that specify sensitivity degree M, usage
; degree N, and the given key will return the given value. If there's
; already a binding of this key, its fuse must be equal by `dex-fuse`
; to the given fuse, and the given value is fused into the existing
; one to get the value seen by `read-fusable!h`. Otherwise, a new
; binding is created whose fuse is the given fuse and whose value is
; the given value.
;
; Only the usage degree N is given, and the evaluation strategy
; sensitivity degree M is derived from that: If the usage degree is 0,
; the sensitivity degree is 0. Otherwise, it's 1.
;
; This actually opens a region of degree greater than N, but only N
; degrees of holes can be manually opened. If this call is made in the
; context of another similar effect of degree greater than N, then
; degree-N-or-greater holes will be opened in this region when they're
; opened in that one. An effect is "similar" for these purposes if
; it's any of the effects `purely!h` can open a hole in. This
; operation's hole effects behave accordingly so that they can open
; holes in similar effects of ostensibly lower degree.
;
(define/contract (init-fusable!h usage-degree key fuse val)
  (->i
    (
      [usage-degree exact-nonnegative-integer?]
      [key name?]
      [fuse fuse?]
      [val any/c])
    [_ (usage-degree)
      (computation-h/c (=/c #/min 1 usage-degree) (=/c usage-degree)
      #/nothing)])
  (unsafe-nontrivial-computation-1!h usage-degree #/lambda ()
    'TODO))

; A degree-M-sensitive, usage-degree-N indeterminism effect which
; makes it so that any `read-fusable!h` effects taking place within
; its degree-N dynamic extent that specify sensitivity degree M, usage
; degree N, and the given key will return a certain value derived from
; the given value. There must already be a binding of this key, and
; the derived value is obtained by fusing the existing binding's value
; with the given value according to the binding's fuse value.
;
; Only the usage degree N is given, and the evaluation strategy
; sensitivity degree M is derived from that: If the usage degree is 0,
; the sensitivity degree is 0. Otherwise, it's 1.
;
; This actually opens a region of degree greater than N, but only N
; degrees of holes can be manually opened. If this call is made in the
; context of another similar effect of degree greater than N, then
; degree-N-or-greater holes will be opened in this region when they're
; opened in that one. An effect is "similar" for these purposes if
; it's any of the effects `purely!h` can open a hole in. This
; operation's hole effects behave accordingly so that they can open
; holes in similar effects of ostensibly lower degree.
;
(define/contract (write-fusable!h usage-degree key val)
  (->i
    (
      [usage-degree exact-nonnegative-integer?]
      [key name?]
      [val any/c])
    [_ (usage-degree)
      (computation-h/c (=/c #/min 1 usage-degree) (=/c usage-degree)
      #/nothing)])
  (unsafe-nontrivial-computation-1!h usage-degree #/lambda ()
    'TODO))



; In order to let programmers expose effect handlers which can only be
; consulted as long as they're in scope (i.e. impossible to capture as
; a first-class procedure value), we define a system of effect
; handlers based on storing all the handlers in a big
; `write-fusable!h` fusion under an obscure key.

; TODO: Figure out how to represent handlers so we can verify they
; don't overlap with existing handlers. We probably need a handler
; construction DSL that looks like a pattern-matching branch, like so:
;
;   (handle-lambda (my-degree-0-effect)
;     (lambda (state)
;       (handler-result (string-append state "re") "RE" #/list)))
;
; The `(my-degree-0-effect)` part is the pattern, and the other
; subform of `handle-lambda` is its function body.
;
(define/contract (handler? x)
  (-> any/c boolean?)
  'TODO)

(define/contract
  (handle!h sensitivity-degree usage-degree custom-computation)
  (->i
    (
      [sensitivity-degree exact-nonnegative-integer?]
      [usage-degree exact-nonnegative-integer?]
      [custom-computation any/c])
    [_ (sensitivity-degree usage-degree)
      (computation-h/c (=/c sensitivity-degree) (=/c usage-degree)
      #/nothing)])
  'TODO)

; TODO: See if we should still have `with-first-handler!h` if we're
; going to add `with-effect!h`. There's probably a difference between
; these in the sense that `with-effect!h` is always degree-0-sensitive
; and this is sometimes only degree-1-sensitive. If we keep this, we
; should think about what kind of value is returned by the handler. Is
; it anything like the values returned by the `with-effect!h`
; handlers?
(define/contract
  (with-first-handler!h sensitivity-degree usage-degree handler)
  (->i
    (
      [sensitivity-degree exact-nonnegative-integer?]
      [usage-degree exact-nonnegative-integer?]
      [handler handler?])
    [_ (sensitivity-degree usage-degree)
      (computation-h/c (=/c sensitivity-degree) (=/c usage-degree)
      #/nothing)])
  (unsafe-nontrivial-computation-1!h usage-degree #/lambda ()
    'TODO))


(define/contract gensym!h
  (computation-h/c (=/c 1) (=/c 0) #/just symbol?)
  ; NOTE: Don't implement this as an ambient capability by defining
  ; it as part of the effect system. Just have this return something
  ; that uses `handle!h` to use a gensym operation if one is in the
  ; dynamic scope.
  (computation-h 1 0 #/lambda ()
    'TODO))
