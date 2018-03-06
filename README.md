# Effection

Effection is a library for Racket that will provides tools for programming in a more pure style than is typical for Racket. Effection is also planned to offer a more extensible system of side effects than Racket's usual side effect model. Effection is a work in progress.


## Purity

Effection's notion of purity is _quasi-determinism_, a notion I'm borrowing from "[Freeze After Writing: Quasi-Deterministic Parallel Programming with LVars](https://www.cs.indiana.edu/~lkuper/papers/lvish-popl14.pdf)." No two runs of a quasi-deterministic computation return different values, but some runs are allowed to encounter errors or nontermination rather than returning a value at all. I've been exploring deterministic concurrency similar to LVars as an extensibility mechanism in my Cene language, and Effection is my attempt to translate most of these building blocks to Racket.

Concurrency goes hand in hand with commutative composition of effects, so a big chunk of the utilities implemented for Effection are primarily useful for a "computation on sets" kind of order-invariant data manipulation. Effection defines a circus of things I call *clines*, *dexes*, *merges*, and *fuses* that help for working with commutative sets of values. As the Effection library is fleshed out, fuses in particular will be essential for specifying how a concurrent state resource should unify incoming writes.

I've built and used some of the state resources for determinitsic concurrency already in my Cene language implementation to power its concurrent macroexpansion phase. Effection is my attempt to port these to Racket and explore them further.


## Side effects

Effection's framework for extensible side effects is very tentative at this point, incorporating a few different ideas I'm still experimenting with.


### Motivating use cases

There are a few motivating use cases that drive these experiments:

* It would be nice for impure code to be able to set up "pure regions," for instance to enforce that a function passed in by a client is pure. And likewise, it would be nice for pure code to have "impure regions." Together these make impure code and pure code composable in both directions, making it easier to introduce pieces of pure code or various effectful DSLs without committing the whole codebase to that one concept of effects.

* Although Effection's deterministic concurrency is designed for use by pure code, certain patterns I've used with it (in Cene) resemble continuation-passing style. It would be interesting to see if Racket's support for first-class continuations can make these techniques more convenient to express, by implementing an effectful DSL that offers an experience closer to Racket's thread-based concurrency.

Something I find curious about Racket is that while imperative effects seem to be used pretty rarely in the language, there are still lots of places in the language, particularly around the macroexpander, reflective operations, and the output format of printed values, that rely on dynamic scope (Racket's `parameterize`) to propagate information. The way these use dynamic scope is still a side effect, but a relatively tame one: It survives left-to-right reordering in Racket's evaluation strategy, but it doesn't survive leaf-to-root reordering (i.e. laziness or evaluation under lambda).

Along the way of implementing Effection, I've started to explore a middle-of-the-road notion of purity that allows `parameterize` but not mutation, and really it's the same one I need for the effects that set up effect handlers and pure regions, so it's not entirely the kind of scope creep it looks like. :)


### Hypersnippet-shaped dynamic extents

For background, there's a concept I've been calling _hypersnippets_, which I originally explored in my Punctaffy library to generalize the way quasiquotation operators interact. A text hypersnippet of degree 1 is a segment of text with a beginning and an end, or in other words a string. A text hypersnippet of degree 2 is a string (the "beginning") with string-shaped holes in it (the "ends"), or in other words an interpolated string. Extrapolating backwards, degree 0 is a text stream; you put a stream-shaped hole in it to get back to a string.

If we draw these intervals on a timeline rather than on a piece of text, then we can classify the dependencies between side effects. Degree-0 effects begin at one moment in time and are active forevermore, which is the kind of side effect we see in imperative mutation. Degree-1 effects begin with one moment and end with another moment, so the `parameterize` operation itself is an example of one of these. Getting to the good stuff, if we want a way to open a _nearly pure region_ which offers only a few effects, one of which is the (lexically scoped) capability to restore access to all the effects we removed, then the operation that sets up that pure region must be at least a degree-2 effect.

Effection actually associates a side effect with two degrees. One is its _usage degree_, the degree of the dynamic extent in which the effect observably interacts with other effects. The other is its _sensitivity degree_, the degree to which it's sensitive to Racket's evaluation strategy. An effect with left-to-right sensitivity has sensitivity degree 0, an effect with merely leaf-to-root sensitivity has 1, and an effect that's totally pure has sensitivity degree 2 or more. (Racket's surface-level syntax and evaluation model both deal with tree structure, a structure made out of degree-1 hypersnippets, so there seems to be no degree-2 structure to be sensitive to.)

For each usage degree, Effection will offer ways to set up and look up dynamically scoped data and effect handlers of that degree, as well as ways to open nearly pure regions of that degree, which wipe out all dynamic scope and effect handlers currently set for their duration.

Effection will also offer a way to reconfigure the current sensitivity level of a computation. This could even be used to enter a region where the sensitivity degree is restricted to 2 or greater, which is truly a pure region since it can't even invoke the effect that configures the sensitivity level again.


### Philosophical rationale for side effects

What does it really mean to support side effects? In Effection's case, a lot of it has to do with interacting in a principled way with existing Racket idioms, but why pick those idioms to support in the first place?

Side effects are often designed and documented in terms of how specific ways they'll act on the world, but in spite of those concrete intentions, ultimately a software program's ability to act on the world goes through the intermediary of an interpreter (sometimes a hardware interpreter). People can bring in time-travelling debuggers and memory editors and modded hardware and such that let them execute code with semantics neither the programmer nor the language designer ever intended.

If an interpreter runs a piece of code, that's sort of like calling it. It might even just *be* calling it, i.e. letting the interpreter's interpreter run the code itself for a while (likely in a more optimized way). So there's a certain likeness between the interpreter of a program and the longest-living systems in the program; it can be reasonable for their roles to blur together to some extent.

If we had a system in the language for making explicit requests of the interpreter, and if some of those requests meant "interpreter, I know you know the entire state of the call stack, so please look up the nearest binding/handler for me"... then we would have all we need for dynamic scope/effect handlers.

The very reference to a "call stack" insinuates there's a reliance on the evaluation strategy. Under Racket's evaluation strategy and its dynamic scope systems (like `parameterize`), the following two procedures can be observed to invoke their callbacks from different call stacks, even though a language with a different evaluation strategy (one with evaluation under lambda) might have the functions invoke their callbacks from the same stack:

```racket
(define (foo1 callback)
  (let ([callback-result (callback)])
    (lambda ()
      callback-result)))

(define (foo2 callback)
  (lambda ()
    (callback)))
```

This is an example of an essential ambiguity in the determination of what systems in a program are "longest-lived." The question of which parts of a program get to have interpreter-like status has an ambiguous answer, which is disambiguated by the real interpreter.

Nevertheless, if the notion of "longer-lived" is precise enough, then it's transitive, and it leads to a kind of hierarchy. When a hierarchy extends very deep, segments of it can be intuitively recognized as logically isolated components and discussed independently of their surroundings, and as these chunks become more precisely bounded, they form a hierarchy of chunks, arriving at the next-higher hypersnippet degree.

By applying the concept of hypersnippets directly to one of the most essential arbiters of "longest-lived," the wall clock timeline itself, we can get a pretty clear account of several notions of side effects all at once. Writing to a mutable store is a degree-0 binding/handler introduction, and reading from a mutable store is a degree-0 binding/handler lookup. The `parameterize` effect is a degree-1 binding introduction.

So we have a clear picture of why we would imbue a longer-lived part of the program with the interpreter-like power to interfere during the execution of a shorter-lived part of a program, and the wall clock timeline and hypersnippets give us precise and familiar notions of "longer-lived" to go by.

This leaves us with deciding what basic kinds of interaction are appropriate to distribute ambiently in a program. Whichever abilities we choose to expose in the core design, the ability to set up custom effect handlers should make it pretty easy to create abstractions around those designs if someone finds a way to improve upon them. This indecision gives us a couple of clear choices for which effects to make ambiently available: The very act of setting up an effect handler and the very act of invoking one.

Even communicating a simple first-class value through an effect handler could make our degree-0 effect handlers work a lot like mutation -- and if we do commit to using mutation, we can indeed simulate just about any other effect by emulating concurrent processes using explicit state transitions. But considering how unruly mutation can be for concurrency concerns, we may want to dial back the kinds of effect handlers we set up in most situations. That brings us to another one of Effection's experiments.


### Dynamically scoped interpreter extensions

In addition to the kind of dynamic scope that can shadow previous dynamically scoped bindings, Effection is planned to offer another kind that monotonically extends what's already there.

In degree 0 dynamic scope, shadowing is like mutation, whereas this kind of monotonic extension is like writing to a monotonic state resource instead. It effectively enables the LVars style of deterministic concurrency.

From a high-level viewpoint, this offers a way for longer-lived call stack frames to behave like _interpreter extensions_ as opposed to behaving like full interpreters.

As a user of this system, you can be confident that if you make monotonic contributions to two separate parts of the dynamic scope at the same time, and if you only take them out of scope at the same time, they will remain together all along the way, even if other parts of the program have access to make contributions to the same two resources. This may relax the demand for monolithic, opaque state managers; complex monotonic stateful components can be expressed in terms of a composition of simpler monotonic stateful components without getting out of sync.

Not only has this monotonic dynamic scope not been implemented yet, even the API is still unclear at this point.


## Effection as a style of programming in Racket

Although Effection encourages the use of quasi-deterministic code, Effection code can coexist alongside other Racket code. We call Racket code "Effection-unsafe" when it uses side effects outside of Effection's own effect model to stray from quasi-determinism, and the Effection library will mark things as simply "unsafe" if they're Effection-unsafe.

There are a few non-obvious rules that determine if Racket code is Effection-safe. Operations that are otherwise deterministic but may allocate fresh values that are not `eq?` to each other, such as the operations `lambda` and `list`, are considered safe, but to cover up for this, `eq?` itself is considered unsafe. As per the usual rules for quasi-determinism, operations that can raise errors by exhausting the heap or the stack are nevertheless considered safe, and other error-raising operations like `append`, `+`, and `error` are considered safe as well; to cover up for this, catching errors at all is considered unsafe.

Somewhat inconveniently, the Effection style considers structure types that inherit from other structure types to be unsafe. In particular, it is Effection-unsafe to invoke their predicates and their field accessors of one structure type that inherits from another, despite the fact that these things are considered Effection-safe for structure types that don't inherit. This way, Effection-safe code can determine for sure that it knows a non-subtyping structure instance up to observational equivalence as observed by Effection-safe code (see [`dex-struct`](https://docs.racket-lang.org/effection/index.html#%28form._%28%28lib._effection%2Forder%2Fbase..rkt%29._dex-struct%29%29)), which is necessary knowledge for certain data representation and concurrency techniques.

These policies achieve a kind of purity, but Effection-based programs are not necessarily pure throughout. Effection offers an extensible side effect model, allowing programmers to continue using Racket-style impurity in their programs without compromising their ability to expose a pure interface. This does not mean existing impure Racket code can be used as-is, but it means many of the same idioms can be translated into slightly more verbose Effection equivalents, particularly without inversion of control.

With the use of a restricted style of programming in Racket, there comes a need to offer alternatives to certain idioms that unrestricted Racket code has taken for granted, as well as an opportunity to simplify some interfaces that no longer need to anticipate clients who use all of Racket's features. For instance, since the act of comparing values by object identity is considered unsafe, the interface for hash tables may become harder to design, implement, and use... but when iteration callbacks can be assumed to be pure, the interface for hash tables may at the same time become somewhat simpler.

At some point these redesigned libraries for Racket may effectively culminate into a Racket `#lang` as a way to assist programmers who want to be sure they're writing Effection-safe code. For now, Effection isn't complete enough to justify that project.


## Overview of the Effection codebase

For now, Effection offers a few modules:

  - `effection/maybe` - A module that re-exports `effectoin/maybe/base` and may someday offer more auxiliary utilities alongside it.
    - `effection/maybe/base` - A module that defines structure type instances for `nothing` and `just`, for places where Haskell's `Maybe` or ML's `option` would be useful.
  - `effection/order` - A module that re-exports `effectoin/order/base` and may someday offer more auxiliary utilities alongside it.
    - `effection/order/base` - A module offering basic support for doing comparisons and doing orderless merge operations.

API documentation is maintained in this repo and [hosted at the Racket website](https://docs.racket-lang.org/effection/). (TODO: At the moment, the blurb in the documentation is a bit stale. That blurb was originally copied from this readme, and this readme has gone through a substantial rewrite.)

There's some work currently happening in `effection/effects/base` to implement Effection's dynamic scope/effect handler system. The definitions in this module are not entirely documented or exported yet, and there are still several big holes in the impelementation.
