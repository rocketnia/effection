# Effection

[![CI](https://github.com/rocketnia/effection/actions/workflows/ci.yml/badge.svg)](https://github.com/rocketnia/effection/actions/workflows/ci.yml)

Effection is a highly experimental library for managing side effects in Racket. It supports a certain programming style that's almost pure, but which also has the ability to introduce handlers for custom side effects.

Even the act of introducing a handler is a side-effectful act, let alone the act of using the side effect handled by one. Aside from those two little bits of impurity, Effection caters to the same pure functional programming style expected by Interconfection. In particular, Effection isn't particularly designed for programs to make pervasive use of Racket's built-in side effects; most effects in Racket programs that use Effection should be handled by Effection handlers. This allows Effection to control what computations have access to effects.

The most remarkable feature of Effection's side effect system is that the dynamically scoped regions they're observable in can have dynamically scoped holes inside, which for instance can take undesired side effects out of scope for controlled periods of time.

This is all a work in progress. In fact, the only particularly solid ideas have been factored out into Interconfection, a library for using deterministic concurrency to build extensible systems.


## Side effects

Effection's framework for extensible side effects is very tentative at this point, incorporating a few different ideas I'm still experimenting with.


### Motivating use cases

There are a few motivating use cases that drive these experiments:

* It would be nice for impure code to be able to set up "pure regions," for instance to enforce that a function passed in by a client is pure. And likewise, it would be nice for pure code to have "impure regions." Together these make impure code and pure code composable in both directions, making it easier to introduce pieces of pure code or various effectful DSLs without committing the whole codebase to that one concept of effects.

* Although Interconfection's deterministic concurrency is designed for use by pure code, certain patterns I've used with it (in Cene) resemble continuation-passing style. It would be interesting to see if Racket's support for first-class continuations can make these techniques more convenient to express, by implementing an effectful DSL that offers an experience closer to Racket's thread-based concurrency.

Something I find curious about Racket is that while imperative effects seem to be used pretty rarely in the language, there are still lots of places in the language, particularly around the macroexpander, reflective operations, and the output format of printed values, that rely on dynamic scope (Racket's `parameterize`) to propagate information. The way these use dynamic scope is still a side effect, but a relatively tame one: It survives left-to-right reordering in Racket's evaluation strategy, but it doesn't survive leaf-to-root reordering (i.e. laziness or evaluation under lambda).

Along the way of implementing Effection, I've started to explore a middle-of-the-road notion of purity that allows `parameterize` but not mutation. Really it's a necessary feature for effects that set up effect handlers and pure regions, so it's well motivated by the goals of the project.


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

In degree 0 dynamic scope, shadowing is like mutation, whereas this kind of monotonic extension is like writing to an Interconfection monotonic state resource instead.

From a high-level viewpoint, this offers a way for longer-lived call stack frames to behave like _interpreter extensions_ as opposed to behaving like full interpreters.

As a user of this system, you can be confident that if you make monotonic contributions to two separate parts of the dynamic scope at the same time, and if you only take them out of scope at the same time, they will remain together all along the way, even if other parts of the program have access to make contributions to the same two resources. This may relax the demand for monolithic, opaque state managers; complex monotonic stateful components can be expressed in terms of a composition of simpler monotonic stateful components without getting out of sync.

Not only has this monotonic dynamic scope not been implemented yet, even the API is still unclear at this point.


## Overview of the Effection codebase

For now, Effection offers no public modules. The modules it once offered have been moved to Interconfection.

API documentation is maintained in this repo and [hosted at the Racket website](https://docs.racket-lang.org/effection/).

There's some work currently happening in `effection/effects/base` to implement Effection's dynamic scope/effect handler system. These definitions are not entirely documented or exported yet, and there are still several big holes in their implementations.
