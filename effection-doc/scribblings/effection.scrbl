#lang scribble/manual
@(require (for-label racket/base))


@title{Effection}

Effection is a set of alternate core libraries for Racket, designed around semi-deterministic functional programming.

Effection code can seamlessly coexist with Racket code, and Effection's modules export most of the same bindings as Racket modules do. The difference is that for an Effection-based program, more things than usual are considered "unsafe."

While programming in the Effection style, most of Racket's usual side effects are considered unsafe. Operations that are otherwise deterministic but may allocate fresh values that are not @racket[eq?] to each other, such as the operations @racket[lambda] and @racket[list], are considered safe, but to cover up for this, @racket[eq?] itself is considered unsafe. Operations that can raise errors by exhausting the heap or the stack are nevertheless considered safe, and other error-raising operations like @racket[append] and @racket[+] are considered safe as well; to cover up for this, catching errors is considered unsafe.

These policies achieve a kind of purity, but Effection-based programs are not necessarily pure throughout. Effection offers an extensible side effect model, allowing programmers to continue using Racket-style impurity in their programs without compromising their ability to expose a pure interface. This does not mean existing impure Racket code can be used as-is, but it means many of the same idioms can be translated into slightly more verbose Effection equivalents, particularly without inversion of control.

With the addition of restrictions, there comes a need to offer alternatives to certain utilities that unrestricted Racket code has taken for granted. There also comes an opportunity for interfaces that are more elegant on the happy path thanks to narrower notion of what the happy path is. For instance, when the act of comparing values by object identity is considered unsafe, the interface for hash tables may become harder to design, implement, and use... but when the act of performing side effects in an iteration callback is also considered unsafe, the interface for hash tables may at the same time become easier to design, implement, and use.


@table-of-contents[]


@section[#:tag "example-section"]{Example section}

(TODO: Write documentation as we implement Effection.)
