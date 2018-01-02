#lang parendown scribble/manual
@(require #/for-label racket/base)
@(require #/for-label effection/order)
@(require #/for-label #/only-in racket/contract/base
  -> ->i any/c contract? flat-contract?)


@title{Effection}

Effection is a set of alternate core libraries for Racket, designed around quasi-deterministic functional programming.

Effection code can seamlessly coexist with Racket code, and Effection's modules export most of the same bindings as Racket modules do. The difference is that for an Effection-based program, more things than usual are considered "unsafe."

While programming in the Effection style, most of Racket's usual side effects are considered unsafe. Operations that are otherwise deterministic but may allocate fresh values that are not @racket[eq?] to each other, such as the operations @racket[lambda] and @racket[list], are considered safe, but to cover up for this, @racket[eq?] itself is considered unsafe. Operations that can raise errors by exhausting the heap or the stack are nevertheless considered safe, and other error-raising operations like @racket[append] and @racket[+] are considered safe as well; to cover up for this, catching errors is considered unsafe.

These policies achieve a kind of purity, but Effection-based programs are not necessarily pure throughout. Effection offers an extensible side effect model, allowing programmers to continue using Racket-style impurity in their programs without compromising their ability to expose a pure interface. This does not mean existing impure Racket code can be used as-is, but it means many of the same idioms can be translated into slightly more verbose Effection equivalents, particularly without inversion of control.

With the addition of restrictions, there comes a need to offer alternatives to certain utilities that unrestricted Racket code has taken for granted. There also comes an opportunity for interfaces that are more elegant on the happy path thanks to narrower notion of what the happy path is. For instance, when the act of comparing values by object identity is considered unsafe, the interface for hash tables may become harder to design, implement, and use... but when the act of performing side effects in an iteration callback is also considered unsafe, the interface for hash tables may at the same time become easier to design, implement, and use.


@table-of-contents[]


@section[#:tag "order"]{Order}

@defmodule[effection/order/base]

A “cline” is based on a total ordering on values in its domain, or in other words a binary relation that is reflexive, transitive, and antisymmetric. Its antisymmetry is as fine-grained as possible: If any two values in a cline’s domain are related by that cline in both directions, only Effection-unsafe code will be able to distinguish the two values.

However, a cline does not merely expose this total ordering. Within the cline’s domain, there may be equivalence classes of values for which every two nonequal values will not have their relative order exposed to Effection-safe code. When Effection-safe code uses @racket[call-cline] to compare two values by a cline, it can get several results:

@itemlist[
    @item{@racket[(list)]: The values are not both in the domain.}
    @item{@racket[(list (ordering-lt))]: The first value candidly precedes the second.}
    @item{@racket[(list _ordering-private-lt)] where @var[ordering-private-lt] is an opaque value that satisfies @racket[ordering-private?]: The first value secretly precedes the second.}
    @item{@racket[(list (ordering-eq))]: The first value is equal to the second.}
    @item{@racket[(list _ordering-private-gt)] where @var[ordering-private-gt] is an opaque value that satisfies @racket[ordering-private?]: The first value secretly follows the second.}
    @item{@racket[(list (ordering-gt))]: The first value candidly follows the second.}
]

The “secretly precedes” and “secretly follows” cases are indistinguishable to Effection-safe code.

A “dex” is like a cline, but it never results in the “candidly precedes” and “candidly follows” cases. Thus, a dex is useful as a kind of equality test.


@; TODO: Put this somewhere better.
@defproc[(maybe/c [c contract?]) contract?]{
  Returns a contract that recognizes a list of zero or one elements, where the one element (if present) matches the given contract.
}


@defstruct*[ordering-lt ()]{
  A struct that represents the result of a comparison where the first value turned out to be candidly strictly less than the second value.
}

@defstruct*[ordering-eq ()]{
  A struct that represents the result of a comparison where the first value turned out to be equal to the second value.
}

@defstruct*[ordering-gt ()]{
  A struct that represents the result of a comparison where the first value turned out to be candidly strictly greater than the second value.
}

@defproc[(ordering-private? [x any/c]) boolean?]{
  Returns whether the given value is an opaque value that represents the result of a comparison where the first value turned out to be secretly strictly less than or secretly strictly greater than the second value.
}

@defproc[(dex-result? [x any/c]) boolean?]{
  Returns whether the given value is a possible result for a dex (something that satisfies either @racket[ordering-eq?] or @racket[ordering-private?]).
}

@defproc[(cline-result? [x any/c]) boolean?]{
  Returns whether the given value is a possible result for a dex (something that satisfies @racket[ordering-lt?], @racket[dex-result?], or @racket[ordering-gt?]).
}


@defproc[(cline? [x any/c]) boolean?]{
  Returns whether the given value is a cline.
}

@defproc[(cline-containing [x any/c]) flat-contract?]{
  Returns a contract that recognizes any cline @var[c] such that @racket[(in-cline? _c x)].
}

@defproc[(in-cline? [cline cline?] [x any/c]) boolean?]{
  Given a cline and a value, returns whether the value belongs to the cline's domain.
}

@defproc[
  (call-cline [cline cline?] [a any/c] [b any/c])
  (maybe/c cline-result?)
]{
  Given a cline and two values, compares those values according to the cline. The result is @racket[(list)] if either value is outside the cline's domain.
}


@defproc[(dex? [x any/c]) boolean?]{
  Returns whether the given value is a dex.
}

@defproc[(dex/c [dex dex?]) flat-contract?]{
  Returns a contract that recognizes any value @var[x] such that @racket[(in-dex? dex _x)].
}

@defproc[(in-dex? [dex dex?] [x any/c]) boolean?]{
  Given a dex and a value, returns whether the value belongs to the dex's domain.
}

@defproc[
  (call-dex [dex dex?] [a any/c] [b any/c])
  (maybe/c dex-result?)
]{
  Given a dex and two values, compares those values according to the dex. The result is @racket[(list)] if either value is outside the cline's domain.
}


@defstruct*[dexable ([dex any/c] [value any/c])]{
  A struct that pairs a value with a dex that it purportedly belongs to. If @racket[dex] actually is a dex and @racket[value] actually does belong to its domain, this is considered well-formed.
}

@defthing[dexable/c contract?]{
  A contract that recognizes a well-formed @racket[dexable].
}

@defproc[(dexableof [c contract?]) contract?]{
  Returns a contract that recognizes a well-formed @racket[dexable] and additionally imposes the given contract on its @racket[dexable-value].
}

@defproc[(dexables-autodex [a dexable/c] [b dexable/c]) boolean?]{
  Returns whether the two given well-formed @racket[dexable] values have the same @racket[dexable-dex] and the same @racket[dexable-value].
}


@defthing[dex-dex dex?]{
  A dex that compares dexes. This is fine-grained enough that it trivializes most dexes' equational theory. (TODO: Give an example.)
}

@defthing[dex-cline dex?]{
  A dex that compares clines. This is fine-grained enough that it trivializes most clines' equational theory; for instance, @racket[(cline-default (cline-give-up) (cline-give-up))] and @racket[(cline-give-up)] can be distinguished this way despite otherwise having equivalent behavior.
}

@defproc[(dex-by-cline [cline cline?]) dex?]{
  Returns a dex that compares values according the given cline. If the cline returns the "candidly precedes" or "candidly follows" results, this dex returns the "secretly precedes" or "secretly follows" results respectively.
}


@defproc[(cline-by-dex [dex dex?]) cline?]{
  Returns a cline that compares values according the given dex. Since the dex never returns the "candidly precedes" or "candidly follows" results, this cline doesn't either.
}

@defthing[cline-give-up cline?]{
  A cline over an empty domain.
}

@defproc[
  (cline-default
    [cline-for-trying-first cline?]
    [cline-for-trying-second cline?])
  cline?
]{
  Given two clines, returns a cline over the union of their domains. The resulting cline’s ascending order consists of the first cline’s ascending order in its domain, followed by the second cline’s ascending order outside the first cline’s domain.
}

@defproc[
  (cline-by-own-method
    [dexable-get-method
      (dexableof
        (->i ([x any/c])
          [result (x) (maybe/c (cline-containing x))]))])
  cline?
]{
  Given a dexable function, returns a cline that works by invoking that function with each value to get @racket[(list _cline)] or @racket[(list)], verifying that the two @var[cline] values are the same, and then proceeding to invoke that value.
}

@defproc[
  (cline-fix [dexable-unwrap (dexableof (-> cline? cline?))])
  cline?
]{
  Given a dexable function, returns a cline that works by passing itself to the function and then invoking the resulting cline.
}
