#lang parendown scribble/manual
@(require #/for-label racket/base)
@(require #/for-label effection/order/base)
@(require #/for-label effection/maybe/base)
@(require #/for-label #/only-in racket/contract/base
  -> any/c chaperone-contract? contract?)


@title{Effection}

Effection is a set of alternate core libraries for Racket, designed around quasi-deterministic functional programming.

Effection code can seamlessly coexist with Racket code, and Effection's modules export most of the same bindings as Racket modules do. The difference is that for an Effection-based program, more things than usual are considered "unsafe."

While programming in the Effection style, most of Racket's usual side effects are considered unsafe. Operations that are otherwise deterministic but may allocate fresh values that are not @racket[eq?] to each other, such as the operations @racket[lambda] and @racket[list], are considered safe, but to cover up for this, @racket[eq?] itself is considered unsafe. Operations that can raise errors by exhausting the heap or the stack are nevertheless considered safe, and other error-raising operations like @racket[append] and @racket[+] are considered safe as well; to cover up for this, catching errors is considered unsafe.

Somewhat inconveniently, the Effection style considers structure types that inherit from other structure types to be unsafe. In particular, it is Effection-unsafe to invoke their predicates and their field accessors, despite the fact that it is Effection-safe to invoke predicates and field accessors of other structure types. This way, Effection-safe code can actually determine that it knows a non-subtyping structure instance up to (Effection-safe) observational equivalence (see @racket[dex-struct]), knowledge which enables certain data representation and concurrency techniques.

These policies achieve a kind of purity, but Effection-based programs are not necessarily pure throughout. Effection offers an extensible side effect model, allowing programmers to continue using Racket-style impurity in their programs without compromising their ability to expose a pure interface. This does not mean existing impure Racket code can be used as-is, but it means many of the same idioms can be translated into slightly more verbose Effection equivalents, particularly without inversion of control.

With the addition of restrictions, there comes a need to offer alternatives to certain utilities that unrestricted Racket code has taken for granted. There also comes an opportunity for interfaces that are more elegant on the happy path thanks to narrower notion of what the happy path is. For instance, when the act of comparing values by object identity is considered unsafe, the interface for hash tables may become harder to design, implement, and use... but when the act of performing side effects in an iteration callback is also considered unsafe, the interface for hash tables may at the same time become easier to design, implement, and use.


@table-of-contents[]



@section[#:tag "maybe"]{Maybe}

@defmodule[effection/maybe/base]

Maybe values are a way to encode optional data. Using maybe values can simplify some interfaces that would otherwise use run time errors or special-cased sentinel values like @racket[#f].


@defstruct*[nothing ()]{
  A maybe value that does not contain a value.
  
  Every two @tt{nothing} values are @racket[equal?].
}

@defstruct*[just ([value any/c])]{
  A maybe value that does contain a value.
  
  Two @tt{just} values are @racket[equal?] if they contain @racket[equal?] values.
}

@defproc[(maybe? [x any/c]) boolean?]{
  Returns whether the given value is a maybe value. That is, it checks that the value is either a @racket[nothing] value or a @racket[just] value.
}

@defproc[(maybe/c [c chaperone-contract?]) chaperone-contract?]{
  Returns a chaperone contract that recognizes a maybe value where the contained value, if any, abides by the given chaperone contract.
}



@section[#:tag "order"]{Order}

@defmodule[effection/order/base]

A “cline” is based on a total ordering on values in its domain, or in other words a binary relation that is reflexive, transitive, and antisymmetric. Its antisymmetry is as fine-grained as possible: If any two values in a cline’s domain are related by that cline in both directions, only Effection-unsafe code will be able to distinguish the two values.

However, a cline does not merely expose this total ordering. Within the cline’s domain, there may be equivalence classes of values for which every two nonequal values will not have their relative order exposed to Effection-safe code. When Effection-safe code uses @racket[compare-by-cline] to compare two values by a cline, it can get several results:

@itemlist[
    @item{@racket[(nothing)]: The values are not both in the domain.}
    @item{@racket[(just (ordering-lt))]: The first value candidly precedes the second.}
    @item{@racket[(just (make-ordering-private-lt))]: The first value secretly precedes the second.}
    @item{@racket[(just (ordering-eq))]: The first value is equal to the second.}
    @item{@racket[(just (make-ordering-private-gt))]: The first value secretly follows the second.}
    @item{@racket[(just (ordering-gt))]: The first value candidly follows the second.}
]

The “secretly precedes” and “secretly follows” cases are indistinguishable to Effection-safe code.

A “dex” is like a cline, but it never results in the “candidly precedes” and “candidly follows” cases. Thus, a dex is useful as a kind of equality test.


@subsection[#:tag "orderings"]{Orderings}

@defstruct*[ordering-lt ()]{
  A struct that represents the result of a comparison where the first value turned out to be candidly strictly less than the second value.
  
  For the purposes of Effection-unsafe Racket code, every two @tt{ordering-lt} values are @racket[equal?].
}

@defstruct*[ordering-eq ()]{
  A struct that represents the result of a comparison where the first value turned out to be equal to the second value.
  
  For the purposes of Effection-unsafe Racket code, every two @tt{ordering-eq} values are @racket[equal?].
}

@defstruct*[ordering-gt ()]{
  A struct that represents the result of a comparison where the first value turned out to be candidly strictly greater than the second value.
  
  For the purposes of Effection-unsafe Racket code, every two @tt{ordering-gt} values are @racket[equal?].
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

@defproc[(make-ordering-private-lt) ordering-private?]{
  Returns a value that represents the result of a comparison where the first value turned out to be secretly strictly less than the second value.
}

@defproc[(make-ordering-private-gt) ordering-private?]{
  Returns a value that represents the result of a comparison where the first value turned out to be secretly strictly greater than the second value.
}


@subsection[#:tag "dexes"]{Names, Dexes, and Dexables}

@defproc[(name? [x any/c]) boolean?]{
  Returns whether the given value is a name. In Effection, a "name" is something like a partial application of comparison by a dex. Any value can be converted to a name using @racket[name-of] if any dex for that value is at hand (and it always converts to the same name regardless of which dex is chosen), and names themselves can be compared using @racket[dex-name].
}


@defproc[(dex? [x any/c]) boolean?]{
  Returns whether the given value is a dex.
}

@defproc[(in-dex? [dex dex?] [x any/c]) boolean?]{
  Given a dex and a value, returns whether the value belongs to the dex's domain.
}

@defproc[(name-of [dex dex?] [x any/c]) (maybe/c name?)]{
  Given a dex and a value, returns a @racket[just] of a name that the value can be compared by, if the value belongs to the dex's domain; otherwise returns a @racket[nothing].
}

@defproc[
  (compare-by-dex [dex dex?] [a any/c] [b any/c])
  (maybe/c dex-result?)
]{
  Given a dex and two values, compares those values according to the dex. The result is @racket[(nothing)] if either value is outside the dex's domain.
}


@defstruct*[dexable ([dex any/c] [value any/c])]{
  A struct that pairs a value with a dex that it purportedly belongs to. If @racket[dex] actually is a dex and @racket[value] actually does belong to its domain, this is considered well-formed.
}

@defproc[(valid-dexable? [x any/c]) boolean?]{
  Returns whether the given value is a well-formed @racket[dexable].
}

@defproc[(dexableof [c contract?]) contract?]{
  Returns a contract that recognizes a well-formed @racket[dexable] and additionally imposes the given contract on its @racket[dexable-value].
}

@defproc[(compare-dexables [a valid-dexable?] [b valid-dexable?]) (maybe/c dex-result?)]{
  Compares the two given well-formed @racket[dexable] values to see if they have the same @racket[dexable-dex] and the same @racket[dexable-value]. If they have the same dex, this returns a @racket[just] of a @racket[dex-result?]; otherwise, this returns @racket[(nothing)].
}

@defproc[(name-of-dexable [x valid-dexable?]) name?]{
  Given a well-formed @racket[dexable] value, returns a name the contained value can be compared by.
  
  This is a convenience layer over @racket[name-of].
}


@defthing[dex-name dex?]{
  A dex that compares names.
}

@defthing[dex-dex dex?]{
  A dex that compares dexes.
  
  All presently existing dexes allow this comparison to be fine-grained enough that it trivializes their equational theory. For instance, @racket[(dex-default (dex-give-up) (dex-give-up))] and @racket[(dex-give-up)] can be distinguished this way despite otherwise having equivalent behavior.
}


@defthing[dex-give-up dex?]{
  A dex over an empty domain.
}

@defproc[
  (dex-default
    [dex-for-trying-first dex?]
    [dex-for-trying-second dex?])
  dex?
]{
  Given two dexes, returns a dex over the union of their domains.
  
  For the sake of nontermination, error, and performance concerns, this attempts to compute the result using @racket[dex-for-trying-first] before it moves on to @racket[dex-for-trying-second].
  
  When compared by @racket[dex-dex], all @tt{dex-default} values are @racket[ordering-eq] if their @racket[dex-for-trying-first] values are and their @racket[dex-for-trying-second] values are.
}

@defproc[
  (dex-by-own-method
    [dexable-get-method (dexableof (-> any/c (maybe/c dex?)))])
  dex?
]{
  Given a dexable function, returns a dex that works by invoking that function with each value to get @racket[(just _dex)] or @racket[(nothing)], verifying that the two @var[dex] values are the same, and then proceeding to invoke that value.
  
  When compared by @racket[dex-dex], all @tt{dex-by-own-method} values are @racket[ordering-eq] if their @racket[dexable-get-method] values' dexes and values are.
}

@defproc[(dex-fix [dexable-unwrap (dexableof (-> dex? dex?))]) dex?]{
  Given a dexable function, returns a dex that works by passing itself to the function and then invoking the resulting dex.
  
  When compared by @racket[dex-dex], all @tt{dex-fix} values are @racket[ordering-eq] if their @racket[dexable-unwrap] values' dexes and values are.
}

@defform[
  (dex-struct-by-field-position struct-id
    [field-position-nat dex-expr]
    ...)
  #:contracts ([dex-expr dex?])
]{
  Returns a dex that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the dexes produced by the @racket[dex-expr] expressions.
  
  Each @racket[field-position-nat] must be a distinct number indicating which field should be checked by the associated dex, and there must be an entry for every field.
  
  For the sake of nontermination, error, and performance concerns, this dex computes by attempting the given dexes in the order they appear in this call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[dex-dex], all @tt{dex-struct-by-field-position} values are @racket[ordering-eq] if they're for the same structure type descriptor, if they have @racket[field-position-nat] values in the same sequence, and if their @racket[dex-expr] values are @racket[ordering-eq].
}

@defform[
  (dex-struct struct-id dex-expr ...)
  #:contracts ([dex-expr dex?])
]{
  Returns a dex that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the dexes produced by the @racket[dex-expr] expressions.
  
  For the sake of nontermination, error, and performance concerns, this dex computes by attempting the given dexes in the order they appear in this call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[dex-dex], each @tt{dex-struct} value is @racket[ordering-eq] to the equivalent @racket[dex-struct-by-field-position] value.
}


@subsection[#:tag "clines"]{Clines}

@defproc[(cline? [x any/c]) boolean?]{
  Returns whether the given value is a cline.
}

@defproc[(get-dex-from-cline [cline cline?]) dex?]{
  Given a cline, returns a dex over the same domain.
}

@defproc[(in-cline? [cline cline?] [x any/c]) boolean?]{
  Given a cline and a value, returns whether the value belongs to the cline's domain.
}

@defproc[
  (compare-by-cline [cline cline?] [a any/c] [b any/c])
  (maybe/c cline-result?)
]{
  Given a cline and two values, compares those values according to the cline. The result is @racket[(nothing)] if either value is outside the cline's domain.
}

@defthing[dex-cline dex?]{
  A dex that compares clines.
  
  All presently existing clines allow this comparison to be fine-grained enough that it trivializes their equational theory. For instance, @racket[(cline-default (cline-give-up) (cline-give-up))] and @racket[(cline-give-up)] can be distinguished this way despite otherwise having equivalent behavior.
}


@defproc[(cline-by-dex [dex dex?]) cline?]{
  Returns a cline that compares values according the given dex. Since the dex never returns the "candidly precedes" or "candidly follows" results, this cline doesn't either.
  
  When compared by @racket[dex-cline], all @tt{cline-by-dex} values are @racket[ordering-eq] if their dexes are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[dex-dex], it is @racket[ordering-eq] to the original @racket[dex].
}

@defthing[cline-give-up cline?]{
  A cline over an empty domain.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[dex-dex], it is @racket[ordering-eq] to @racket[dex-give-up].
}

@defproc[
  (cline-default
    [cline-for-trying-first cline?]
    [cline-for-trying-second cline?])
  cline?
]{
  Given two clines, returns a cline over the union of their domains. The resulting cline’s ascending order consists of the first cline’s ascending order in its domain, followed by the second cline’s ascending order outside the first cline’s domain.
  
  For the sake of nontermination, error, and performance concerns, this attempts to compute the result using @racket[cline-for-trying-first] before it moves on to @racket[cline-for-trying-second].
  
  When compared by @racket[dex-cline], all @tt{cline-default} values are @racket[ordering-eq] if their @racket[cline-for-trying-first] values are and their @racket[cline-for-trying-second] values are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[dex-dex], it is @racket[ordering-eq] to the similarly constructed @racket[dex-default].
}

@defproc[
  (cline-by-own-method
    [dexable-get-method (dexableof (-> any/c (maybe/c cline?)))])
  cline?
]{
  Given a dexable function, returns a cline that works by invoking that function with each value to get @racket[(just _cline)] or @racket[(nothing)], verifying that the two @var[cline] values are the same, and then proceeding to invoke that value.
  
  When compared by @racket[dex-cline], all @tt{cline-by-own-method} values are @racket[ordering-eq] if their @racket[dexable-get-method] values' dexes and values are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[dex-dex], it is @racket[ordering-eq] to another dex only if that dex was obtained the same way from a cline @racket[ordering-eq] to this one.
}

@defproc[
  (cline-fix [dexable-unwrap (dexableof (-> cline? cline?))])
  cline?
]{
  Given a dexable function, returns a cline that works by passing itself to the function and then invoking the resulting cline.
  
  When compared by @racket[dex-cline], all @tt{cline-fix} values are @racket[ordering-eq] if their @racket[dexable-unwrap] values' dexes and values are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[dex-dex], it is @racket[ordering-eq] to another dex only if that dex was obtained the same way from a cline @racket[ordering-eq] to this one.
}

@defform[
  (cline-struct-by-field-position struct-id
    [field-position-nat cline-expr]
    ...)
  #:contracts ([cline-expr cline?])
]{
  Returns a cline that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the clines produced by the @racket[cline-expr] expressions. The comparison is lexicographic, with the most significant comparisons being the @racket[cline-expr] values that appear earliest in this call.
  
  Each @racket[field-position-nat] must be a distinct number indicating which field should be checked by the associated cline, and there must be an entry for every field.
  
  For the sake of nontermination, error, and performance concerns, this cline computes by attempting the given clines in the order they appear in this call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[dex-cline], all @tt{cline-struct-by-field-position} values are @racket[ordering-eq] if they're for the same structure type descriptor, if they have @racket[field-position-nat] values in the same sequence, and if their @racket[cline-expr] values are @racket[ordering-eq].
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[dex-dex], it is @racket[ordering-eq] to the similarly constructed @racket[dex-struct-by-field-position].
}

@defform[
  (cline-struct struct-id cline-expr ...)
  #:contracts ([cline-expr cline?])
]{
  Returns a cline that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the clines produced by the @racket[cline-expr] expressions. The comparison is lexicographic, with the most significant comparisons being the @racket[cline-expr] values that appear earliest in this call.
  
  For the sake of nontermination, error, and performance concerns, this cline computes by attempting the given clines in the order they appear in this call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[dex-cline], each @racket[cline-struct] value is @racket[ordering-eq] to the equivalent @racket[cline-struct-by-field-position] value.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[dex-dex], it is @racket[ordering-eq] to the similarly constructed @racket[dex-struct].
}
