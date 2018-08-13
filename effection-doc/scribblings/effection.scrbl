#lang parendown scribble/manual

@(require #/for-label racket/base)
@(require #/for-label #/only-in racket/contract/base
  -> any/c chaperone-contract? contract?)

@(require #/for-label #/only-in lathe-comforts/maybe
  just maybe? maybe/c nothing)

@(require #/for-label effection/order/base)


@title{Effection}

Effection is a library for managing side effects in Racket. It supports a certain programming style that's almost pure, but which also has the ability to introduce handlers for custom side effects.

The notion of purity Effection uses is chosen deliberately, and it's meant to facilitate commutative extensibility mechanisms by way of quasi-deterministic concurrency.

The notions of @emph{side effect} in Effection are also chosen according to elaborate reasoning, although at this point they're very experimental. The most remarkable feature of Effection's side effects is that the dynamically scoped regions they're observable in can have dynamically scoped holes inside, which for instance can take undesired side effects out of scope for controlled periods of time.

This is all a work in progress. The only pieces of Effection that are fully implemented at this point are some pure utilities that will come in handy for commutatively merging values.

For a more thorough overview of Effection's goals, @hyperlink["https://github.com/rocketnia/effection/blob/master/README.md"]{see the readme}.



@table-of-contents[]



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

All the exports of @tt{effection/order/base} are also exported by @racketmodname[effection/order].


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
  Returns whether the given value is a name. In Effection, a "name" is something like a partial application of comparison by a dex. Any value can be converted to a name using @racket[name-of] if any dex for that value is at hand (and it always converts to the same name regardless of which dex is chosen), and names themselves can be compared using @racket[(dex-name)].
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
  
  The dex's @racket[compare-by-dex] behavior is called as a tail call when comparing the values.
}

@defproc[(name-of-dexable [x valid-dexable?]) name?]{
  Given a well-formed @racket[dexable] value, returns a name the contained value can be compared by.
  
  This is a convenience layer over @racket[name-of].
}


@defproc[(dex-name) dex?]{
  Returns a dex that compares names.
}

@defproc[(dex-dex) dex?]{
  Returns a dex that compares dexes.
  
  All presently existing dexes allow this comparison to be fine-grained enough that it trivializes their equational theory. For instance, @racket[(dex-default (dex-give-up) (dex-give-up))] and @racket[(dex-give-up)] can be distinguished this way despite otherwise having equivalent behavior.
}


@defproc[(dex-give-up) dex?]{
  Returns a dex over an empty domain.
}

@defproc[
  (dex-default
    [dex-for-trying-first dex?]
    [dex-for-trying-second dex?])
  dex?
]{
  Given two dexes, returns a dex over the union of their domains.
  
  For the sake of nontermination, error, and performance concerns, this attempts to compute the result using @racket[dex-for-trying-first] before it moves on to @racket[dex-for-trying-second].
  
  The invocation of @racket[dex-for-trying-second] is a tail call.
  
  When compared by @racket[(dex-dex)], all @tt{dex-default} values are @racket[ordering-eq] if their @racket[dex-for-trying-first] values are and their @racket[dex-for-trying-second] values are.
}

@defproc[
  (dex-by-own-method
    [dexable-get-method (dexableof (-> any/c (maybe/c dex?)))])
  dex?
]{
  Given a dexable function, returns a dex that works by invoking that function with each value to get @racket[(just _dex)] or @racket[(nothing)], verifying that the two @var[dex] values are the same, and then proceeding to tail-call that dex value.
  
  When compared by @racket[(dex-dex)], all @tt{dex-by-own-method} values are @racket[ordering-eq] if their @racket[dexable-get-method] values' dexes and values are.
}

@defproc[(dex-fix [dexable-unwrap (dexableof (-> dex? dex?))]) dex?]{
  Given a dexable function, returns a dex that works by passing itself to the function and then tail-calling the resulting dex.
  
  When compared by @racket[(dex-dex)], all @tt{dex-fix} values are @racket[ordering-eq] if their @racket[dexable-unwrap] values' dexes and values are.
}

@defform[
  (dex-struct-by-field-position struct-id
    [field-position-nat dex-expr]
    ...)
  #:contracts ([dex-expr dex?])
]{
  Returns a dex that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the dexes produced by the @racket[dex-expr] expressions.
  
  Each @racket[field-position-nat] must be a distinct number indicating which field should be checked by the associated dex, and there must be an entry for every field.
  
  For the sake of nontermination, error, and performance concerns, this dex computes by attempting the given dexes in the order they appear in this call. If a dex before the last one determines a non-@racket[ordering-eq] result, the following dexes are only checked to be sure their domains contain the respective field values. Otherwise, the last dex, if any, is attempted as a tail call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[(dex-dex)], all @tt{dex-struct-by-field-position} values are @racket[ordering-eq] if they're for the same structure type descriptor, if they have @racket[field-position-nat] values in the same sequence, and if their @racket[dex-expr] values are @racket[ordering-eq].
}

@defform[
  (dex-struct struct-id dex-expr ...)
  #:contracts ([dex-expr dex?])
]{
  Returns a dex that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the dexes produced by the @racket[dex-expr] expressions.
  
  For the sake of nontermination, error, and performance concerns, this dex computes by attempting the given dexes in the order they appear in this call. The last dex, if any, is attempted as a tail call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[(dex-dex)], each @tt{dex-struct} value is @racket[ordering-eq] to the equivalent @racket[dex-struct-by-field-position] value.
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

@defproc[(dex-cline) dex?]{
  Returns a dex that compares clines.
  
  All presently existing clines allow this comparison to be fine-grained enough that it trivializes their equational theory. For instance, @racket[(cline-default (cline-give-up) (cline-give-up))] and @racket[(cline-give-up)] can be distinguished this way despite otherwise having equivalent behavior.
}


@defproc[(cline-by-dex [dex dex?]) cline?]{
  Returns a cline that compares values by tail-calling the given dex. Since the dex never returns the "candidly precedes" or "candidly follows" results, this cline doesn't either.
  
  When compared by @racket[(dex-cline)], all @tt{cline-by-dex} values are @racket[ordering-eq] if their dexes are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to the original @racket[dex].
}

@defproc[(cline-give-up) cline?]{
  Returns a cline over an empty domain.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to @racket[(dex-give-up)].
}

@defproc[
  (cline-default
    [cline-for-trying-first cline?]
    [cline-for-trying-second cline?])
  cline?
]{
  Given two clines, returns a cline over the union of their domains. The resulting cline’s ascending order consists of the first cline’s ascending order in its domain, followed by the second cline’s ascending order outside the first cline’s domain.
  
  For the sake of nontermination, error, and performance concerns, this attempts to compute the result using @racket[cline-for-trying-first] before it moves on to @racket[cline-for-trying-second].
  
  The invocation of @racket[cline-for-trying-second] is a tail call.
  
  When compared by @racket[(dex-cline)], all @tt{cline-default} values are @racket[ordering-eq] if their @racket[cline-for-trying-first] values are and their @racket[cline-for-trying-second] values are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to the similarly constructed @racket[dex-default].
}

@defproc[
  (cline-by-own-method
    [dexable-get-method (dexableof (-> any/c (maybe/c cline?)))])
  cline?
]{
  Given a dexable function, returns a cline that works by invoking that function with each value to get @racket[(just _cline)] or @racket[(nothing)], verifying that the two @var[cline] values are the same, and then proceeding to tail-call that value.
  
  When compared by @racket[(dex-cline)], all @tt{cline-by-own-method} values are @racket[ordering-eq] if their @racket[dexable-get-method] values' dexes and values are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to another dex only if that dex was obtained the same way from a cline @racket[ordering-eq] to this one.
}

@defproc[
  (cline-fix [dexable-unwrap (dexableof (-> cline? cline?))])
  cline?
]{
  Given a dexable function, returns a cline that works by passing itself to the function and then tail-calling the resulting cline.
  
  When compared by @racket[(dex-cline)], all @tt{cline-fix} values are @racket[ordering-eq] if their @racket[dexable-unwrap] values' dexes and values are.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to another dex only if that dex was obtained the same way from a cline @racket[ordering-eq] to this one.
}

@defform[
  (cline-struct-by-field-position struct-id
    [field-position-nat cline-expr]
    ...)
  #:contracts ([cline-expr cline?])
]{
  Returns a cline that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the clines produced by the @racket[cline-expr] expressions. The comparison is lexicographic, with the most significant comparisons being the @racket[cline-expr] values that appear earliest in this call.
  
  Each @racket[field-position-nat] must be a distinct number indicating which field should be checked by the associated cline, and there must be an entry for every field.
  
  For the sake of nontermination, error, and performance concerns, this cline computes by attempting the given clines in the order they appear in this call. The last cline, if any, is attempted as a tail call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[(dex-cline)], all @tt{cline-struct-by-field-position} values are @racket[ordering-eq] if they're for the same structure type descriptor, if they have @racket[field-position-nat] values in the same sequence, and if their @racket[cline-expr] values are @racket[ordering-eq].
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to the similarly constructed @racket[dex-struct-by-field-position].
}

@defform[
  (cline-struct struct-id cline-expr ...)
  #:contracts ([cline-expr cline?])
]{
  Returns a cline that compares instances of the structure type named by @racket[struct-id], and whose field values can be compared by the clines produced by the @racket[cline-expr] expressions. The comparison is lexicographic, with the most significant comparisons being the @racket[cline-expr] values that appear earliest in this call.
  
  For the sake of nontermination, error, and performance concerns, this cline computes by attempting the given clines in the order they appear in this call. If a cline before the last one determines a non-@racket[ordering-eq] result, the following clines are only checked to be sure their domains contain the respective field values. Otherwise, the last cline, if any, is attempted as a tail call.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[(dex-cline)], each @tt{cline-struct} value is @racket[ordering-eq] to the equivalent @racket[cline-struct-by-field-position] value.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to the similarly constructed @racket[dex-struct].
}


@subsection[#:tag "merges-and-fuses"]{Merges and Fuses}

Effection offers a non-exhaustive but extensive selection of "merges" and "fuses." These are values which can be compared for equality with like values (using @racket[(dex-merge)] and @racket[(dex-fuse)]), and they represent operations of two arguments (invocable using @racket[call-merge] and @racket[call-fuse]).

Merges represent operations that are commutative, associative, and idempotent, or in other words exactly the kind of operation that can operate on a (nonempty and finite) unordered set of inputs.

Fuses represent operations that are commutative and associative (and not necessarily idempotent). A fuse is ideal for operating on a (nonempty and finite) unordered @emph{multiset} of inputs.

The idempotence of a merge operation is such enough that if the two inputs to the merge are @racket[order-eq] by any dex, the result will be @racket[order-eq] to them both by the same dex.


@deftogether[(
  @defproc[(merge? [x any/c]) boolean?]
  @defproc[(fuse? [x any/c]) boolean?]
)]{
  Returns whether the given value is a merge/fuse.
}

@deftogether[(
  @defproc[(call-merge [merge merge?] [a any/c] [b any/c]) maybe?]
  @defproc[(call-fuse [fuse fuse?] [a any/c] [b any/c]) maybe?]
)]{
  Given a merge/fuse and two values, combines those values according to the merge/fuse. The result is @racket[(nothing)] if either value is outside the merge's/fuse's domain. Otherwise, the result is @racket[(just _value)] for some @var[value] that's also in the domain.
  
  For @tt{call-merge}, if there is any dex for which the input values are @racket[ordering-eq], then the result will be @racket[ordering-eq] to them both.
}

@deftogether[(
  @defproc[(dex-merge) dex?]
  @defproc[(dex-fuse) dex?]
)]{
  Returns a dex that compares merges/fuses.
}


@defproc[(merge-by-dex [dex dex?]) merge?]{
  Returns a merge that merges any values that are already @racket[ordering-eq] according the given dex. The result of the merge is @racket[ordering-eq] to both of the inputs.
  
  When compared by @racket[(dex-merge)], all @tt{merge-by-dex} values are @racket[ordering-eq] if their dexes are.
}

@defproc[(fuse-by-merge [merge merge?]) fuse?]{
  Returns a fuse that fuses values by merging them using the given merge.
  
  When compared by @racket[(dex-fuse)], all @tt{fuse-by-merge} values are @racket[ordering-eq] if their merges are.
}

@deftogether[(
  @defproc[
    (merge-by-own-method
      [dexable-get-method (dexableof (-> any/c (maybe/c merge?)))])
    merge?
  ]
  @defproc[
    (fuse-by-own-method
      [dexable-get-method (dexableof (-> any/c (maybe/c fuse?)))])
    fuse?
  ]
)]{
  Given a dexable function, returns a fuse that works by invoking that function with each value to get @racket[(just _method)] or @racket[(nothing)], verifying that the two @var[method] values are the same, and invoking that merge/fuse value to get a result of @racket[(just _result)] or @racket[(nothing)]. If the result is @racket[(just _result)], this does a final check before returning it: It invokes the method-getting function on the @racket[result] to verify that it obtains the same @var[method] value that was obtained from the inputs. This ensures that the operation is associative.
  
  When compared by @racket[(dex-merge)]/@racket[(dex-fuse)], all @tt{merge-by-own-method}/@tt{fuse-by-own-method} values are @racket[ordering-eq] if their @racket[dexable-get-method] values' dexes and values are.
}

@deftogether[(
  @defproc[
    (merge-fix [dexable-unwrap (dexableof (-> merge? merge?))])
    merge?
  ]
  @defproc[
    (fuse-fix [dexable-unwrap (dexableof (-> fuse? fuse?))])
    fuse?
  ]
)]{
  Given a dexable function, returns a merge/fuse that works by passing itself to the function and then tail-calling the resulting merge/fuse.
  
  When compared by @racket[(dex-merge)]/@racket[(dex-fuse)], all @tt{merge-fix}/@tt{fuse-fix} values are @racket[ordering-eq] if their @racket[dexable-unwrap] values' dexes and values are.
}

@deftogether[(
  @defform[
    (merge-struct-by-field-position struct-id
      [field-position-nat field-method-expr]
      ...)
    #:contracts ([field-method-expr merge?])
  ]
  @defform[
    (fuse-struct-by-field-position struct-id
      [field-position-nat fuse-expr]
      ...)
    #:contracts ([field-method-expr fuse?])
  ]
)]{
  Returns a merge/fuse that combines instances of the structure type named by @racket[struct-id], and whose field values can be combined by the merges/fuses produced by the @racket[field-method-expr] expressions.
  
  Each @racket[field-position-nat] must be a distinct number indicating which field should be checked by the associated merge/fuse, and there must be an entry for every field.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[(dex-merge)]/@racket[(dex-fuse)], all @tt{merge-struct-by-field-position}/@tt{fuse-struct-by-field-position} values are @racket[ordering-eq] if they're for the same structure type descriptor, if they have @racket[field-position-nat] values in the same sequence, and if their @racket[field-method-expr] values are @racket[ordering-eq].
}

@deftogether[(
  @defform[
    (merge-struct struct-id field-method-expr ...)
    #:contracts ([field-method-expr merge?])
  ]
  @defform[
    (fuse-struct struct-id field-method-expr ...)
    #:contracts ([field-method-expr fuse?])
  ]
)]{
  Returns a merge/fuse that combines instances of the structure type named by @racket[struct-id], and whose field values can be combined by the merges/fuses produced by the @racket[field-method-expr] expressions.
  
  A struct type is only permitted for @racket[struct-id] if it's fully immutable and has no super-type.
  
  When compared by @racket[(dex-merge)]/@racket[(dex-fuse)], each @tt{merge-struct}/@tt{fuse-struct} value is @racket[ordering-eq] to the equivalent @racket[merge-struct-by-field-position]/@racket[fuse-struct-by-field-position] value.
}


@subsection[#:tag "tables"]{Tables}

Effection's "tables" are similar to Racket hash tables where all the keys are Effection name values. However, tables are encapsulated in such a way that Effection-safe code will always process the table entries in an order-oblivious way. For instance, an Effection table cannot be converted to a list in general. This makes tables useful for representing orderless sets that cross API boundaries, where the API client should not be able to depend on accidental details of the set representation.


@defproc[(table? [x any/c]) boolean?]{
  Returns whether the given value is an Effection table.
}

@defproc[(table-get [key name?] [table table?]) maybe?]{
  Returns the value associated with the given name in the given table, if any.
}

@defproc[(table-empty) table?]{
  Returns an empty table. In this table, the result of @racket[table-get] for any name is @racket[(nothing)].
}

@defproc[
  (table-shadow [key name?] [maybe-val maybe?] [table table?])
  table?
]{
  Returns another table that's just like the given one, except that the @racket[table-get] result for the given name is the given @racket[maybe?] value. That is, this overwrites or removes the value associated with the given name.
}

@defproc[
  (table-map-fuse
    [table table?]
    [fuse fuse?]
    [key-to-operand (-> name? any/c)])
  maybe?
]{
  Given a table, a fuse, and a function, calls that function with each key of the table, and returns a @racket[just] containing the fused value of all the function results. If the table is empty or if any function result is outside the fuse’s domain, this returns @racket[(nothing)] instead.
}

@defproc[(dex-table [dex-val dex?]) dex?]{
  Returns a dex that compares tables, using the given dex to compare each value.
  
  When compared by @racket[(dex-dex)], all @tt{dex-table} values are @racket[ordering-eq] if their @racket[dex-val] values are.
}

@deftogether[(
  @defproc[(merge-table [merge-val merge?]) merge?]
  @defproc[(fuse-table [fuse-val fuse?]) fuse?]
)]{
  Returns a merge/fuse that combines tables by collecting all the nonoverlapping entries and combining the overlapping entries using the given @racket[merge-val]/@racket[fuse-val].
  
  When compared by @racket[(dex-merge)]/@racket[(dex-fuse)], all @tt{merge-table}/@tt{fuse-table} values are @racket[ordering-eq] if their @racket[merge-val]/@racket[fuse-val] values are.
}


@subsection[#:tag "other-data"]{Operations for Other Data Types}

@defmodule[effection/order]

The @tt{effection/order} module exports all the definitions of @racketmodname[effection/order/base] plus the definitions below.

@defproc[(dex-immutable-string) dex?]{
  Returns a dex that compares immutable strings.
}

@defproc[(cline-immutable-string) cline?]{
  Returns a cline that compares immutable strings by their
  @racket[string<?] ordering.
  
  When the dex obtained from this cline using @racket[get-dex-from-cline] is compared by @racket[(dex-dex)], it is @racket[ordering-eq] to @racket[(dex-immutable-string)].
}
