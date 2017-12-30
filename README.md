# Effection

Effection is a set of alternate core libraries for Racket, designed around quasi-deterministic functional programming.

Effection code can seamlessly coexist with Racket code, and Effection's modules export most of the same bindings as Racket modules do. The difference is that for an Effection-based program, more things than usual are considered "unsafe."

While programming in the Effection style, most of Racket's usual side effects are considered unsafe. Operations that are otherwise deterministic but may allocate fresh values that are not `eq?` to each other, such as the operations `lambda` and `list`, are considered safe, but to cover up for this, `eq?` itself is considered unsafe. Operations that can raise errors by exhausting the heap or the stack are nevertheless considered safe, and other error-raising operations like `append` and `+` are considered safe as well; to cover up for this, catching errors is considered unsafe.

These policies achieve a kind of purity, but Effection-based programs are not necessarily pure throughout. Effection offers an extensible side effect model, allowing programmers to continue using Racket-style impurity in their programs without compromising their ability to expose a pure interface. This does not mean existing impure Racket code can be used as-is, but it means many of the same idioms can be translated into slightly more verbose Effection equivalents, particularly without inversion of control.

With the addition of restrictions, there comes a need to offer alternatives to certain utilities that unrestricted Racket code has taken for granted. There also comes an opportunity for interfaces that are more elegant on the happy path thanks to narrower notion of what the happy path is. For instance, when the act of comparing values by object identity is considered unsafe, the interface for hash tables may become harder to design, implement, and use... but when the act of performing side effects in an iteration callback is also considered unsafe, the interface for hash tables may at the same time become easier to design, implement, and use.

This brings us to the way the Racket collection for Effection is organized. For any given Racket module like `racket/base`, Effection may offer several modules that can be dropped in as substitutes:

  - `effection/extended/racket/base` - A module that re-exports the bindings of `effection/safe-subset/racket/base`, `effection/reminders/racket/base`, and `effection/replacements-only/racket/base`.
    - `effection/replacements-only/racket/base` - A module of safe alternatives to unsafe utilities in `racket/base`.
    - `effection/strict/racket/base` - A module that re-exports the bindings of `effection/safe-subset/racket/base` and `effection/reminders/racket/base`.
      - `effection/reminders/racket/base` - A module that exports syntaxes that imitate an unsafe subset of `racket/base` but cause informative compile-time errors when they're used.
      - `effection/safe-subset/racket/base` - A module that re-exports only a safe subset of `racket/base`.

Effection may also offer its own modules that are not designed to align with those.

  - `effection` - A module with purpose similar to `racket`: A batteries-inclded language for use in short or informal programs, such as small scripts, demonstrations, and REPL sessions.
    - `effection/base` - A module with purpose similar to `racket/base`: A relatively minimalistic language for use in programs that go to the effort to keep a lean dependency graph, such as libraries.
  - `effection/...` - Various batteries-included modules, which will tend to re-export everything in `effection/.../base` and be re-exported by `effection` but not by `effection/base`.
    - `effection/.../base` - Various minimalistic modules, will tend to be re-exported by `effection` and `effection/base`.
  - `effection/racket-client` - A module providing entrypoints Racket programs can use to execute Effection-based programs and their side effects.

For `effection/...` and `effection/.../base` there, we have a few values of `...` in mind already:

  - `handler` - Modules offering basic support for setting up and using effect handlers.
  - `promise` - Modules offering basic support for monotonic-state-based deterministic concurrency in a style that uses promises for control flow.
  - `order`, `merge`, `fuse`, `table` - Modules offering basic support for doing comparisons, doing orderless merge operations, representing orderless collections, and sorting.

All that being said, here are the modules currently (partially) implemented:

  - `effection/order`
    - `effection/order/base`
