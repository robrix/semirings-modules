# Semirings & (left) R-modules

This package provides Haskell typeclasses for semirings (with & without additive & multiplicative identities) & (left) R-modules, plus a variety of useful instances.

## Semirings

`Semiring`s extend the notion of a `Semigroup` with a second binary operator, `><`. Intuitively, `<>` is to addition as `><` is to multiplication, and several of the useful properties of `+` and `*` apply:

- `><` has higher precedence than `<>`: `a >< b <> c` is the same as `(a >< b) <> c`, not `a >< (b <> c)`.
- `<>` and `><` are both associative: `a <> b <> c` is the same as `a <> (b <> c)` is the same as `(a <> b) <> c`. Associative operators are trivially parallelizable, making this quite a useful property.
- Unlike in ordinary semigroups, `<>` is expected to be commutative: `a <> b` is the same as `b <> a`.
- `><` distributes over `<>`: `a >< (b <> c)` is the same as `a >< b <> a >< c`.

In this presentation, `Semiring`s are not required to have additive or multiplicative identities. They _can_ have them, however, by defining `Monoid` and `Unital` instances, respectively. If they do, there are some extra properties:

- `zero` (a synonym for the `Monoid` method `mempty`) is the additive identity: `zero <> a` is the same as `a <> zero` is the same as `a`.
- `zero` is the multiplicative absorbing element, or annihilator: `zero >< a` is the same as `a >< zero` is the same as `zero`.
- `one` is the multiplicative identity: `one >< a` is the same as `a >< one` is the same as `a`.

- [Read the docs][docs].

[docs]: http://antitypical.com/semirings-modules/
