# A. Parsing.

## A. 1. Parsing functions and the `Parser s e a` type.

A parsing function is a value of type

```haskell
type Parser s e a = s -> Either e (a, s) 
````

This should be read as: a parsing function for values `a` with error type `e` on streams `s`. Wrapping in a newtype:

```haskell
newtype Parser s e a = Parser (s -> Either e (a, s)) 
    deriving stock Functor
````

To parse, or _decode_ some input, simply:

```haskell
-- | Run the parser on the input and return the results.
parse :: Parser s e a -> s -> Either e (a, s)
parse (Parser p) = p
```

### A. 1. 1. One implication and one design decision.

An immediate implication of the type signature of a parsing function is that it is all-or-nothing: _either_ it throws an error _or_ (exclusive or) it succeeds, returning the pair of the parsed result and the rest of the input.

At this point, we note that `Parser` could be generalized to a transformer by,

```haskell
-- The ParserT monad transformer.
data ParserT m s e a = Parser (s -> m (Either e (a, s)))

-- The Parser monad.
type Parser = ParserT Identity
```

as is done in say, the [Megaparsec library](https://hackage.haskell.org/package/megaparsec). In this library, we explicitly do _not_ make such a generalization; all code is pure (meaning: effect free). This design forces prospective library users to construct the parser and stuff it somewhere, gather the input from the IO layer and apply the parser via `parse`. The expectation is that, for the cases where the input must be consumed incrementally, some scheme using a streaming library can be bolted on top.

### A. 1. 2. A small improvement: the type `Result s e a`.

We make one small improvement by replacing the return type `Either e (a, s)` of a parsing function by the isomorph

```haskell
data Result s e a
    = Error !e      -- ^ Error case.
    | Success a !s  -- ^ Success case.
```

This introduces strictness where laziness is not needed while keeping it in where it is useful. It also removes one layer of indirection in the `Success` case. The disadvantage of this flattening of the return type is that there is no `Monad` or even `Applicative` structure for `Result s e a` so the actual code is a tad hairier.

Since this is an implementation, detail, it is kept hidden from public eye; the user-facing API uses base types like `Either` and `(,)` exclusively. In all the code examples of this document we keep using `Either e (a, s)` as the return type of a parsing function.
