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
run :: Parser s e a -> s -> Either e (a, s)
run (Parser p) = p
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

