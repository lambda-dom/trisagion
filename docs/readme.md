# A. Parsing.

## A. 1. Parsing functions and the `Parser s e a` type.

A _parsing function_ is a value of type,

```haskell
type Parser s e a = s -> e :+: (a, s) 
````

where `:+:` is just a type operator alias for `Either` to make signatures look prettier. It should be read as: a parsing function for values `a` with error type `e` on input streams `s`. Wrapping in a newtype:

```haskell
newtype Parser s e a = Parser (s -> e :+: (a, s))
    deriving stock Functor
````

To _parse_ some input, simply:

```haskell
-- | Run the parser on the input and return the results.
parse :: Parser s e a -> s -> e :+: (a, s)
parse (Parser p) = p
```

An immediate implication of the type signature of a parsing function is that it is all-or-nothing: _either_ it throws an error _or_ (exclusive or) it succeeds, returning the pair of the parsed result and the rest of the input.

At this point, we note that `Parser` could be generalized to a transformer by,

```haskell
-- The ParserT monad transformer.
data ParserT m s e a = Parser (s -> m (e :+: (a, s)))

-- The Parser monad.
type Parser = ParserT Identity
```

as is done in say, the [Megaparsec library](https://hackage.haskell.org/package/megaparsec). In this library, we explicitly do _not_ make such a generalization; all code is pure (meaning: effect free). This design forces library users to construct the parser and stuff it somewhere, gather the input from the IO layer and apply the parser via `parse`. For the cases where the input must be consumed incrementally, some scheme using a streaming library can be bolted on top.

## A. 2. A small improvement: the type `Result s e a`.

We make one small improvement by replacing the return type `e :+: (a, s)` of a parsing function by the isomorph

```haskell
data Result s e a
    = Error !e      -- ^ Error case.
    | Success a !s  -- ^ Success case.
```

This introduces strictness where laziness is not needed while keeping it in where it is useful. It also removes one layer of indirection in the `Success` case. The disadvantage of this flattening of the return type is that there is no `Monad`, or even an `Applicative`, instance for `Result s e a` so the actual code is a tad hairier.

Since this is an implementation detail, it is kept mostly private like a closet skeleton or an ugly vice, and the user-facing API uses base types like `Either` and `(,)` as much as possible. To simplify the exposition, in the rest of this document we keep using `Either e (a, s)` as the return type of a parsing function.

## A. 3. Basic instances.

The type constructor `Parser s e a` is a functor in `a` and this is stock derivable. Other instances provide extra structure. The `Bifunctor` instance provides functoriality in the error type and is the basic error handling mechanism. The `Applicative` instance allows to construct a parser for `(a, b)` from parsers for `a` and `b`, or to construct a parser for product types from parsers for the fields. Finally, the `Monad` instance allows us to feed the result of a parser to another.

It should be noted that none of these instances require constraints on the stream type `s` or the error type `e`.

### A. 3. 1. The `Bifunctor` instance.

Functoriality in the error type is given by the `Bifunctor` instance.

```haskell
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap f g p = Parser $ \ s -> bimap f (first g) $ parse p s
```

Bifunctoriality provides the basic way to unify error types. If we have parsers `p_i :: Parser s e_i a` and a cospan `f_i :: e_i -> e`, then we have a cospan `first f_i :: Parser s e_i a -> Parser s e a`. The choice of `e` is left to the user, but there is a canonical, minimal choice by taking the coproduct of all `e_i`. Unfortunately, dealing with arbitrary coproducts in Haskell is clunky, even if we reached for any of the innumerable packages on hackage offering extensible sum types [^1].

[^1]: For just one example among many, see [sop-core](https://hackage.haskell.org/package/sop-core).

### A. 3. 2. The `Applicative` instance.

We can endow `Parser s e a` with further structure, starting with the `Applicative` instance.

```haskell
instance Applicative (Parser s e) where
    pure :: a -> Parser s e a
    pure x = Parser $ \ s -> pure (x, s)

    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = Parser $ \ s -> do
        (f, t) <- parse p s
        (x, u) <- parse q t
        pure (f x, u)
```

#### A. 3. 2. 1. The parser `pure x`.

For a value `x`, the parser `pure x` does not error and _never consumes input_. The two are linked because if a parser does not error then it must return something when there is no input to be had, which of course requires that it _does not consume input_.

To formalize these definitions, we start by introducing the _remainder_ function that returns the rest of the input on a successful parse:

```haskell
remainder :: Parser s e a -> s -> e :+: s
remainder p = fmap snd . parse p
```

When we speak of the remainder on a successful parse, we always mean the `ys :: s` inside the `Right ys` returned by `remainder p xs`.

__Definition__: A parser `p :: Parser s e a` _does not consume input_ if there is one `xs :: s` for which parsing succeeds and the remainder is equal to `xs`. The parser `p` _never consumes input_ if for every `xs` for which parsing succeeds the remainder is equal to `xs`.

However, even if the two are linked only the former can be reflected in the type signature, to wit:

```haskell
pure :: a -> Parser s Void a
```

The type signature of `pure` is already fixed by the `Applicative` typeclass, so we provide an error-free version of it:

```haskell
value :: a -> Parser s Void a
value = pure
```

One could retort that being fully polymorphic in the error type `e` implies that the parser cannot throw an error, since it is not possible to create values of `e` ex-nihilo; after all, `e` could well be uninhabited as is the case with `e ~ Void`. That much is true, but it is still valuable to signal such, and signal it loudly. So where possible, if a parser does not error it will be reflected in the type signature, at the cost of having to litter the code with `first absurd` calls to satisfy the type checker.

