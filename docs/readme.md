# Trisagion.

# A. Formats, output types and serializers.

A serializer for a type `a` is a function `a -> ByteString` that given a value `x :: a` returns a `ByteString` encoding `x`. We can generalize over the return type and put, on a first approximation,

```haskell
newtype Serializer s a = Serializer (a -> s)
```

with serialization done via

```haskell
serialize :: Serializer s a -> a -> s
serialize (Serializer f) = f
```

What constraints should be put on `s` to serve as an adequate output type? By Quine's dictum "No entity without identity", which even if not true of being in general, it certainly is of mathematicals, we (implicitly) require `Eq s`, but a discussion of what else is needed will be deferred to a later chapter. For now, we content ourselves with noting that the paradigmatical examples of `s` we have in mind are `ByteString`, `Text` and `[Char]`.

We will also delay any extra elaboration of the `Serializer` type for latter, but we do mention one extra wrinkle; the type signature for `Serializer` suggests that given `a` and `s`there is a unique serializer `Serializer s a`. But this is not quite right, as there is an implicit dependency on an _encoding format_, so what we should have is,

```haskell
makeSerializer :: Format s -> Serializer s a
```

where `Format s` is some (G)ADT describing the encoding format.

# B. Parsers.

Dually to serializing, _parsing_ is a function `s -> a` that given some input `xs :: s` returns a decoded value `x :: a`. Wrapping in newtypes,

```haskell
newtype Parser s a = Parser (s -> a)
```

To make the dependency on the encoding explicit we have as in section [Formats, output types and serializers](#a-formats-output-types-and-serializers),

```haskell
makeParser :: Format s -> Parser s a
```

As in section [Formats, output types and serializers](#a-formats-output-types-and-serializers), we defer for later what is needed for `s` to be an adequate input type.

## B. 1. Parsing functions.

File(s):

  * [Parser.hs](../src/Trisagion/Parser.hs)

We defined a parsing function as a function `a -> s` but the problem with this definition is that it does not allow composition. To be able to compose parsing functions, we need the parsing function to also return the rest of the input so that the next parser in the pipeline can continue, so we redefine the `Parser` type as

```haskell
newtype Parser s a = Parser (s -> (a, s))
```

This is still not quite right because parsing, contrary to serializing, can fail, so:

```haskell
newtype Parser s e a = Parser (s -> Either e (a, s))
```

To run a parser on input, simply

```haskell
run :: Parser s e a -> s -> Either e (a, s)
run (Parser p) = p
```

The original parsing function `s -> a` is recovered, minus the error term, as

```haskell
eval :: Parser s e a -> s -> Either e a
eval p = fmap fst . run p
```

Fixing the format `Format s`, and minus the error handling, `eval` is the inverse of `serialize`.

## B. 2. One implication and one design decision.

An immediate implication of the type signature of a parsing function is that it is all-or-nothing: _either_ it throws an error _or_ (exclusive or) it succeeds, returning the pair of the parsed result and the rest of the input.

At this point, we note that `Parser` could be generalized to a transformer by,

```haskell
-- The ParserT monad transformer.
data ParserT m s e a = Parser (s -> m (Either e (a, s)))

-- The Parser monad.
type Parser = ParserT Identity
```

as is done in say, the [Megaparsec](https://hackage.haskell.org/package/megaparsec) library. In this library, we explicitely do _not_ make such a generalization; all code is pure (meaning: effect free). This design forces prospective library users to construct the parser and stuff it somewhere, gather the input from the IO layer and apply the parser via `run`. The expectation is that, for the cases where the input must be consumed incrementally, some scheme using a streaming library can be bolted on top.

## B. 3. A small improvement: the type `Result s e a`.

File(s):

  * [Result.hs](../src/Trisagion/Types/Result.hs)

We make one small improvement by replacing the return type `Either e (a, s)` of a parsing function by the isomorph

```haskell
data Result s e a
    = Error !e
    | Success a !s 
```

This introduces strictness where lazyness is almost surely not needed while keeping it in where it is useful. It also removes one layer of indirection in the `Success` case.
