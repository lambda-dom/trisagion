# Trisagion.

A _serializer_ for a type `a` is a function `a -> ByteString` that given a value `x :: a` returns a `ByteString` encoding `x`. We can generalize over the return type and put, on a first approximation,

```haskell
newtype Serializer s a = Serializer (a -> s)
```

with serialization done via

```haskell
serialize :: Serializer s a -> a -> s
serialize (Serializer f) = f
```

What constraints should be put on `s` to serve as an adequate output type? By Quine's dictum "No entity without identity", which even if not true of being in general, it certainly is of mathematicals, we (implicitly) require `Eq s`, but a discussion of what else is needed will be deferred to a later chapter. For now, we content ourselves with noting that the paradigmatical examples of `s` we have in mind are `ByteString`, `Text` and `[Char]`.

Dually to serializing, _parsing_ is a function `s -> a` that given some input `xs :: s` returns a decoded value `x :: a`. Wrapping in newtypes,

```haskell
newtype Parser s a = Parser (s -> a)
```

As with serializers we defer for later what is needed for `s` to be an adequate input type.

# A. Parsing.

## A. 1. The `Parser s e a` type.

## A. 1. 1. Parsing functions.

File(s):

  * [Parser.hs](../src/Trisagion/Parser.hs)

In the introduction a parsing function was defined as a function `a -> s` but the problem with this definition is that it does not allow composition. To be able to compose parsing functions, we need the parsing function to also return the rest of the input so that the next parser in the pipeline can continue, so we redefine the `Parser` type as

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

The original notion parsing function `s -> a` is recovered, minus the error term, as

```haskell
eval :: Parser s e a -> s -> Either e a
eval p = fmap fst . run p
```

Minus the error handling, `eval` ought to be the inverse of `serialize`.

### A. 1. 2. One implication and one design decision.

An immediate implication of the type signature of a parsing function is that it is all-or-nothing: _either_ it throws an error _or_ (exclusive or) it succeeds, returning the pair of the parsed result and the rest of the input.

At this point, we note that `Parser` could be generalized to a transformer by,

```haskell
-- The ParserT monad transformer.
data ParserT m s e a = Parser (s -> m (Either e (a, s)))

-- The Parser monad.
type Parser = ParserT Identity
```

as is done in say, the [Megaparsec](https://hackage.haskell.org/package/megaparsec) library. In this library, we explicitely do _not_ make such a generalization; all code is pure (meaning: effect free). This design forces prospective library users to construct the parser and stuff it somewhere, gather the input from the IO layer and apply the parser via `run`. The expectation is that, for the cases where the input must be consumed incrementally, some scheme using a streaming library can be bolted on top.

### A. 1. 3. A small improvement: the type `Result s e a`.

File(s):

  * [Result.hs](../src/Trisagion/Types/Result.hs)

We make one small improvement by replacing the return type `Either e (a, s)` of a parsing function by the isomorph

```haskell
data Result s e a
    = Error !e
    | Success a !s 
```

This introduces strictness where lazyness is almost surely not needed while keeping it in where it is useful. It also removes one layer of indirection in the `Success` case.

note(s):

  * In what follows, all the code examples we give we keep using `Either e (a, s)` as the return type of a parsing function.

## A. 2. Some definitions.

Given a parser `p :: Parser s e a` what is the relation of the input with the remainder, if any?

__Definition__: A parser `p :: Parser s e a` is _normal_ is for every input `xs :: s`, on success, the remainder is a (possibly improper) suffix of `xs`.

There is one obvious problem with this definition, in that being a suffix is not a definable relation for an arbitrary type `s`. However, it is certainly well-defined for types like `ByteString` and `Text`, so, as the reader is probably already expecting given the times we have used the same sentence, we leave for later the working out of what is needed to define such a relation.

__Definition__: A parser `p :: Parser s e a` _does not consume input_ if there is one input `xs :: s` for which parsing succeeds and the remainder is (equal to) `xs`. The parser `p` _never consumes input_ if for every input `xs`, on success the remainder is (equal to) `xs`.

## A. 3. Parser typeclasses.

### A. 3. 1. Products.

Let us start with the case of a product type, a type of the form

```haskell
data T a_0 ... a_n = T a_0 ... a_n
```

Assume there are parsers `p_i :: Parser s e_i a_i` with `i` ranging from `0` to `n`. A natural idea for a format for `T` is to lay out the `a_i` consecutively one after another. Dually, to construct a parser for `T` we have to apply the parsers `p_i` consecutively and then apply the `T` constructor to the results. This is naturally done via the `Functor` and `Applicative` typeclasses:

```haskell
p :: Parser s e (T a_0 ... a_n)
p = T <$> p_0 <*> ... <*> p_n
```

#### A. 3. 1. 1. The `Applicative` typeclass.

The `Functor` instance for `Parser s e a` is stock-derivable and the `Applicative` instance is readily given:

```haskell
instance Applicative (Parser s e) where
    pure :: a -> Parser s e a
    pure x = embed $ \ s -> Right (x, s)

    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = embed $ \ s ->
        case run p s of
            Left e       -> Left e
            Right (f, t) ->
                case run q t of
                    Left e'      -> Left e'
                    Right (x, u) -> Right (f x, u)
```
