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

## A. 2. Basic instances.

The type constructor `Parser s e a` is a functor in `a` and this is stock derivable. The other basic instances are `Bifunctor`, `Applicative` and `Monad`.

## A. 2. 1. The `Bifunctor` instance.

Functoriality in the error type is given by the `Bifunctor` instance.

```haskell
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap f g p = Parser $ \ s -> bimap f (first g) $ parse p s
```

Bifunctoriality provides the basic way to unify error types. If we have parsers `p_i :: Parser s e_i a` and a cospan `f_i :: e_i -> e`, then we have a cospan `first f_i :: Parser s e_i a -> Parser s e a`. The choice of `e` is left to the user, but there is a canonical, minimal choice by taking the coproduct of all `e_i`. Unfortunately, dealing with arbitrary coproducts in Haskell is very clunky, even if we reached for any of the innumerable packages on hackage offering extensible sum types -- for just one example, see [sop-core](https://hackage.haskell.org/package/sop-core). Most likely, any truly satisfying solution will need some form of dependent types. Latter on, we will get another way to deal with this recurring problem.

### A. 2. 2. The `Applicative` instance.

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

#### A. 2. 2. 1. The parser `pure x`.

For a value `x`, the parser `pure x` does not error and never consumes input. The two are linked because if a parser does not error then it must do something when there is no input to be had, which of course, requires that it does not consume input. However, even if the two are linked only the former can be reflected in the type signature, e. g. by:

```haskell
pure :: a -> Parser s Void a
```

Since the type signature of `pure` is already fixed by the `Applicative` typeclass, we instead provide an error-free version of it:

```haskell
value :: a -> Parser s Void a
value = pure
```

One could retort that being fully polymorphic in the error type `e` implies that the parser cannot throw an error, since it is not possible to create values of `e` ex-nihilo; after all, `e` could well be uninhabited as is the case with `e ~ Void`. That much is true, but it is still valuable to signal such, and signal it loudly, to the users. So where possible, if a parser does not error it will be reflected in the type signature, at the cost of having to litter the code with `first absurd` calls to satisfy the type checker.

#### A. 2. 2. 2. Unzipping and cozipping.

The universal properties of products and coproducts yield canonical maps,

```haskell
unzip :: Functor f => f (a :*: b) -> f a :*: f b
unzip = fmap fst &&& fmap snd

cozip :: Functor f => f a :+: f b -> f (a :+: b)
cozip = either (fmap Left) (fmap Right)
```

where `:+:` and `:*:` are type operator aliases for `Either` and `(,)` respectively, introduced to make type signatures look nicer. The operator `(&&&)` is the representability isomorphism implied by the universal property of products:

```haskell
(&&&) :: (c -> a) -> (c -> b) -> c -> a :*: b
(&&&) f g x = (f x, g x)
```

In category-theoretic language, every functor is colax-monoidal for products and lax-monoidal for coproducts [^1].

[^1]: The verification of the required coherence laws is a straightforward, albeit tedious, exercise best left to the interested reader.

#### A. 2. 2. 3. Equivalent description of `Applicative`.

As is well known, the `Applicative` typeclass is equivalent to `f` being lax-monoidal for products [^2]:

```haskell
zip :: Applicative f => f a -> f b -> f (a :*: b)
zip p q = (,) <$> p <*> q

unit :: Applicative f => () -> f ()
unit = pure
```

Given the latter, then `<*>` of `Applicative` is given by,

```haskell
(<*>) :: f (a -> b) -> f a -> f b
(<*>) = fmap (uncurry ($)) .> zip
```

where `.>` is composition of a unary function with a binary function:

```haskell
(.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.>) g f x = g . f x
```

The map `pure` is given by,

```haskell
pure :: a -> f a
pure x = fmap (point x) . unit $ terminal x
```

where `terminal` is the unique function `a -> ()` and `point` the isomorphism `a -> (() -> a)`:

```haskell
terminal :: a -> ()
terminal = const ()

point :: a -> (() -> a)
point = const
```

The laws for the typeclasses guarantee that we get the same results either way. The reason for this piece of category-theoretic trivia is that once we get to serializers, we will see that it is the lax-monoidal version of `Applicative` that dualizes well.

[^2]: See for example [Lax monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor).

### A. 2. 3. The `Monad` typeclass.

The `Monad` instance is also readily given.

```haskell
instance Monad (Parser s e) where
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = Parser $ \ s -> do
        (x, t) <- parse p s
        parse (h x) t
```

#### A. 2. 3. 1. Error laws.

The `Bifunctor` instance provides, for a function `f :: d -> e`, a function `first f :: Parser s d a -> Parser s e a`. This function is not only type-changing, but monad-changing.

__Theorem__: For every `f :: d -> e`, the function `first f :: Parser s d a -> Parser s e a` is a natural transformation that is _monoidal_,

```haskell
prop> first f . pure = pure
prop> (first f p) <*> (first f q) = first f (p <*> q)
```

and a _monad morphism_,

```haskell
prop> (first f p) >>= (first f .  h) = first f (p >>= h)
```

__Proof__: Note that if `p` succeeds, then `first f p` also succeeds and with the same result, and conversely, if `p` errors with `e` then `first f p` errors with `f e`. The rest of the proof is a case analysis over the failures.

### A. 2. 4. The `Alternative` instance.

The `Alternative` instance for `Parser s e a` implements _choice_. Specifically, the operator

```haskell
(<|>) :: ParseError s e a -> ParseError s e a -> ParseError s e a
```

tries the first parser and if it errors, _backtracks_ and tries the second on the same input. In order to implement choice, we rely on the parser combinator

```haskell
backtrack :: Parser s e a -> Parser s Void (e :+: a)
```

implementing backtracking. Specifically, the `backtrack` parser runs the argument parser and if it succeeds it returns the result as a `Right` while if it errors, it backtracks and returns the error as a `Left`. In order to implement the backtracking part, we need to probe and change the input state `s` of the parser, so let us start with that first.

#### A. 2. 4. 1. The (absence of the) `MonadState` class.

Probing the state of the parser monad is abstracted out in the `MonadState` typeclass, available from the [mtl package](https://hackage.haskell.org/package/mtl). The instance implementation is as easy as:

```haskell
instance MonadState s (Parser s e) where
    get :: Parser s e s
    get = embed $ \ s -> Right (s, s)

    put :: s -> Parser s e ()
    put s = embed $ const (Right ((), s))
```

The first thing to notice is that both `get` and `put` do not throw an error and `get` does not consume any input; the `put` parser however, allows arbitrary state transformations. Because of all this, we have retained the `get` parser but with `Void` in the type error but have _not_ implemented the full `MonadState` typeclass. This means that `backtrack` cannot make use of `put` and must be implemented as a primitive. See also section.

#### A. 2. 4. 2. The `backtrack` parser.

The implementation of `backtrack` is a standard try-catch implemented directly:

```haskell
backtrack :: Parser s e a -> Parser s Void (e :+: a)
backtrack p = Parser $ \ xs ->
    case parse p xs of
        Left e        -> Right (Left e, xs)
        Right (x, ys) -> Right (Right x, ys)
```

#### A. 2. 4. 3. The `Alternative` instance.

We now have all the ingredients to implement choice via `(<|>)` of the `Alternative` typeclass: try the first parser and return its result; if it errors, backtrack and try the second parser. There is one issue to be solved however, namely, what to do if _both_ parsers error out? One obvious answer is "combine the errors" and "combine the errors" is a code word for a `Monoid` constraint on the error type `e`. With this setup:

```haskell
(<|>) :: Monoid e => Parser s e a -> Parser s e a -> Parser s e a
(<|>) p q = do
    x <- first absurd $ backtrack p
    case x of
        Right x -> pure x
        Left e  -> do
            y <- first absurd $ backtrack q
            case y of
                Right z -> pure z
                Left e' -> throwError $ e <> e'
```

The `empty` parser is also easily implemented with a call to `throwError` with the monoid unit for `e`. The monoid laws for `e` imply that with this structure `Parser s e a` is indeed a monoid, but as we will see next, it implies much more.

What does the constraint `Monoid e` mean in practice? Error types are usually plain data, mainly useful for developers, with no meaningful monoid operation. One of the most common things to do with an error is to just throw it away to a logger trash bin. But this, I contend, is a wrong way to look at the constraint. What the constraint really is, is a strategy for _accumulating errors_, e. g. maybe you need to gather them all in a list or keep the first one only. We revisit the problem below.

### A. 2. 5. Handling errors.

