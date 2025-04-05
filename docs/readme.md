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

Since this is an implementation detail, it is kept private like a closet skeleton or an ugly vice, and the user-facing API uses types like `Either` and `(,)` from base exclusively. To simplify things, in the rest of this document we keep using `Either e (a, s)` as the return type of a parsing function.

## A. 2. Basic instances.

The type constructor `Parser s e a` is a functor in `a` and this is stock derivable. The other basic instances are `Bifunctor`, `Applicative` and `Monad`.

## A. 2. 1. The `Bifunctor` instance.

Functoriality in the error type is given by the `Bifunctor` instance.

```haskell
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap f g p = Parser $ \ s -> bimap f (first g) $ parse p s
```

Bifunctoriality provides the basic way to unify error types. If we have parsers `p_i :: Parser s e_i a` and a cospan `f_i :: e_i -> e`, then we have a cospan `first f_i :: Parser s e_i a -> Parser s e a`. The choice of `e` is left to the user, but there is a canonical, minimal choice by taking the coproduct of all `e_i`. Unfortunately, dealing with arbitrary coproducts in Haskell is very clunky, even if we reached for any of the innumerable packages on hackage offering extensible sum types [^1]. Most likely, any truly satisfying solution will need some form of dependent types. Latter on, we will get another way to deal with this recurring problem.

[^1]: For just one example among many, see [sop-core](https://hackage.haskell.org/package/sop-core).

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

For a value `x`, the parser `pure x` does not error and _never consumes input_. The two are linked because if a parser does not error then it must do something when there is no input to be had, which of course, requires that it _does not consume input_.

__Definition__: A parser `p :: Parser s e a` _does not consume input_ if there is one `xs :: s` for which parsing succeeds and the remainder is equal to `xs`. The parser `p` _never consumes input_ if for every `xs` for which parsing succeeds the remainder is equal to `xs`.

However, even if the two are linked only the former can be reflected in the type signature, e. g. by:

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

In category-theoretic language, every functor is colax-monoidal for products and lax-monoidal for coproducts [^2].

[^2]: See for example [Lax monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor). The verification of the required coherence laws is a straightforward, albeit tedious, exercise best left to the interested reader.

#### A. 2. 2. 3. Equivalent description of `Applicative`.

As is well known, the `Applicative` typeclass is equivalent to `f` being lax-monoidal for products [^3]:

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

[^3]: See for example [Notions of computation as monoids](https://arxiv.org/abs/1406.4823) and references therein.

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

implementing backtracking. Specifically, the `backtrack p` parser runs `p` and if it succeeds it returns the result as a `Right` while if it errors, it backtracks and returns the error as a `Left`. In order to implement the backtracking part, we need to probe and change the input state `s` of the parser, so let us start with that first.

#### A. 2. 4. 1. The (absence of the) `MonadState` class.

Probing the state of the parser monad is abstracted out in the `MonadState` typeclass, available from the [mtl package](https://hackage.haskell.org/package/mtl). The instance implementation is as easy as:

```haskell
instance MonadState s (Parser s e) where
    get :: Parser s e s
    get = embed $ \ s -> Right (s, s)

    put :: s -> Parser s e ()
    put s = embed $ const (Right ((), s))
```

The first thing to notice is that both `get` and `put` do not error and `get` does not consume any input; the `put` parser however, allows arbitrary state transformations. Because of all this, we have retained the `get` parser but with `Void` in the type error but have _not_ implemented the full `MonadState` typeclass. This means that `backtrack` cannot make use of `put` and must be implemented as a primitive.

#### A. 2. 4. 2. The `backtrack` parser.

The implementation of `backtrack` is a standard try-catch implemented directly:

```haskell
backtrack :: Parser s e a -> Parser s Void (e :+: a)
backtrack p = Parser $ \ xs ->
    case parse p xs of
        Left e        -> Right (Left e, xs)
        Right (x, ys) -> Right (Right x, ys)
```

### A. 2. 5. The `Alternative` instance.

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

What does the constraint `Monoid e` mean in practice? Error types are usually plain data, mainly useful for developers, with no meaningful monoid operation. One of the most common things to do with an error is to just throw it away to a logger trash bin. But this, I contend, is a wrong way to look at the constraint. What the constraint really is, is a strategy for _accumulating errors_, e. g. maybe you need to gather them all in a list or keep the first one only.

#### A. 2. 5. 1. The `either` parser and an alternative to `Alternative`.

With the `Alternative` typeclass, we can write the `either` parser that is more deserving of the name choice:

```haskell
either :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (Either a b)
either p q = (Left <$> p) <|> (Right <$> q)
```

Uncurry-ing `either`, we get a natural transformation `(Parser s e a) :*: (Parser s e b) -> Parser s e (a :+: b)`. Since there is also a map

```haskell
unit :: Monoid e => () -> Parser s e Void
unit = const $ throwError mempty
```

this gives a (symmetric) lax-monoidal structure to `Parser s e` between products and coproducts [^4]. Considering the _codiagonal_,

```haskell
codiagonal :: Either a a -> a
codiagonal = either id id
```

we have the equality

```haskell
(<|>) = fmap codiagonal . either
```

The upshot of all this is that the `Alternative` typeclass is a red herring, choice is really a lax-monoidal structure. Let us go through the steps one by one to make this clearer.

[^4]: The coherence laws do hold up; exercise to the interested reader.

#### A. 2. 5. 2. Monoidal categories and monoids.

Monoidal structures are to categories as monoids are to sets, that is, just as a monoid is a binary function satisfying some equational laws, a monoidal structure is a bifunctor `(:*:)` and an object `k`, the _unit_ object, together with natural isomorphisms

```haskell
associator  :: a :*: (b :*: c) -> (a :*: b) :*: c
unitorLeft  :: k :*: a -> a
unitorRight :: a :*: k -> a
```

satisfying a bunch of equations, the so called _coherence laws_ [^5].

The value of any abstraction is in the list of examples it covers and the things you can do with it, both the new concepts that can be expressed and the theorems that can be derived. Starting with the examples, the two most important for us, are the monoidal structures given by products and coproducts respectively. As for concepts that can be expressed, one can now generalize the notion of monoid to a monoid in a monoidal category. The classical notion is recovered by using the product monoidal structure.

[^5]: See [Monoidal categories](https://ncatlab.org/nlab/show/monoidal+category) for the full story.

#### A. 2. 5. 2. Why you never heard of monoids for coproducts.

We start with an existence theorem.

__Theorem__: The codiagonal `a :+: a -> a` together with the unique initial `absurd :: Void -> a` is a monoid for the coproduct monoidal structure.

__Proof__: standard exercise in universal property juggling.

Let us call this monoid structure on `a` the _trivial_ one.

__Theorem__: For each `a` there is only one monoid structure for coproducts, the trivial one.

__Proof__: Since `Void` is initial, there is only one function `Void -> a`. By the universal property of coproducts, a function `a :+: a -> a` is of the form `either f g` for functions `f :: a -> a` and `g :: a -> a`. Now plug this in the two identity laws to get `f = id` and `g = id`.

#### A. 2. 5. 3. Monoids and lax-monoidal functors.

Just as there is a notion of _monoid morphism_, a function that is suitably compatible with monoid structures, there is a notion of _monoidal functor_, a functor `f` together with natural isomorphisms,

```haskell
u :: f a :*: f b -> f (a :+: b)
e :: j -> f k
```

where the objects `j` and `k` are the unit objects for `(:*:)` and `(:+:)` respectively, satisfying some equations [^6] . If we drop the requirements that the natural transformations are isomorphisms we obtain the notion of _lax monoidal functor_.

__Theorem__: Let `f` be a lax monoidal functor between monoidal structures `(:*:)` and `(:+:)` and

```haskell
m :: a :+: a -> a
v :: k -> a
```

a `(:+:)`-monoid structure on `a`. Then

```haskell
m' :: f a :*: f a -> f a
m' = fmap m . u

v' :: j -> f a
v' = fmap v . e
```

is a `(:*:)`-monoid structure on `f a`.

__Proof__: See [Monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor), proposition 3. 1. and references therein.

[^6]: See [Monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor) for them.

#### A. 2. 5. 4. The punchline.

Combining sections [Why you never heard of coproducts](#a-2-5-2-why-you-never-heard-of-monoids-for-coproducts) and [Monoids and lax monoidal functors](#a-2-5-3-monoids-and-lax-monoidal-functors), we have that the functions,

```haskell
u :: Monoid e => (Parser s e a) :*: (Parser s e b) -> Parser s e (a :+: b)
u = uncurry (either)

e :: Monoid e => () -> Parser s e Void
e = unit
```

make `Parser s e` a lax monoidal functor between `:*:` and `:+:`. By the preservation theorem, the functions

```haskell
m :: Monoid e => Parser s e a :*: Parser s e a -> Parser s e a
m = fmap codiagonal . uncurry (either)

v :: Monoid e => () -> Parser s e Void
v = const (throwError mempty)
```

give a monoid structure on `f a`, which is just the `Alternative` instance minus the currying.

#### A. 2. 5. 5. More laws.

Since every function is automatically a monoid morphism for the trivial `:+:`-monoid structures, it follows that for every function `f` and all parsers `p` and `q`:

```haskell
fmap f (p <|> q) = (fmap f p) <|> (fmap f q)
fmap f empty = empty
```

But this is also a consequence of naturality of `(<|>)` and thus a consequence of the free theorem [^7]. Slightly more substantive (because they do not follow from any free theorem) are the next two results.

__Theorem__: If `f :: d -> e` is a monoid morphism, then `first f :: Parser s d a -> Parser s e a` is a monoid morphism:

```haskell
first f empty == empty
(first f p) <|> (first f q) == first f (p <|> q)
```

__Proof__: proof by case analysis on the failures.

The second result are the compatibility laws between `<*>` and `<|>`.

__Theorem__: The `Alternative` instance for `Parser s e a` satisfies _right absorption_, that is, for every `x :: Parser s e a`:

```haskell
empty <*> x == empty
```

If the monoid structure on the error type `e` is idempotent (that is, for all `x :: e`, `x <> x = x`), then, it satisfies both _left_ and _right distributivity_:

```haskell
f <*> (x <|> y) == (f <*> x) <|> (f <*> y)
(f <|> g) <*> x == (f <*> x) <|> (g <*> y)
```
__Proof__: Follows from the monadic definition of `(<*>)` as

```haskell
p <*> q = do
    f <- p
    x <- q
    pure (f x)
```

and doing a case by case analysis on the failures.

As we will see in the next section, the monoid that we will use in the library is idempotent. Another important case is the case when all the error distinctions are erased and the trivial monoid `()` is picked for error type.

[^7]: See [Theorems for Free!](https://dl.acm.org/doi/pdf/10.1145/99370.99404).

### A. 2. 6. Handling errors.

A parsing function has type `s -> Either e (a, s)` with the error type introduced to signal, well, errors. Error throwing and catching is captured in the `MonadError` typeclass from the [mtl package](https://hackage.haskell.org/package/mtl) and the instance for `Parser s e a` is readily given:

```haskell
instance MonadError e (Parser s e) where
    throwError :: e -> Parser s e a
    throwError e = embed $ const (Left e)

    catchError :: Parser s e a -> (e -> Parser s e a) -> Parser s e a
    catchError p h = Parser $ \ s ->
        case run p s of
            Left e      -> run (h e) s
            r@Right _ _ -> r
```

The `catchError` method of the `MonadError` typeclass does not change the error type (and therefore, the monad), but it is easy (and more importantly, useful), to implement a type-changing version:

```haskell
catch
    :: Parser s d a             -- ^ Parser to try.
    -> (e -> Parser s e a)      -- ^ Error handler.
    -> Parser s e a
catch p h = Parser $ \ s ->
    case run p s of
        Left e  -> run (h e) s
        Right p -> Right p
```

`catch` and `throwError` have the right shape for a monad structure for `ParseError s e a` in the error type `e` but it is not difficult to see that, essentially because of short-circuiting, while it satisfies the identity laws, associativity is violated.

## A. 3. On errors.

As discussed in [The `Alternative` instance](#a-2-5-the-alternative-instance), the `Alternative` typeclass requires a `Monoid e` constraint on the error type `e` that determines how errors combine, or as we termed it, the error accumulation strategy. There are two basic options: either errors accumulate in a a list or the parsers short-circuit on the first error. Short-circuiting completely determines the monoid operation:

```haskell
(<>) :: Eq e => e -> e -> e
(<>) x y
    | x == mempty = y
    | otherwise   = x
```

One advantage of the short-circuiting strategy is that the monoid is idempotent guaranteeing stronger laws for the `Alternative` instance -- see section [More laws](#a-2-5-5-more-laws).

### A. 3. 1. First attempt.

The `ParseError e` type is a thin wrapper around `e`, the _error tag_, to implement the short-circuiting strategy:

```haskell
data ParseError e
    = Fail
    | ParseError !e
    deriving stock (Eq, Show, Functor)
```

For the `Monoid` instance, we have as discussed above:

```haskell
instance Semigroup (ParseError e) where
    (<>) :: ParseError e -> ParseError e -> ParseError e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError e) where
    mempty :: ParseError e
    mempty = Fail
```

A little bit of staring and the reader should be able to convince of himself that this type is monoid-isomorphic to `Maybe (First a)` with `First a` the newtype-wrapper from base with semigroup operation pick-the-first-element. The `Maybe` functor then freely adds the monoid unit.

From this isomorphism, it follows that:

__Theorem__: for every `f :: d -> e`, `fmap f :: ParseError d -> ParseError e` is a monoid morphism.

The theorem is an implication, not an iff. The constant map `const Fail :: ParseError d -> ParseError e` is a monoid morphism. For a minimal example of a non-monoid morphism, let `y, y' :: d` be two distinct non-identity elements and `z :: e` a non-identity element, then:

```haskell
f :: Eq d => ParseError d -> ParseError
f x
    | x == y    = z
    | otherwise = mempty 
```

Now, `f (y' <> y)` and `f y' <> f y` are not equal.

### A. 3. 2. What is in an error?

`ParseError e` is just a thin wrapper around `e` for the short-circuiting accumulation strategy; any information specific to the error must be packed in the type `e`. But there are pieces of information that are useful independently of the error type `e`, and that thus are a better fit as fields of `ParseError`, for example a notion of _stream position_ to better locate the source of the error. So we change the `ParseError` to

```haskell
data ParseError s e
    = Fail
    | ParseError !s !e
    deriving stock (Eq, Show, Functor)
```

But now we face a problem: for binary parsers with input type `ByteString`, a `Word` offset is a reasonable notion of position, while for text parsers with input type `Text`, something like

```haskell
data Position = Position {
    line   :: !Word,
    column :: !Word,
}
```

is more useful. So we do what every self-respecting Haskeller does and introduce a typeclass to abstract over the notions of position.

```haskell
{- | The typeclass for input streams with a notion of current position. -}
class HasPosition s where
    {-# MINIMAL position #-}

    {- | The type of the stream's position. -}
    type PositionOf s :: Type

    {- | Return the current position of the stream. -}
    position :: s -> PositionOf s
```

As one can see, the entirety of `HasPosition` is nothing more than a getter for the input stream. It follows that every type `s` has an `HasPosition` instance by simply returning itself as the current position!

```haskell
instance HasPosition s
    type PositionOf s = s

    position :: s -> s
    position = id
```

And this notion of position is not entirely silly, because if the current position can be used to locate the source of the problem, much more so with the entire input stream. So strictly speaking there is no need for this lawless typeclass (and lawless typeclasses are a code smell). There are two reasons that I can enjoin, to put a position instead of the whole stream in `ParseError`. The first is that having the error carry a reference to the input stream potentially keeps it alive in memory for much longer than needed. The second is that if we want to `show` errors (we do), we will get this potentially enormous string filled with completely useless noise.

### A. 3. 3. Backtraces.

Consider the following block

```haskell
parser = do
    ...
    x <- p  -- ^ Can throw here.
    ...
    y <- q  -- ^ Can throw here.
    ...
```

`p` and `q` must have the same error type. If `p` and `q` have different error types `e1` and `e2`, we must find a type `e` and a cospan of functions `e1 -> e <- e2` and write

```haskell
parser = do
    ...
    x <- first f1 p  -- ^ Can throw here.
    ...
    y <- first f2 q  -- ^ Can throw here.
    ...
```

Another option is to throw a different error `e'`, with the error thrown from `p` attached like an _exception backtrace_. The obvious problem is that the errors of `p` and `q` can be different so we must still find appropriate cospans; Haskell's GADT's and existentials to the rescue.

```haskell
data ParseError s e where
    Fail :: ParseError s e              -- ^ Monoid unit for `ParseError s e`.
    ParseError
        :: (Typeable d, Eq d, Show d)
        => (Maybe (ParseError s d))     -- ^ Backtrace.
        -> !s                           -- ^ (Position of the) input stream.
        -> !e                           -- ^ Error tag.
        -> ParseError s e
```

The reader can read up on existentials, but the one-line summary is that we can use _any_ `(Typeable d, Eq d, Show d) => ParseError s d` as a backtrace of an error but getting it back the only thing we know about it is that it is a `Maybe (ParseError s d)` with `d` satisfying the constraints `(Typeable d, Eq d, Show d)`. We are trading more flexibility in error handling for less flexibility in handling backtraces, since we cannot pin down their type. Is the trade-off worth it? I guess we will find out.

note(s):

  * The actual shape of `ParseError` in [ParseError.hs](../src/Trisagion/Parsers/ParseError.hs) is slightly different.

With these changes to `ParseError`, we can now have a parser combinator that turns a thrown error into the backtrace of a new, contextually more useful, error:

```haskell
onParseError
    :: (HasPosition s, Typeable d, Eq d, Show d)
    => e                                        -- ^ Error tag of new error.
    -> Parser s (ParseError (PositionOf s) d) a -- ^ Parser to run.
    -> Parser s (ParseError (PositionOf s) e) a
onParseError e p =
    catch
        p
        (\ b -> do
            s <- first absurd get
            absurd <$> throw (makeBacktrace b s e))
```

The above block can now be written as,

```haskell
parser = do
    ...
    x <- onParseError e1 p  -- ^ Can throw here.
    ...
    y <- onParseError e2 q  -- ^ Can throw here.
    ...
```

for appropriate `e1, e2 :: e`, without having to unify the error types of `p` and `q`.

#### A. 3. 3. 1. The backtrace getter.

With backtraces, a `ParseError` looks like,

>  error -> Just error_0 -> ... -> Just error_n -> Nothing

with `error_i` _not_ equal to a `Fail` by normalization. So the full backtrace is just a list of `(Typeable d, Eq d, Show d) => ParseError s d`. This leads to implement a getter for the backtrace as an elimination function:

```haskell
backtrace :: forall s e a . (forall d . s -> d -> a) -> ParseError s e -> [a]
backtrace f = go
    where
        go :: ParseError s c -> [a]
        go Fail               = []
        go (ParseError b s e) = f s e : maybe [] go b
```

### A. 3. 4. Anything else you want to add?

No, not really.

## A. 4. Typeclasses for the input.

We are at the point where we can finally tackle the constraints needed for the input type `s`; after all, at this point we cannot even get out one element from the input stream.

### A. 4. 1. One out of `s`: the `Streamable` typeclass.

What we need from `s`: an `uncons` operation with the return type depending on `s`.

```haskell
{- | The @Streamable@ typeclass of monomorphic input streams. -}
class Streamable s where
    type ElementOf s :: Type

    {- | Uncons the first element from the input stream. -}
    uncons :: s -> Maybe (ElementOf s, s)

head :: Streamable s => s -> Maybe (ElementOf s)
head = fmap fst . uncons

tail :: Streamable s => s -> Maybe s
tail = fmap snd . uncons
```

### A. 4. 2. The `MonoFunctor` constraint.

All the paradigmatic examples of input streams like `ByteString` and `Text` have a `map`-like operation, a monomorphic variant of `fmap`. The `MonoFunctor` typeclass captures this;

```haskell
class MonoFunctor s where
    -- | The type of the elements of the monofunctor.
    type ElementOf s :: Type

    -- | Map over an element of the monofunctor.
    monomap :: (ElementOf s -> ElementOf s) -> s -> s

-- Instances.
instance MonoFunctor [a] where
    type ElementOf [a] = a

    monomap :: (a -> a) -> [a] -> [a]
    monomap = fmap
```

There are corresponding instances for types like `ByteString` and `Text`, but in this document, if we need some concrete stream type we will just defer to something like `[a]`.

The `MonoFunctor` typeclass, as well as monomorphic versions of `Foldable` and `Traversable` can be found in the [mono-traversable package](https://hackage.haskell.org/package/mono-traversable). We have our own version of it, available from the [mono github repository](https://github.com/lambda-dom/mono).

`MonoFunctor` is not a terribly useful typeclass but it ends up being important as a base and to state some of the typeclass laws. With this superclass:

```haskell
{- | The @Streamable@ typeclass of monomorphic, streamable functors. -}
class MonoFunctor s => Streamable s where
    {- | Uncons the first element from the input stream. -}
    uncons :: s -> Maybe (ElementOf s, s)
```

### A. 4. 3. No free laws.

Since monofunctors `f` are not fully polymorphic in `ElementOf f`, there are no free theorems available and equational laws like naturality must be explicitly required.

__Definition__: Let `s` and `t` be two monofunctors with `a ~ ElementOf s ~ ElementOf t`. A function `h :: s -> t` is _mononatural_ if for every `f :: a -> a` we have the equality `monomap f . h = h . monomap f`.

notes(s):

  * There are equivalent descriptions of monofunctoriality and mononaturality in terms of monoid actions, but these trivial reformulations do not yield anything important for our purposes.

The first law for `Streamable` is that `uncons` is mononatural. In case it is not clear, the `MonoFunctor` instance of the codomain is

```haskell
monomap :: MonoFunctor s => (s -> s) -> Maybe (ElementOf s, s) -> Maybe (ElementOf s, s)
monomap f = fmap (bimap f (monomap f))
```

### A. 4. 4. The (absence of the) `MonoFoldable` constraint.

Given the `uncons` operation, we can define a conversion to lists,

```haskell
toList :: Streamable s => s -> [ElementOf s]
toList = unfoldr uncons
```

so that `Streamable` could / should have `MonoFoldable` as a superclass. There are two main reasons why `MonoFoldable` is not a superclass:

  1. For infinite lists, `monolength` diverges and we _do_ want infinite lists and other stream-like objects to be instances of `Streamable`.

  2. For input streams like `ByteString.Lazy` computing its length would force the entire bytestring into memory which is a big no-no.

Of course, _if_ `s` is an instance of `MonoFoldable` then the equality should hold and this is the second law for `Streamable`. This means that `toList` is _not_ a typeclass method; if you want an overridable list conversion, you need a `MonoFoldable` instance.

### A. 4. 5. Two fundamental parsers.

With the `Streamable` typeclass we can now extract one element from the input stream and also check if the input stream has more elements to yield.

```haskell
-- | Error thrown when input stream is exhausted.
data InputError = InputError
    deriving stock (Eq, Show)


eoi :: Streamable s => Parser s Void Bool
eoi = null <$> get

one :: Streamable s => Parser s (ParseError (PositionOf s) InputError) (ElementOf s)
one = do
    xs <- get
    case uncons xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> absurd <$> throw InputError
```

Since the `put` parser is not available, `one` has to be implemented directly in the core. Also, and as can be seen in the example of `one`, the type signature of parsers involving `ParseError` can get gnarly. We introduce the type alias `type ParserPE s e a = Parser s (ParseError (PositionOf s) e) a` to shorten them.

### A. 4. 6. Some definitions.

Recall that the remainder of a parser's input can be obtained via

```haskell
remainder :: Parser s e a -> s -> Maybe s
remainder p = either (const Nothing) id . fmap snd . parse p
```

What is the relation of the remainder with the original input, if any?

__Definition__: A parser `p :: Parser s e a` is _normal_ if for every `xs :: s`, on success, the remainder is a (possibly improper) suffix of `xs`.

With the `Streamable` typeclass, the is-suffix relation is simply `isSuffixOf` at the level of lists.

```haskell
isSuffixOf :: Streamable s => s -> s -> Bool
isSuffixOf xs ys = (toList xs) `isSuffixOf` (toList ys)
```

In this library, it is not possible to construct non-normal parsers. All primitives return normal parsers, all combinators do as much and the `Parser` constructor is not exported and there is no way to change a parser's state. The trade-off for this guarantee is that the user cannot add new primitives.

## A. 5. Prefixes and the `Splittable` typeclass.

The `Streamable` typeclass allows to write down all commonly used parsers, but alas, getting one element from the input stream at a time can be very inefficient. What we need is a notion of chunk, or stream prefix, and methods to cut out prefixes from streams.

### A. 5. 1. The `Splittable` class.

Hence the `Splittable` typeclass.

```haskell
class Streamable s => Splittable s where
    {-# MINIMAL splitAt, splitWith #-}

    {- | The type of prefixes of the streamable. -}
    type PrefixOf s :: Type

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    splitAt :: Word -> s -> (PrefixOf s, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.

    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    splitWith :: (ElementOf s -> Bool) -> s -> (PrefixOf s, s)
```

### A. 5. 2. The laws.

To state the laws, we must assume something of `PrefixOf s` that is not expressed directly in the typeclass. The first constraint is that `PrefixOf s` is a monofunctor with the same type of elements as `s`, that is, `ElementOf (PrefixOf s) ~ ElementOf s`. With this assumption: for every `n` and every `p`, both `splitAt n` and `splitWith p` are mononatural.

For the second law, put

```haskell
let (prefix, suffix) = spliAt n xs
```

for arbitrary `n` and `xs`. Given the `toList` function on `Streamable`, both `suffix` and `xs` can be converted to lists, and since as per the name `prefix` is supposed to be a prefix of `xs`, then there should be a unique list `l` such that

```haskell
toList xs = l ++ toList suffix
```

It follows that we have the equality,

```haskell
l = take (length $ toList xs - length $ toList suffix) (toList xs)
```

so it is not much of a stretch to assume that prefixes can be converted to lists. Note that the arguments above for not requiring `MonoFoldable s` on a `Streamable` do _not_ apply, that is, we are implicitly assuming that prefixes are indeed _finite_ monofoldables, in part because some important parsers with a `Splittable s` constraint require computing the lengths of prefixes. Therefore, assuming a further `MonoFoldable (PrefixOf s)`, which is satisfied by all the `Splittable` instances defined by the library, the second typeclass law just says that at the level of lists `splitAt` is `splitAt` and `splitWith` is `span`:

```haskell
bimap monotoList toList . splitAt n = splitAt n . toList
bimap monotoList toList . splitWith p = span p . toList
```

The third and final law is a compatibility condition between `uncons` and `splitAt`:

```haskell
maybe [] (bimap singleton toList) . uncons = bimap toList toList . splitAt 1
```

# B. Serializers.

## B. 1. First attempt.

A first solution is to carry the codomain `s` of a serializer in a `Writer` monad as is done in say, the [cereal package](https://hackage.haskell.org/package/cereal).

```haskell
data Writer w a = Writer w a
    deriving stock Functor

-- Instances.
instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure = Writer mempty

    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    (<*>) (Writer m f) (Writer n x) = Writer (m <> n) (f x)

instance Monoid m => Monad (Writer m) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    (>>=) (Writer m x) h = let (Writer n y) = h x in Writer (m <> n) y 


-- Basic functions.
run :: Writer w a -> (w, a)
run (Writer m x) = (m, x)

tell :: w -> Writer w ()
tell m = Writer m ()
```

As is obvious from the code block, `Writer w a` is isomorphic to `w :*: a`. `w` is a monoid with a "cheap" `(<>)` operation, typically a builder for some underlying stream like `ByteString` or `Text`. This detail is not really relevant for this section, so we can just take `w` to be `[Word8]` for the sake of concreteness.

We get, from the `Writer` instances alone, a lot of power. For example, the zip combinator allows a to combine writers for `a` and `b` to a writer for `(a, b)`. Expanding the definition:

```haskell
pair :: Writer w a -> Writer w b -> Writer w (a, b)
pair p q = do
    x <- p
    y <- q
    pure (x, y)
```

The `w` accumulator is threaded around by `Writer` in such a way that it disappears from sight. We can recover it at the end of a computation, by `fst . run`.

## B. 2. Two arguments.

There are two arguments against this implementation. The first is that the `Writer` monad leaks -- see [Issues with monad transformers](https://github.com/haskell-effectful/effectful/blob/master/transformers.md). More importantly is the conceptual reason: the implementation does not align with basic intuitions about serialization. The result `w` is _hidden_ in the computation when it is the whole point of it. As a consequence, the construction of the serializer for the whole from the parts constructs in parallel the whole object because the constructors are invoked on the way -- e. g. the serializer for `(,)` calls the `(,)` constructor -- but this gets things backwards.

## B. 3. Serializers in the corepresentable representation.

We arrive at the representation of a serializer as,

```haskell
newtype Serializer m a = Serializer (a -> m)

-- | Run the serializer on the input and return the results.
run :: Serializer m a -> a -> m
run (Serializer m) = m
```

where the codomain `m` is a monoid -- see below for more on this.

The first difference with parsers is that a serializer is contravariant in `a`. `Contravariant` functor is the typeclass from base formalizing this.

```haskell
instance Contravariant (Serializer m) where
    contramap :: (a -> b) -> Serializer m b -> Serializer m a
    contramap f s = Serializer (run s . f)
```

### B. 3. 1. Relation with parsers.

### B. 3. 2. The prism version.

## B. 4. Instances.

### B. 4. 1. The lax monoidal structure for products.

As mentioned in [Equivalent description of `Applicative`](#a-2-2-3-equivalent-description-of-applicative), the applicative structure is equivalent to a lax-monoidal structure for the product monoidal structure. The corresponding structure for serializers is the same lax-monoidal structure:

```haskell
zip :: Monoid m => Serializer m a -> Serializer m b -> Serializer m (a, b)
zip s t = Serializer $ uncurry (<>) . bimap (run s) (run t)

unit :: Monoid m => Serializer m ()
unit = Serializer $ const mempty
```

This is encoded in the `Divisible` typeclass from the [contravariant package](https://hackage.haskell.org/package/contravariant).

```haskell
instance Monoid m => Divisible (Serializer m) where
    conquer :: Serializer m a
    conquer = Serializer $ const mempty

    divide :: forall a b c . (c -> a :*: b) -> Serializer m a -> Serializer m b -> Serializer m c
    divide f s t = Serializer $ g . f
        where
            g :: a :*: b -> m
            g = uncurry (<>) . bimap (run s) (run t)
```

### B. 4. 2. The monoid instance.

There is also a `Monoid` instance for `Serializer m a` that allows to combine two serializers.

```haskell
instance Semigroup m => Semigroup (Serializer m a) where
    (<>) :: Serializer m a -> Serializer m a -> Serializer m a
    (<>) s t = Parser $ \ x -> run s x <> run t x

instance Monoid m => Monoid (Serializer m a) where
    mempty :: Serializer m a
    mempty = Parser $ const mempty
```

If we look at the code for `Divisible`, one can see that this monoid structure allows us to replace uses of it in a way analogous that `<*>` allows us to seemlessly extend a binary parser combinator to an n-ary parser combinator.

One important property this monoid structure is that it is natural in `a`, and thus a monoid morphism:

__Theorem__: For every `f :: a -> b`, we have the equalities:

```haskell
prop> contramap f mempty == mempty
prop> contramp f (s <> t) == contramap f s <> contramap f t 
```

Because of this property, it could be argued that the monoid instance is the analog of the `Alternative` instance for parsers. But as seen in section [`Alternative`](#a-2-5-the-alternative-instance), the `Alternative` instance is best seen as a lax-monoidal structure from products to coproducts, and in this form, the serializer analog is given below in section [The lax-monoidal structure for coproducts](#b-4-5-the-lax-monoidal-structure-for-coproducts).

But the fundamental break down in the analogy is that there is no error handling needed for serializers; and since there is no error handling, no backtracking is needed; and since there is no analog of backtracking, there is no analog of choice.

### B. 4. 3. The left action.

Closely related to the monoid instance is the left action of `m` on `Serializer m a`:

```haskell
(|*>) :: Monoid m => m -> Serializer m a -> Serializer m a
(|*>) m s = Serializer $ \ x -> m <> run s x
infixr 5 |*>
```

__Theorem__: `(|*>)` is a left `m`-action on `Serializer m a`, that is, it satisfies the equalities:

```haskell
prop> m <> n |*> s == m |*> n |*> s
prop> mempty |*> s == s
```

This also gives us the occasional useful function `collapse`, that allows us to embed `m` in `Serializer m a`:

```haskell
collapse :: m -> Serializer m a
collapse m = m |*> mempty
```

### B. 4. 4. The prism for products.

Consider the case of a product type, a type of the form

```haskell
data T a_0 ... a_n = T a_0 ... a_n
```

Assume there are serializers `Serializer m a_i` with `i` ranging from `0` to `n`. A natural idea for a format for `T` is to lay out the `a_i` consecutively one after another. So a serializer for `T` is, denoting by `f_i :: T a_0 ... a_n -> a_i` the field projections,

```haskell
s :: Serializer m (T a_0 ... a_n)
    =  s_0 (f_0 x)
    <> ...
    <> s_n (f_n x)
```

Dually, assume there are parsers `p_i :: Parser s e_i a_i` with `i` ranging from `0` to `n`. A parser for `T` we have to apply the parsers `p_i` consecutively and then apply the `T` constructor to the results. Fixing a cospan `f_i :: e_i -> e`:

```haskell
p :: Parser s e (T a_0 ... a_n)
p = T <$> first f_0 p_0 <*> ... <*> first f_n p_n
```

Instead of a cospan `f_i`, we can fix instead an error tag type `e` and use `onParseError e_i` for approppriate `e_i :: e`, instead of `first f_i`.

Note the duality in constructing serializers and parsers: for the parsers we use the constructor to synthesize the whole from the parts, while for serializers we use the field projections, or the eliminators, to synthesize the whole from the parts.

### B. 4. 5. The lax-monoidal structure for coproducts.

As seen in the section [`Alternative`](#a-2-5-the-alternative-instance), the `Alternative` instance is equivalent to a lax-monoidal structure from products to coproducts. The corresponding in the serializer world is:

```haskell
either :: Monoid m => Serializer m a -> Serializer m b -> Serializer m (a :+: b)
either s t = Serializer $
    \case
        Left x  -> (run s) x
        Right y -> (run t) y

empty :: Serializer m Void
empty = Serializer absurd
```

Using the `Decidable` typeclass, also from the [contravariant package](https://hackage.haskell.org/package/contravariant):

```haskell
instance Monoid m => Decidable (Serializer m) where
    lose :: (a -> Void) -> Serializer m a
    lose f = Serializer $ absurd . f

    choose :: (a -> b :+: c) -> Serializer m b -> Serializer m c -> Serializer m a
    choose f s t = Serializer $ choice (run s) (run t) . f
        where
            -- | The Representability isomorphism.
            choice :: (a -> m) -> (b -> m) -> (a :+: b) -> m
            choice p q
                = \case 
                    Left x  -> p x
                    Right y -> q y
```

### B. 4. 6. The prism for coproducts.

Now consider the case of a coproduct, a type of the form

```haskell
data T a_0 ... a_n
    = T_0 a_0
    ...
    | T_n a_n
```

The first thing to notice is that the general case of a constructor of the form `T_i b_0 ... b_n_i` can be reduced to the one-argument case, by setting `a_i ~ (b_0, ..., b_n_i)` and using the constructions of section [The prism for products](#b-4-3-the-prism-for-products).

Assuming the existence of serializers `s_i :: Serializer m a_i` with `i` ranging from `0` to `n`, a natural format for `T a_0 ... a_n` is to first have a discriminating tag followed by the encoding of the relevant value. The tag can be implemented simply by enumerating the constructors top to bottom and return the corresponding ordinal:

```haskell
tag :: T a_0 ... a_n -> Word
tag x = case x of
    T_0 _ -> 0
    ...
    T_n _ -> n
```

This piece of bloatware can even be derived automatically using something like the [generics-sop library](https://hackage.haskell.org/package/generics-sop) or (God forbid) template Haskell, but we will not dwell on this detail here.

note(s):

  * The serializing format using the `tag` function is vulnerable to changes in `T` like reordering or adding new constructors. How this can be solved is a whole different problem.

Assuming the existence of a primitive serializer `word :: Serializer m Word`, we now have:

```haskell
s :: Serializer m (T a_0 ... a_n)
s = Serializer $ x ->
    word (tag x)
    |*> case x of
        T_0 x_0 -> s_0 x0
        ...
        T_n x_n -> s_n x_n 
```

The `case` statement is just an expansion of the generic eliminator for `T`, `either s_0 ... s_n`, which can be expressed in terms of the prisms for `a_i` and the alternative instance for `Maybe`, e. g. denoting the prism getters `T a_0 ... a_n -> Maybe a_i` by `p_i` then:

```haskell
either :: (a_0 -> b) -> ... -> (a_n -> b) -> T a_0 ... a_n -> b
either f_0 ... f_n r = asum [f_0 $ p_0 r, ..., f_n $ p_n r]
```

Dually, assume the existence of parsers `p_i :: Parser s e_i a_i` and a cospan `f :: e_i -> e`. Also assume the existence of a primitive parser `word :: Parser s e' Word` and an error conversion function `f :: e' -> e`. Then the parser for this format is just:

```haskell
parser :: Parser s e (T a_0 ... a_n)
parser = do
    i <- first f word
    case i of
        i | 0 == i -> bimap f_0 T_0 p_0
        ...
        i | n == i -> bimap f_n T_n p_n
        _          -> throwError e
```

Once again we see the duality: on the parser side we have the `Monad` bind combinator sequencing the two parsers, while on the serializer side that role is played by the left action `(|*>)` operator. On the parser side, we do a case analysis on the constructor tag and call the appropriate constructor on the appropriate parser, and we have to add a default error branch, while on the serializer side we use the eliminator to dispatch on the appropriate serializer.

### B. 4. 7. The prism for sequences.

We give one more example of constructing a prism for the whole from a prism for the parts. Let `t` be a `Functor` and a `Foldable`. Then a format for serializing `t` is to first serialize the length, then repeatedly apply the serializer `s :: Serializer m a` to the elements of `t a`.

```haskell
serializer :: Foldable t => Serializer m (t a)
serializer = Serializer $ xs ->
    word (length xs) |*> foldmap (run s) xs
```

To parse this format, we need to first be able to construct a `t` from a list.

__Definition__: A foldable `f` is a _sequence_ if `toList :: f a -> [a]` is an isomorphism.

We denote the inverse of `toList` by `fromList`. Examples of sequences include `[a]`, `Vector a` and `Seq a`; `Ord a => Set a` is _not_ a sequence, because `toList` returns the list of elements in ascension-key order. A second related reason is that `Set a` does not have a `Functor` instance.

With the `fromList` inverse, and assuming the existence of a parser `p :; Parser s e a`, we have,

```haskell
parser :: Parser s e (t a)
parser = word >>= fmap fromList . flip repeat p
```

where `repeat` is the parser combinator:

```haskell
repeat :: Word -> Parser s e a -> Parser s e [a]
repeat 0 _ = pure []
repeat n p = (:) <$> p <*> repeat (pred n) p
```
