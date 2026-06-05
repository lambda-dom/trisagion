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

It should be noted that none of these instances require constraints on the input stream type `s` or the error type `e`.

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

#### A. 3. 2. 2. Unzipping and cozipping.

The universal properties of products and coproducts yield canonical maps,

```haskell
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip = fmap fst &&& fmap snd

cozip :: Functor f => f a :+: f b -> f (a :+: b)
cozip = either (fmap Left) (fmap Right)
```

The operator `(&&&)` is the representability isomorphism implied by the universal property of products:

```haskell
(&&&) :: (c -> a) -> (c -> b) -> c -> (a, b)
(&&&) f g x = (f x, g x)
```

In category-theoretic language, every functor is colax-monoidal for products and lax-monoidal for coproducts [^2].

[^2]: See for example [Lax monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor). The verification of the required coherence laws is a straightforward, albeit tedious, exercise best left to the interested reader.

#### A. 3. 2. 3. Equivalent description of `Applicative`.

As is well known, the `Applicative` typeclass is equivalent to `f` being lax-monoidal for products [^3]:

```haskell
zip :: Applicative f => f a -> f b -> f (a, b)
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

<!-- The canonical tensor strengths. -->
<!-- Explicit reference. -->

[^3]: See for example [Notions of computation as monoids](https://arxiv.org/abs/1406.4823) and references therein.

### A. 3. 3. The `Monad` typeclass.

The `Monad` instance is also readily given.

```haskell
instance Monad (Parser s e) where
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = Parser $ \ s -> do
        (x, t) <- parse p s
        parse (h x) t
```

### A. 3. 4. Error laws.

The `Bifunctor` instance yields, for a function `f :: d -> e`, a function `first f :: Parser s d a -> Parser s e a`. This function is not only type-changing, but monad-changing.

__Theorem__: For every `f :: d -> e`, the function `first f :: Parser s d a -> Parser s e a` is a natural transformation that is _monoidal_,

```haskell
first f . pure == pure
first f (p <*> q) == (first f p) <*> (first f q)
```

and a _monad morphism_,

```haskell
first f (p >>= h) == (first f p) >>= (first f .  h)
```

note(s):

    * In all these laws, free variables are universally quantified.

__Proof__: Note that if `p` succeeds, then `first f p` also succeeds and with the same result, and conversely, if `p` errors with `e` then `first f p` errors with `f e`. The rest of the proof is a case analysis over the failures.

## A. 4. The `Alternative` instance: choice and backtracking.

The `Alternative` instance for `Parser s e a` implements _choice_. Specifically, the operator

```haskell
(<|>) :: Parser s e a -> Parser s e a -> Parser s e a
```

tries the first parser and if it errors, _backtracks_ and tries the second on the same input. In order to implement choice, we rely on the parser combinator

```haskell
try :: Parser s e a -> Parser s Void (e :+: a)
```

implementing backtracking. The `try p` parser runs `p` and if it succeeds it returns the result as a `Right` while if it errors, it backtracks and returns the error as a `Left`. In order to implement the backtracking part, we need to probe and change the input state `s` of the parser, so let us start with that first.

### A. 4. 1. The `MonadState` class.

Probing the state of the parser monad is abstracted out in the `MonadState` typeclass, available from the [mtl package](https://hackage.haskell.org/package/mtl). The instance implementation is as easy as:

```haskell
instance MonadState s (Parser s e) where
    get :: Parser s e s
    get = embed $ \ s -> Right (s, s)

    put :: s -> Parser s e ()
    put s = embed $ const (Right ((), s))
```

The first thing to notice is that both `get` and `put` do not error and `get` does not consume any input; the `put` parser however, allows arbitrary state transformations.

### A. 4. 2. The `try` parser.

The implementation of `try` is a standard try-catch, implemented directly:

```haskell
try :: Parser s e a -> Parser s Void (e :+: a)
try p = do
    xs <- get
    case parse p xs of
        Left e        -> throwError e
        Right (x, ys) -> put ys $> x
```

The `throwError e` fails unconditionally with error `e`:

```haskell
throwError :: e -> Parser s e a
throwError e = Parser $ \ _ -> Left e
```

### A. 4. 3. The `Alternative` instance.

We now have all the ingredients to implement choice via `(<|>)` of the `Alternative` typeclass: try the first parser and return its result; if it errors, backtrack and try the second parser. There is one issue to be solved however, namely, what to do if _both_ parsers error out? One obvious answer is "combine the errors" and "combine the errors" is a code word for a `Monoid` constraint on the error type `e`. With this setup:

```haskell
(<|>) :: Monoid e => Parser s e a -> Parser s e a -> Parser s e a
(<|>) p q = do
    x <- first absurd $ try p
    case x of
        Right x -> pure x
        Left e  -> do
            y <- first absurd $ try q
            case y of
                Right z -> pure z
                Left e' -> throwError $ e <> e'
```

The `empty` parser is also easily implemented with a call to `throwError` with the monoid unit for `e`. The monoid laws for `e` imply that with this structure `Parser s e a` is indeed a monoid, but as we will see next, it implies much more.

What does the constraint `Monoid e` mean in practice? Error types are usually plain data with no meaningful monoid operation. One of the most common things to do with an error is to just throw it away to a logger trash bin. But this, I contend, is a wrong way to look at the constraint. What the constraint really is, is a strategy for _accumulating errors_, e. g. maybe you need to gather them all in a list or keep the first one only.

### A. 4. 4. The `eitherP` parser and an alternative to `Alternative`.

With the `Alternative` typeclass, we can write the `either` parser, renamed as `eitherP` to avoid name clashes with base, that is more deserving of the name choice:

```haskell
eitherP :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (a :+: b)
eitherP p q = (Left <$> p) <|> (Right <$> q)
```

Uncurry-ing `either`, we get a natural transformation `(Parser s e a, Parser s e b) -> Parser s e (a :+: b)`. Since there is also a map

```haskell
unit :: Monoid e => () -> Parser s e Void
unit = const (throwError mempty)
```

this gives a (symmetric) lax-monoidal structure to `Parser s e` between products and coproducts [^4]. Considering the _codiagonal_,

```haskell
codiagonal :: a :+: a -> a
codiagonal = either id id
```

we have the equality

```haskell
(<|>) = fmap codiagonal . either
```

The upshot of all this is that the `Alternative` typeclass is a red herring, choice is really a lax-monoidal structure. Let us go through the steps one by one to make this clearer.

[^4]: The coherence laws do hold up; exercise to the interested reader.

#### A. 4. 4. 1. Monoidal categories and monoids.

Monoidal structures are to categories as monoids are to sets, that is, just as a monoid is a binary function satisfying some equational laws, a monoidal structure is a bifunctor `(:*:)` and an object `k`, the _unit_ object, together with natural isomorphisms

```haskell
associator  :: a :*: (b :*: c) -> (a :*: b) :*: c
unitorLeft  :: k :*: a -> a
unitorRight :: a :*: k -> a
```

satisfying a bunch of equations, the so called _coherence laws_ [^5].

The value of any abstraction is in the list of examples it covers and the things you can do with it, both the new concepts that can be expressed and the theorems that can be derived. Starting with the examples, the two most important for us, are the monoidal structures given by products and coproducts respectively. As for concepts that can be expressed, one can now generalize the notion of monoid to a monoid in a monoidal category. The classical notion is recovered by using the product monoidal structure.

<!-- The canonical tensor strengths. -->

[^5]: See [Monoidal categories](https://ncatlab.org/nlab/show/monoidal+category) for the full story.

#### A. 4. 4. 2. Why you never heard of monoids for coproducts.

We start with an existence theorem.

__Theorem__: The codiagonal `a :+: a -> a` together with the unique initial `absurd :: Void -> a` is a monoid for the coproduct monoidal structure.

__Proof__: standard exercise in universal property juggling.

Let us call this monoid structure on `a` the _trivial_ one.

__Theorem__: For each `a` there is only one monoid structure for coproducts, the trivial one.

__Proof__: Since `Void` is initial, there is only one function `Void -> a`. By the universal property of coproducts, a function `a :+: a -> a` is of the form `either f g` for functions `f :: a -> a` and `g :: a -> a`. Now plug this in the two identity laws to get `f = id` and `g = id`.

#### A. 4. 4. 3. Monoids and lax-monoidal functors.

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

#### A. 4. 4. 4. The punchline.

Combining sections [Why you never heard of coproducts](#a-3-4-2-why-you-never-heard-of-monoids-for-coproducts) and [Monoids and lax monoidal functors](#a-3-4-3-monoids-and-lax-monoidal-functors), we have that the functions,

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

#### A. 4. 4. 5. More laws.

Since every function is automatically a monoid morphism for the trivial `:+:`-monoid structures, it follows that for every function `f` and all parsers `p` and `q`:

```haskell
fmap f (p <|> q) = (fmap f p) <|> (fmap f q)
fmap f empty = empty
```

But this is also a consequence of naturality of `(<|>)` and thus a consequence of the free theorem [^7]. Slightly more substantive (because they do not follow from any free theorem) are the next two results.

__Theorem__: If `f :: d -> e` is a monoid morphism, then `first f :: Parser s d a -> Parser s e a` is a monoid morphism:

```haskell
first f empty == empty
first f (p <|> q) == (first f p) <|> (first f q)
```

__Proof__: proof by case analysis on the failures.

The second result are the compatibility laws between `<*>` and `<|>`. For that we first need a definition.

__Definition__: A monoid `e` is _idempotent_ if for every `x :: e`, `x <> x = x`.

__Theorem__: The `Alternative` instance for `Parser s e a` satisfies _left catch_, _left absorption_ and _left zero_ laws:

```haskell
pure x <|> p == pure x
empty <*> p == empty
empty >>= h == empty
```

If the monoid structure on the error type `e` is idempotent then it satisfies both _left_:

```haskell
f <*> (x <|> y) == (f <*> x) <|> (f <*> y)
```
__Proof__: Follows from the monadic definition of `(<*>)` as

```haskell
p <*> q = do
    f <- p
    x <- q
    pure (f x)
```

and doing a case by case analysis on the failures.

The right (no pun intended) versions of the laws are all violated, essentially because of short-circuiting.

As we will see in the next section, the monoid that we will use in the library is idempotent. Another important case is the case when all the error distinctions are erased and the trivial monoid `()` is picked for error type.

[^7]: See [Theorems for Free!](https://dl.acm.org/doi/pdf/10.1145/99370.99404).

## A. 5. Handling errors.

### A. 5. 1. The `MonadError` instance.

A parsing function has type `s -> e :+: (a, s)` with error type `e`. Error throwing and catching is captured in the `MonadError` typeclass from the [mtl package](https://hackage.haskell.org/package/mtl) and the instance for `Parser s e a` is readily given:

```haskell
instance MonadError e (Parser s e) where
    throwError :: e -> Parser s e a
    throwError e = Parser $ const (Left e)

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

### A. 5. 2. The `Monoid e` constraint.

As discussed in [The `Alternative` instance](#a-3-the-alternative-instance-choice-and-backtracking), the `Alternative` typeclass requires a `Monoid e` constraint on the error type `e` that determines how errors combine, or as we termed it, the error accumulation strategy. There are two basic options: either errors accumulate in some container like a list or the parsers short-circuit on the first error. Short-circuiting completely determines the monoid operation:

```haskell
(<>) :: Eq e => e -> e -> e
(<>) x y
    | x == mempty = y
    | otherwise   = x
```

One advantage of the short-circuiting strategy is that the monoid is idempotent guaranteeing stronger laws for the `Alternative` instance -- see section [More laws](#a-3-4-5-more-laws).

### A. 5. 3. First attempt.

The `ParseError e` type is a thin wrapper around `e`, the _error tag_, to implement the short-circuiting strategy:

```haskell
data ParseError e
    = Failure
    | ParseError !e
    deriving stock (Eq, Show, Functor)
```

For the `Monoid` instance, we have as discussed above in [The `Monoid e` constraint](#a-4-2-the-monoid-e-constraint):

```haskell
instance Semigroup (ParseError e) where
    (<>) :: ParseError e -> ParseError e -> ParseError e
    (<>) Fail x = x
    (<>) x    _ = x

instance Monoid (ParseError e) where
    mempty :: ParseError e
    mempty = Fail
```

A little bit of staring and the reader should be able to convince himself that this type is monoid-isomorphic to `Maybe (First a)` with `First a` the newtype-wrapper from base with semigroup operation pick-the-first-element. The `Maybe` functor then freely adds the monoid unit. From this isomorphism, it follows that:

__Theorem__: for every `f :: d -> e`, `fmap f :: ParseError d -> ParseError e` is a monoid morphism.

The theorem is an implication, not an iff. The constant map `const Fail :: ParseError d -> ParseError e` is a monoid morphism. For a minimal example of a non-monoid morphism, let `y, y' :: d` be two distinct non-identity elements and `z :: e` a non-identity element, then:

```haskell
f :: Eq d => ParseError d -> ParseError
f x
    | x == y    = z
    | otherwise = mempty 
```

Now, `f (y' <> y)` and `f y' <> f y` are not equal.
