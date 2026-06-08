# A. Parsing.

## A. 1. Parsing functions and the `Parser s e a` type.

A _parsing function_ is a value of type,

```haskell
type Parser s e a = s -> e :+: (a, s) 
````

where `:+:` is just a type operator alias for `Either` to make signatures look prettier. It should be read as: a parsing function for values `a` with error type `e` on _input streams_ `s`. What an input stream is exactly is to be decided later, but typical examples to have in mind are types like `[a]`, `ByteString` and `Text`.

Wrapping in a newtype:

```haskell
newtype Parser s e a = Parser (s -> e :+: (a, s))
    deriving stock Functor
````

There are two basic functions, one to embed a parsing function in `Parser` and another to run the parser on the input:

```haskell
{- | Embed a parsing function in 'Parser'. -}
embed :: (s -> e :+: (a, s)) -> Parser s e a
embed = Parser

{- | Run the parser on the input and return the results. -}
run :: Parser s e a -> s -> e :+: (a, s)
run (Parser p) = p
```

Note that `run` is the inverse to `embed`. Note also that, as the type signature of a parsing function implies, a parser is all-or-nothing: _either_ it throws an error _or_ (exclusive or) it succeeds, returning the pair of the parsed result and the rest of the input.

At this point, we note that `Parser` could be generalized to a transformer by,

```haskell
-- The ParserT monad transformer.
data ParserT m s e a = Parser (s -> m (e :+: (a, s)))

-- The Parser monad.
type Parser = ParserT Identity
```

as is done in say, the [Megaparsec library](https://hackage.haskell.org/package/megaparsec). In this library, we explicitly do _not_ make such a generalization; all code is pure. This design forces library users to construct the parser and stuff it somewhere, gather the input from the IO layer and apply the parser. For the cases where the input must be consumed incrementally, some scheme using a streaming library can be bolted on top.

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

The type constructor `Parser s e a` is a functor in `a` and this is stock derivable. Other instances provide extra structure. The `Bifunctor` instance provides functoriality in the error type and is the basic error handling mechanism. The `Applicative` instance allows to construct a parser for `(a, b)` from parsers for `a` and `b`; more generally, it allows the construction of parsers for product types from parsers for the fields. Finally, the `Monad` instance allows us to feed the result of a parser to another.

It should be noted that none of these instances require constraints on the input stream type `s` or the error type `e`.

### A. 3. 1. The `Bifunctor` instance.

Functoriality in the error type is given by the `Bifunctor` instance.

```haskell
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap f g p = embed $ \ s -> bimap f (first g) $ run p s
```

Bifunctoriality provides the basic way to unify error types. If we have parsers `p_i :: Parser s e_i a` and a cospan `f_i :: e_i -> e`, then we have a cospan `first f_i :: Parser s e_i a -> Parser s e a`. The choice of `e` is left to the user, but there is a canonical, minimal choice by taking the coproduct of all `e_i` [^1]. It follows that, given a finite collection of parsers `p_i`, we can assume without loss of generality that they have the same error type `e`.

[^1]: Unfortunately, dealing with arbitrary coproducts in Haskell is clunky, even if we reached for any of the innumerable packages on hackage offering extensible sum types. For just one example among many, see [sop-core](https://hackage.haskell.org/package/sop-core).

### A. 3. 2. The `Applicative` instance.

We can endow `Parser s e a` with further structure, starting with the `Applicative` instance.

```haskell
instance Applicative (Parser s e) where
    pure :: a -> Parser s e a
    pure x = Parser $ \ s -> pure (x, s)

    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = Parser $ \ s -> do
        (f, t) <- run p s
        (x, u) <- run q t
        pure (f x, u)
```

#### A. 3. 2. 1. The parser `pure x`.

For a value `x`, the parser `pure x` does not error and _never consumes input_. The two are linked because if a parser does not error then it must return something when there is no input to be had, which of course requires that it _does not consume input_.

To formalize these definitions, we start by introducing the _remainder_ function that returns the rest of the input on a successful parse:

```haskell
remainder :: Parser s e a -> s -> e :+: s
remainder p = fmap snd . run p
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

One could retort that being fully polymorphic in the error type `e` already implies that the parser cannot throw an error, since it is not possible to create values of `e` ex-nihilo; after all, `e` could well be uninhabited as is the case with `e ~ Void`. That much is true, but it is still valuable to signal such, and signal it loudly. So where possible, if a parser does not error it will be reflected in the type signature, at the cost of having to litter the code with `first absurd` calls to satisfy the type checker. Note that while we want the error type as monomorphic and concrete as possible, the exact opposite happens with the value type; we want it as polymorphic and general as possible.

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

In category-theoretic language, every functor is colax-monoidal for products and lax-monoidal for coproducts [^2]. This leads us to an equivalent description of the `Applicative` typeclass.

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

<!-- TODO: The canonical tensor strengths. -->

[^3]: See for example [Notions of computation as monoids](https://arxiv.org/abs/1406.4823) and references therein.

#### A. 3. 2. 4. Parsers for product types.

Using the `Applicative` instance, we have parser combinator that for parsers for types `a` and `b` gives us a parser for `(a, b)`:

```haskell
pair :: Parser s e a -> Parser s e b -> Parser s e (a, b)
pair p q = (,) <$> p <*> q
```

We named this parser combinator `pair` instead of `zip` to avoid clashes with base. More generally, given a product type `T a_0 ... a_n` and parsers `p_i :: Parser s e a_i` we have a parser

```haskell
p_T :: Parser s e T
p_T = T <$> p_0 <*> ... <*> p_n
```

Looking at the code, we are applying the parsers `p_i` in succession and then the constructor `T` to the resulting n-tuple. In particular, we are assuming a canonical format where the fields of `T` are laid out in succession.

Similarly, the canonical parser for `()` is simply `pure ()` which implies that in the canonical format `()` takes up no space. This is a consequence of the fact that `()` is the unit for the monoidal product structure. In particular, the canonical format is not self-documenting.

### A. 3. 3. The `Monad` typeclass.

The `Monad` instance is also readily given.

```haskell
instance Monad (Parser s e) where
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = embed $ \ s -> do
        (x, t) <- run p s
        run (h x) t
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

    * In all these laws, it is understood that, unless explicitly said otherwise, free variables are universally quantified.

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

The first thing to notice is that both `get` and `put` do not error and `get` does not consume any input. The `put` parser however, allows _arbitrary_ state transformations. We will see below that this has some unfortunate implications, but the parser is still provided as an escape hatch to allow making new primitive parsers.

### A. 4. 2. The `try` parser.

The implementation of `try` is a standard try-catch, implemented directly:

```haskell
try :: Parser s e a -> Parser s Void (e :+: a)
try p = do
    xs <- get
    case run p xs of
        Left e        -> throwError e
        Right (x, ys) -> put ys $> x
```

The `throwError e` fails unconditionally with error `e`:

```haskell
throwError :: e -> Parser s e a
throwError e = embed $ const (Left e)
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

Uncurry-ing `eitherP`, we get a natural transformation `(Parser s e a, Parser s e b) -> Parser s e (a :+: b)`. Since there is also a map

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
(<|>) = fmap codiagonal . eitherP
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

Combining sections [Why you never heard of coproducts](#a-4-4-2-why-you-never-heard-of-monoids-for-coproducts) and [Monoids and lax monoidal functors](#a-4-4-3-monoids-and-lax-monoidal-functors), we have that the functions,

```haskell
u :: Monoid e => (Parser s e a, Parser s e b) -> Parser s e (a :+: b)
u = uncurry (eitherP)

e :: Monoid e => () -> Parser s e Void
e = unit
```

make `Parser s e` a lax monoidal functor between `(,)` and `:+:`. By the preservation theorem, the functions

```haskell
m :: Monoid e => (Parser s e a, Parser s e a) -> Parser s e a
m = fmap codiagonal . uncurry (eitherP)

v :: Monoid e => () -> Parser s e Void
v = const (throwError mempty)
```

give a monoid structure on `f a`, which is just the `Alternative` instance minus the currying.

#### A. 4. 4. 5. More laws.

Since every function is automatically a monoid morphism for the trivial `:+:`-monoid structures, it follows that:

```haskell
fmap f (p <|> q) = (fmap f p) <|> (fmap f q)
fmap f empty = empty
```

But this is also a consequence of naturality of `(<|>)` and thus a consequence of the free theorem [^7]. Slightly more substantive are the next two results.

__Theorem__: If `f :: d -> e` is a monoid morphism, then `first f :: Parser s d a -> Parser s e a` is a monoid morphism:

```haskell
first f empty == empty
first f (p <|> q) == (first f p) <|> (first f q)
```

__Proof__: proof by case analysis on the failures.

The second result are the compatibility laws between `<*>` and `<|>`. For that we first need a definition.

__Definition__: A monoid `e` is _idempotent_ if for every `x :: e`, `x <> x = x`.

__Theorem__: The `Alternative` instance for `Parser s e a` satisfies the _left catch_, the _left absorption_ and the _left zero_ laws:

```haskell
pure x <|> p == pure x
empty <*> p == empty
empty >>= h == empty
```

If the monoid structure on the error type `e` is idempotent then it also satisfies _left distributivity_:

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

The right (no pun intended) versions of the laws are all violated, essentially because of short-circuiting. As we will see in the next section, the monoid that we will use in the library is idempotent. Another important case is the case when all the error distinctions are erased and the trivial monoid `()` is picked for error type.

[^7]: See [Theorems for Free!](https://dl.acm.org/doi/pdf/10.1145/99370.99404).

## A. 5. Handling errors.

### A. 5. 1. The `MonadError` instance.

A parsing function has type `s -> e :+: (a, s)` with error type `e`. Error throwing and catching is captured in the `MonadError` typeclass from the [mtl package](https://hackage.haskell.org/package/mtl) and the instance for `Parser s e a` is readily given:

```haskell
instance MonadError e (Parser s e) where
    throwError :: e -> Parser s e a
    throwError e = embed $ const (Left e)

    catchError :: Parser s e a -> (e -> Parser s e a) -> Parser s e a
    catchError p h = embed $ \ s ->
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
catch p h = embed $ \ s ->
    case run p s of
        Left e  -> run (h e) s
        Right p -> Right p
```

`catch` and `throwError` have the right shape for a monad structure for `ParseError s e a` in the error type `e` but it is not difficult to see that, essentially because of short-circuiting, while it satisfies the identity laws, associativity is violated.

### A. 5. 2. The `Monoid e` constraint.

As discussed in [The `Alternative` instance](#a-4-3-the-alternative-instance), the `Alternative` typeclass requires a `Monoid e` constraint on the error type `e` that determines how errors combine, or as we termed it, the error accumulation strategy. There are two basic options: either errors accumulate in some container like a list or the parsers short-circuit on the first error. Short-circuiting completely determines the monoid operation:

```haskell
(<>) :: Eq e => e -> e -> e
(<>) x y
    | x == mempty = y
    | otherwise   = x
```

One advantage of the short-circuiting strategy is that the monoid is idempotent guaranteeing stronger laws for the `Alternative` instance -- see section [More laws](#a-4-4-5-more-laws).

### A. 5. 3. First attempt.

The `ParseError e` type is a thin wrapper around `e`, the _error tag_, to implement the short-circuiting strategy:

```haskell
data ParseError e
    = Failure
    | ParseError !e
    deriving stock (Eq, Show, Functor)
```

For the `Monoid` instance, we have as discussed above in [The `Alternative` instance](#a-4-3-the-alternative-instance):

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

## A. 6. Typeclasses for the input.

We are at the point where we can finally tackle the constraints needed for the input type `s`; after all, at this point we cannot even get out one element from the input stream.

### A. 6. 1. One out of `s`: the `Streamable` typeclass.

What we need from `s` is an `uncons` operation, returning the pair of the head and tail of the stream.

```haskell
{- | The @Streamable@ typeclass of input streams @s@ yielding @a@ elements. -}
class Streamable a s | s -> a where
    {- | Uncons the first element from the input stream. -}
    uncons :: s -> Maybe (a, s)
```

We use functional dependencies to tie the element type @a@ with the stream type @s@, but it could also be done with an associated family type. The reason we chose functional dependencies is that in our experience, inference tends to work better.

### A. 6. 2. The `MonoFunctor` constraint.

All the paradigmatic examples of input streams like `ByteString` and `Text` have a `map`-like operation, a monomorphic variant of `fmap`. The `MonoFunctor` typeclass captures this;

```haskell
class MonoFunctor a s | s -> a where
    {- | Map over an element of the monofunctor. -}
    monomap :: (a -> a) -> s -> s

-- Instances.
instance MonoFunctor a [a] where
    monomap :: (a -> a) -> [a] -> [a]
    monomap = fmap
```

There are corresponding instances for types like `ByteString` and `Text`, but in this document, if we need some concrete stream type we will just defer to something like `[a]`.

The `MonoFunctor` typeclass, as well as monomorphic versions of `Foldable` and `Traversable` can be found in the [mono-traversable package](https://hackage.haskell.org/package/mono-traversable). We have our own, slightly different, version available from the [mono github repository](https://github.com/lambda-dom/mono).

`MonoFunctor` is not a terribly useful typeclass but it ends up being important as a base and to state some of the typeclass laws. With this superclass:

```haskell
{- | The @Streamable@ typeclass of monomorphic, input streams. -}
class MonoFunctor a s => Streamable a s where
    {- | Uncons the first element from the input stream. -}
    uncons :: s -> Maybe (a, s)
```

### A. 6. 3. No free laws.

Since monofunctors `s` are not polymorphic in `a`, there are no free theorems available and equational laws like naturality must be explicitly required.

__Definition__: Let `s` and `t` be two monofunctors `MonoFunctor a s` and `MonoFunctor a t`. A function `h :: s -> t` is _mononatural_ if for every `f :: a -> a` we have the equality `monomap f . h = h . monomap f`.

notes(s):

    * There are equivalent descriptions of monofunctoriality and mononaturality in terms of monoid actions, but these trivial reformulations do not yield anything important for our purposes.

The first law for `Streamable` is that `uncons` is mononatural. In case it is not clear, the `MonoFunctor` instance of the codomain is

```haskell
instance MonoFunctor a s => MonoFunctor a (Maybe (a, s))
    monomap :: (a -> a) -> Maybe (a, s) -> Maybe (a, s)
    monomap f = fmap (bimap f (monomap f))
```

### A. 6. 4. Two fundamental parsers.

With the `Streamable` typeclass we can now extract one element from the input stream and also check if the input stream has more elements to yield.

```haskell
{- | Error thrown when input stream is exhausted. -}
data InputError = InputError
    deriving stock (Eq, Show)


eoi :: Streamable a s => Parser s Void Bool
eoi = null <$> get

one :: Streamable a s => Parser s InputError a
one = do
    xs <- get
    case uncons xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> throwError InputError
```

### A. 6. 5. Some definitions.

Recall that the remainder of a parser's input can be obtained via

```haskell
remainder :: Parser s e a -> s -> Maybe s
remainder p = either (const Nothing) id . fmap snd . run p
```

What is the relation of the remainder with the original input, if any?

__Definition__: A parser `p :: Parser s e a` is _normal_ if for every `xs :: s`, on success, the remainder is a (possibly improper) suffix of `xs`.

With the `Streamable` typeclass, the is-suffix relation is simply `isSuffixOf` at the level of lists.

```haskell
isSuffixOf :: Streamable a s => s -> s -> Bool
isSuffixOf xs ys = (toList xs) `isSuffixOf` (toList ys)
```

In this library, all parsers are normal and all parser combinators are normal on the assumption that the argument parsers are normal. Since the `Parser` constructor is not exported, the _only_ way to construct non-normal parsers is by using `put`.

### A. 6. 6. The (absence of the) `MonoFoldable` constraint.

Given the `uncons` operation, we can define a conversion to lists,

```haskell
toList :: Streamable a s => s -> [a]
toList = unfoldr uncons
```

so that `Streamable` could have `MonoFoldable` as a superclass. There are two main reasons why `MonoFoldable` is not a superclass:

    1. For infinite lists, `monolength` diverges and we _do_ want infinite lists and other stream-like objects to be instances of `Streamable`.

    2. For input streams like `ByteString.Lazy.ByteString` computing its length forces the entire bytestring into memory which is a big no-no.

Of course, _if_ `s` is an instance of `MonoFoldable` then the equality should hold and this is the second law for `Streamable`.

### A. 6. 7. Conversion to lists and indexed streams.

### A. 6. 8. The `HasPosition` typeclass and `ParseError` revisited.

### A. 6. 9. A Rant over `Int` indexes and lengths.

## A. 7. Prefixes and the `Splittable` typeclass.

The `Streamable` typeclass allows to write down all commonly used parsers, but alas, since we can only get one element from the input stream at a time the implementations can be very inefficient. What we need is a notion of chunk, or _stream prefix_, and methods to cut out prefixes from streams.

### A. 7. 1. The `Splittable` class.

Hence the `Splittable` typeclass.

```haskell
class Streamable a s => Splittable a b s where
    {-# MINIMAL splitAt, splitWith #-}

    {- | Split the stream at index @n@ into a pair @(prefix, suffix)@. -}
    splitAt :: Int -> s -> (b, s)

    {- | Split the stream into a pair @(prefix, suffix)@ using a predicate @p@.

    @prefix@ is the longest prefix whose elements satisfy @p@ and @suffix@ is the remainder. -}
    splitWith :: (a -> Bool) -> s -> (b, s)
```

### A. 7. 2. The laws.

To state the laws, we must assume something of the prefix `b` that is not expressed directly in the typeclass. The first constraint is `MonoFunctor a b` that `b` is a monofunctor with the same type of elements as `s`. With this assumption: for every `n`, `splitAt n` is mononatural.

note(s):

    * `splitWith p` is _not_ mononatural. There is an example in the haddocks for the typeclass.

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

so it is not much of a stretch to assume that prefixes can be converted to lists. Note that the arguments above for not requiring a `MonoFoldable a s` constraint on `s` do _not_ apply, that is, we are implicitly assuming that prefixes are indeed _finite_ monofoldables, in part because some important parsers with a `Splittable a b s` constraint require computing the lengths of prefixes. Therefore, assuming a further `MonoFoldable a b` constraint, which is satisfied by all the `Splittable` instances defined by the library, the second typeclass law just says that at the level of lists `splitAt` is `splitAt` and `splitWith` is `span`:

```haskell
bimap monotoList toList . splitAt n = splitAt n . toList
bimap monotoList toList . splitWith p = span p . toList
```

The third and final law is a compatibility condition between `uncons` and `splitAt`. Specifically, `uncons` and `splitAt 1` are, minus how they handle the case of an empty stream, the same:

```haskell
maybe [] (bimap singleton toList) . uncons = bimap toList toList . splitAt 1
```

# B. Serializing.

## B. 1. First attempt.

A first solution is to carry the codomain `s` of a serializer in a `Writer` monad as is done in say, the [cereal package](https://hackage.haskell.org/package/cereal).

```haskell
data Writer m a = Writer m a
    deriving stock Functor


-- Instances.
instance Monoid m => Applicative (Writer m) where
    pure :: a -> Writer m a
    pure = Writer mempty

    (<*>) :: Writer m (a -> b) -> Writer m a -> Writer m b
    (<*>) (Writer m f) (Writer n x) = Writer (m <> n) (f x)

instance Monoid m => Monad (Writer m) where
    (>>=) :: Writer m a -> (a -> Writer m b) -> Writer m b
    (>>=) (Writer m x) h = let (Writer n y) = h x in Writer (m <> n) y 


-- Basic functions.
run :: Writer m a -> (m, a)
run (Writer m x) = (m, x)
```

As is obvious from the code block, `Writer m a` is isomorphic to `(m, a)`. `m` is a monoid with a "cheap" `(<>)` operation, typically a builder for some underlying stream like `ByteString` or `Text`.

We get, from the `Writer` instances alone, a lot of power. For example, the zip combinator allows us to combine writers for `a` and `b` to a writer for `(a, b)`. Expanding the definition:

```haskell
pair :: Writer m a -> Writer m b -> Writer m (a, b)
pair p q = do
    x <- p
    y <- q
    pure (x, y)
```

The `m` accumulator is threaded around by `Writer` in such a way that it disappears from sight. We can recover it by running the writer and applying `fst` to the results.

## B. 2. Two arguments.

There are two main arguments against this implementation. The first is that the `Writer` monad leaks -- see [Issues with monad transformers](https://github.com/haskell-effectful/effectful/blob/master/transformers.md). More importantly for us however, is the conceptual reason: the implementation does not align with basic intuitions about serialization. The result `m` is _hidden_ in the computation when it is the whole point of it. As a consequence, the construction of the serializer for the whole from the parts constructs in parallel the whole object because the constructors are invoked on the way -- e. g. the serializer for `(,)` calls the `(,)` constructor -- but this gets things backwards.

## B. 3. Serializers in the corepresentable representation.

We arrive at the representation of a serializer as a function `a -> s` that given a value `x :: a` returns an `s` encoding `x`.

```haskell
newtype Serializer s a = Serializer (a -> s)
```

Entirely parallel to parsers, we have a pair of mutually inverse functions to embed a serializing function and run a serializer:

```haskell
{- | Embed a serializing function in 'Serializer'. -}
embed :: (a -> s) -> Serializer s a
embed = Serializer

{- | Run the serializer on the input @x :: a@ and return the results. -}
run :: Serializer s a -> a -> s
run (Serializer f) = f
```

It is naturally to think of `s` as an _output stream_ or _sink_, but what this means exactly is to be decided later. The typical examples to have in mind are, as with parsers, types such as `[a]`, `ByteString` and `Text`.

The first and more obvious difference with parsers, is that serializers are total and never error. The second difference is that a serializer is contravariant in `a`. `Contravariant` is the typeclass from base formalizing this.

```haskell
instance Contravariant (Serializer m) where
    contramap :: (a -> b) -> Serializer m b -> Serializer m a
    contramap f s = embed $ run s . f
```

## B. 4. Instances.

Similarly to what we have done with parsers, we now discuss the basic typeclass instances for `Serializer` that will allow us to construct serializers for complex types from more primitive ones.

### B. 4. 1. The lax monoidal structure for products.

As mentioned in [Equivalent description of `Applicative`](#a-3-2-3-equivalent-description-of-applicative), the applicative structure is equivalent to a lax-monoidal structure for the product monoidal structure. The corresponding structure for serializers is also a lax-monoidal structure:

```haskell
zip :: Monoid s => Serializer s a -> Serializer s b -> Serializer s (a, b)
zip s t = embed $ uncurry (<>) . bimap (run s) (run t)

unit :: Monoid s => Serializer s ()
unit = embed $ const mempty
```

This is encoded in the `Divisible` typeclass from the [contravariant package](https://hackage.haskell.org/package/contravariant).

```haskell
instance Monoid s => Divisible (Serializer s) where
    conquer :: Serializer s a
    conquer = embed $ const mempty

    divide :: (c -> (a, b)) -> Serializer s a -> Serializer s b -> Serializer m c
    divide f s t = embed $ g . f
        where
            g = uncurry (<>) . bimap (run s) (run t)
```

### B. 4. 2. Serializer for products.

Consider the case of a product type, a type of the form

```haskell
data T a_0 ... a_n = T a_0 ... a_n
```

Assume there are serializers `Serializer s a_i` with `i` ranging from `0` to `n`. As mentioned in the section [Parsers for product types](#a-3-2-4-parsers-for-product-types), a natural idea for a format for `T` is to lay out the fields `a_i` consecutively one after another. So a serializer for `T` is, denoting by `p_i :: T a_0 ... a_n -> a_i` the field projections,

```haskell
s_T :: Serializer s (T a_0 ... a_n)
s_T = s_0 (p_0 x)
    <> ...
    <> s_n (p_n x)
```

Note the duality in constructing serializers and parsers: for the parser we use the constructor to synthesize the whole from the parts, while for the serializer we use the field projections, or the eliminators, to synthesize the whole from the parts.

### B. 4. 3. The lax-monoidal structure for coproducts.

As seen in the section [The `Alternative` instance](#a-4-3-the-alternative-instance), the `Alternative` instance is equivalent to a lax-monoidal structure from products to coproducts. The corresponding in the serializer world is:

```haskell
eitherS :: Monoid s => Serializer s a -> Serializer s b -> Serializer s (a :+: b)
eitherS s t = embed $
    \case
        Left x  -> (run s) x
        Right y -> (run t) y

empty :: Serializer m Void
empty = embed absurd
```

Using the `Decidable` typeclass, also from the [contravariant package](https://hackage.haskell.org/package/contravariant):

```haskell
instance Monoid s => Decidable (Serializer s) where
    lose :: (a -> Void) -> Serializer s a
    lose f = embed $ absurd . f

    choose :: (a -> b :+: c) -> Serializer s b -> Serializer s c -> Serializer s a
    choose f s t = embed $ choice (run s) (run t) . f
        where
            -- The Representability isomorphism.
            choice :: (a -> m) -> (b -> m) -> (a :+: b) -> m
            choice p q
                = \case 
                    Left x  -> p x
                    Right y -> q y
```

### B. 4. 4. The `Monoid` instance.

There is also a `Monoid` instance for `Serializer s a` that allows us to combine two serializers.

```haskell
instance Semigroup s => Semigroup (Serializer s a) where
    (<>) :: Serializer s a -> Serializer s a -> Serializer s a
    (<>) s t = embed $ \ x -> run s x <> run t x

instance Monoid s => Monoid (Serializer s a) where
    mempty :: Serializer s a
    mempty = embed $ const mempty
```

If we look at the code for `Divisible` in section [The Lax Monoidal Structure for Products](#b-4-1-the-lax-monoidal-structure-for-products), one can see that this monoid structure allows us to replace uses of it in a way analogous to how `<*>` allows us to seemlessly extend a binary parser combinator to an n-ary parser combinator.

One important property this monoid structure is that it is natural in `a`, and thus a monoid morphism:

__Theorem__: For every `f :: a -> b`, we have the equalities:

```haskell
contramap f mempty == mempty
contramp f (s <> t) == contramap f s <> contramap f t 
```

Because of this property, it could be argued that the monoid instance is the analog of the `Alternative` instance for parsers. But as seen in section [The `Alternative` instance](#a-4-3-the-alternative-instance), the `Alternative` instance is best viewed as a lax-monoidal structure from products to coproducts, and in this form, the serializer analog is given below in section [The lax-monoidal structure for coproducts](#b-4-3-the-lax-monoidal-structure-for-coproducts). But the fundamental break down in the analogy is that there is no error handling needed for serializers; and since there is no error handling, no backtracking is needed; and since there is no analog of backtracking, there is no analog of _choice_.

### B. 4. 5. The left action.

Closely related to the monoid instance is the left action of `s` on `Serializer s a`:

```haskell
(|*>) :: Monoid s => s -> Serializer s a -> Serializer s a
(|*>) m s = embed $ \ x -> m <> run s x
infixr 5 |*>
```

__Theorem__: `(|*>)` is a left `m`-action on `Serializer m a`, that is, it satisfies the equalities:

```haskell
m <> n |*> s == m |*> n |*> s
mempty |*> s == s
```

This also gives us the occasional useful function `collapse`, that allows us to embed `m` in `Serializer m a`:

```haskell
collapse :: Monoid s => s -> Serializer s a
collapse m = m |*> mempty
```

### B. 4. 6. Serializer and parser for coproducts.

Now consider the case of a coproduct, a type of the form

```haskell
data T a_0 ... a_n
    = T_0 a_0
    ...
    | T_n a_n
```

The first thing to notice is that the general case of a constructor of the form `T_i b_0 ... b_n_i` can be reduced to the one-argument case, by setting `a_i ~ (b_0, ..., b_n_i)` and using the constructions of section [Serializer for products](#b-4-2-serializer-for-products).

Assuming the existence of serializers `s_i :: Serializer s a_i` with `i` ranging from `0` to `n`, a natural format for `T a_0 ... a_n` is to first have a discriminating tag followed by the encoding of the relevant value. The tag can be implemented simply by enumerating the constructors top to bottom and return the corresponding ordinal:

```haskell
tag :: T a_0 ... a_n -> Word
tag x = case x of
    T_0 _ -> 0
    ...
    T_n _ -> n
```

This piece of bloatware can even be derived automatically using something like the [generics-sop library](https://hackage.haskell.org/package/generics-sop) or template Haskell, but we will not dwell on this detail here.

note(s):

  * The serializing format using the `tag` function is vulnerable to changes in `T` like reordering or adding new constructors. How this can be solved is a whole different problem.

Assuming the existence of a primitive serializer `word :: Serializer s Word`, we now have:

```haskell
s :: Serializer s (T a_0 ... a_n)
s = Serializer $ x ->
    word (tag x) |*>
        case x of
            T_0 x_0 -> s_0 x0
            ...
            T_n x_n -> s_n x_n 
```

The `case` statement is just an expansion of the generic eliminator for `T`, `either s_0 ... s_n`.

Dually, assume the existence of parsers `p_i :: Parser s e a_i`. Also assume the existence of a primitive parser `word :: Parser s e Word`. Then the parser for this format is just:

```haskell
parser :: Parser s e (T a_0 ... a_n)
parser =
    word >>=
        \case
            0 -> fmap T_0 p_0
            ...
            n -> fmap T_n p_n
            _ -> throwError e
```

Once again we see the duality: on the parser side we have the `Monad` bind combinator sequencing the two parsers, while on the serializer side that role is played by the left action `(|*>)` operator. On the parser side, we do a case analysis on the constructor tag and call the appropriate constructor on the appropriate parser, adding a default error branch, while on the serializer side we use the eliminator to dispatch on the appropriate serializer.

### B. 4. 7. Serializer for sequences.

We give one more example of constructing a prism for the whole from a prism for the parts. Starting with lists `[a]`, a format for serializing a list `[a]`, given a serializer `s :: Serializer s a` for `a`, is to first serialize the length, then repeatedly apply `s` to the elements of `t a`.

```haskell
count :: Serializer s a -> Serializer s [a]
count s = embed $ xs ->
    word (length xs) |*> foldmap (run s) xs
```

For a general foldable `t`, we need to first be able to construct a `t` from a list.

__Definition__: A foldable `f` is a _sequence_ if `toList :: f a -> [a]` is an isomorphism.

We denote the inverse of `toList` by `fromList`. Examples of sequences include `[a]`, `Vector a` and `Seq a`; `Ord a => Set a` is _not_ a sequence, because `toList` returns the list of elements in ascension-key order and therefore `fromList` is only a left inverse. A second related reason is that `Set a` does not have a `Functor` instance. With the `fromList` inverse, we can simply apply contravariance:

```haskell
sequence :: Foldable t => Serializer s a -> Serializer s (t a)
sequence s = contramap fromList s
```

With the `fromList` inverse, and assuming the existence of a parser `p :: Parser s e a`, we have,

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
