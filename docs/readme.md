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

#### A. 3. 1. 2. Errors and `Bifunctor`.

The attentive reader will surely have noticed that while the parsers `p_i` have types ` s e_i a_i`, the argument parsers for `(<*>)` are required to all have the same type. So at a minimum, and assuming we have conversion functions `f_i :: e_i -> e` for some type `e`, we need a `Bifunctor` instance for `Parser s e a` to write

```haskell
p :: Parser s e (T a_0 ... a_n)
p = T <$> (first f_0 p_0) <*> ... <*> (first f_n p_n)
```

The choice of `e` is left to the user, but there is a canonical, minimal one: take the coproduct of all the `e_i`. In the section [On Errors](#a-4-on-errors) we will see another way to deal with this problem that does not rely on coming up with a cone `e_i -> e`.

Note that as with the input type `s`, there are still no constraints on the error type `e`.

#### A. 3. 1. 3. The parser `pure x`.

File(s):

  * [Combinators.hs](../src/Trisagion/Parsers/Combinators.hs)

For a value `x`, the parser `pure x` does not throw an error and never consumes input. The two are inextricably linked, because if a parser does not throw an error then it must do something when there is no input to be had, which of course, requires that it does not consume input. However, even if the two are linked only the former can be reflected in the type signature, e. g. by:

```haskell
pure :: a -> Parser s Void a
```

Since the type signature of `pure` is already fixed, we instead provide an error-free version of it:

```haskell
value :: a -> Parser s Void a
value = pure
```

One could also retort that being fully polymorphic in the error type `e` implies that the parser cannot throw an error, since it is not possible to create values of `e` ex-nihilo, none are provided and Haskell is pure. That much is true, but it is still valuable to signal such, and signal it loudly, to the users. So where possible, if a parser does not throw an error it will be reflected in the type signature, at the cost of having to litter the code with `first absurd` calls to satisfy the type checker.

#### A. 3. 1. 4. Equivalent descriptions of `Applicative`.

As is well known, the `Applicative` typeclass is equivalent to `f` being lax-monoidal for products [^1]:

```haskell
zip :: Applicative f => f a -> f b -> f (a, b)
zip p q = (,) <$> p <*> q

unit :: Applicative f => () -> f ()
unit = pure
```

Given the latter, then `<*>` of `Applicative` can be regained,

```haskell
(<*>) :: f (a -> b) -> f a -> f b
(<*>) = zip . (uncurry ($))
```

and `pure` is,

```haskell
pure :: a -> f a
pure x = fmap (point x) . unit . terminal
```

with `terminal` the unique function `a -> ()` given by `const ()` and `point` the isomorphism `a -> (() -> a)` given by `\ x -> const x`. The laws for the typeclasses guarantee that we get the same results either way.

The reason for this piece of category-theoretic trivia is that once we get to serializers, we will see that it is the lax-monoidal version of `Applicative` that dualizes well.

note(s):

  * There is a third equivalent description of `Applicative` as monoids for the monoidal structure of Day convolution, but Day convolution is a sophisticated category-theoretic device that for our particular purposes does not add anything new.

[^1]: See for example [Lax monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor).

#### A. 3. 1. 5. Zipping and unzipping.

The universal properties yield canonical maps,

```haskell
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip = fmap fst &&& fmap snd

cozip :: Functor f => Either (f a) (f b) -> f (Either a b)
cozip = either (fmap Left) (fmap Right)
```

where `(&&&)` is the representability isomorphism implied by the universal property of products:

```haskell
(&&&) :: (c -> a) -> (c -> b) -> c -> (a, b)
(&&&) f g x = (f x, g x)
```

In category-theoretic language, every functor is colax-monoidal for products and lax-monoidal for coproducts [^2].

If `unzip` and `terminal :: f () -> ()` are isomorphisms then `f` is said to _preserve products_. `Parser s e a` does _not_ preserve products; we will not show this (it is not very difficult anyways) but it is an instructive exercise to see that `zip`, or more precise its uncurried version, is _not_ an inverse of `unzip`, and the proof works for every monad. First, `zip` can be re-defined -- see [The Monad typeclass](#a-3-2-the-monad-typeclass) for the `Monad` instance for `Parser s e a` -- as

```haskell
zip :: Monad f => f a -> f b -> f (a, b)
zip p q = do
    x <- p
    y <- q
    pure (x, y)
```

Then,

```haskell
uncurry zip . unzip p = do
    x <- fst <$> p
    y <- snd <$> p
    pure (x, y)
```

which is equal to `p` only if running `p` is idempotent. For the other direction,

```haskell
unzip . uncurry zip (p, q)
    = unzip $ zip p q
    = unzip $ do
        x <- p
        y <- q
        pure (x, y)
    = (p', q')
        where
            p' = do
                x <- p
                _ <- q
                pure x

            q' = do
                _ <- p
                y <- q
                pure y
```

which is clearly not equal to `(p, q)`.

[^2]: The verification of the required coherence laws is a straightforward, albeit tedious, exercise best left to the interested reader.

### A. 3. 2. The `Monad` typeclass.

The `Monad` instance for `Parser s e a` is readily given:

```haskell
instance Monad (Parser s e) where
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = embed $ \ s ->
        case run p s of
            Left e       -> Left e
            Right (x, t) -> run (h x) t
```

### A. 3. 3. Coproducts.

In section [Products](#a-3-1-products), we tackled parsing product types, in this section we tackle parsing of coproducts, that is, types of the form

```haskell
data T a_0 ... a_n
    = T_0 a_0
    ...
    | T_n a_n
```

The first thing to notice is that the general case of a constructor of the form `T_i b_0 ... b_n_i` [^3] can be reduced to the one-argument case, by setting `a_i ~ (b_0, ..., b_n_i)` and using the constructions of section [Products](#a-3-1-products).

Assuming the existence of parsers `p_i :: Parser s e_i a_i` with `i` ranging from `0` to `n`, a commonly occuring idea for a serialization format is to first have a discriminating tag followed by the encoding of the relevant value. The tag can be implemented simply by enumerating the construtors top to bottom and return the corresponding ordinal:

```haskell
tag :: T a_0 ... a_n -> Word
tag x = case x of
    T_0 _ -> 0
    ...
    T_n _ -> n
```

This piece of bloatware can derived automatically using Haskell's generics or (God forbid) template Haskell, but we will not dwell on this detail here. To parse this format, we assume the existence of a parser for `Word` [^4]

```haskell
word :: Parser s e Word
```

With this parser on hand, we have

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

where `f :: e' -> e` and `f_i :: e_i -> e` are appropriate error conversion functions. The new ingredient needed is the `throwError e` parser that just throws an error if the tag is out of bounds.

[^3]: This includes the no-argument case since the nullary product is just the terminal `()`.
[^4]: Once we nail down the constraints on the type `s`, we will be in a position to write such a parser.

#### A. 3. 3. 1. The `MonadError` typeclass.

Fortunately, the [mtl package](https://hackage.haskell.org/package/mtl) has a typeclass for monads with a notion of error handling, so we do not need to come up with something of our own.

The `catchError` method of `MonadError` does not change the error type, but it is easy (and more importantly, useful), to implement a type-changing version:

```haskell
catchErrorWith
    :: Parser s e a             -- ^ Parser to try.
    -> (e -> Parser s d a)      -- ^ Error handler.
    -> Parser s d a
catchErrorWith p h = embed $ \ s -> either (flip run s . h) Right $ run p s
```

### A. 3. 4. Coproducts and choice.

In the previous section [Coproducts](#a-3-3-coproducts) we derived a parser for coproduct types by assuming a format consisting of a prefix tag and then dispatch on the tag to call the appropriate parser. This format is natural for binary format parsers, but for text parsers (essentially, language parsers) often something else is needed and that something else is _choice_.

Getting back to our coproduct type

```haskell
data T a_0 ... a_n
    = T_0 a_0
    ...
    | T_n a_n
```

Assume parsers `p_i :: Parser s e_i a_i`; assume furthermore that the formats for `a_i` are _disjoint_ in the sense that for every input `xs :: s` at most one of the parsers `Parser s e_i a_i` succeeds. Then we could get a parser for `T a_0 ... a_n` by trying each `p_i` in turn and returning the result of the first success; this can be written as,

```haskell
p :: Parser s e (T a_0 ... a_n)
p = (bimap f_0 T_0) <|> ... <|> (bimap f_n T_n)
```

where `(<|>)` is the choice operator. To implement choice, we rely on the parser combinator

```haskell
observe :: Parser s e a -> Parser s Void (Either e a)
```

implementing backtracking. Specifically, the `observe` parser runs the argument parser and if it succeeds it returns the result as a `Right` while if it errors, it backtracks and returns the error as a `Left`. In order to implement the backtracking part, we need to probe and change the input state `s` of the parser, so let us start with that first.

#### A. 3. 4. 1. The `MonadState` typeclass.

Probing the state of the parser monad is abstracted out in the `MonadState` typeclass, also available from the [mtl package](https://hackage.haskell.org/package/mtl). The instance implementation is as easy as:

```haskell
instance MonadState s (Parser s e) where
    get :: Parser s e s
    get = embed $ \ s -> Right (s, s)

    put :: s -> Parser s e ()
    put s = embed $ const (Right ((), s))
```

The `put` parser allows arbitrary transformations on the input `s`; in particular it provides a way, _the only_ way, to construct non-normal parsers. In this library it is used exclusively to implement backtracking and the primitive parsers requiring constraints on the input type, so normalcy is provably not violated.

#### A. 3. 4. 2. Backtracking and the `observe` parser.

With the `MonadState` instance in our hands, the implementation of `observe` is now a standard try-catch:

```haskell
observe :: Parser s e a -> Parser s Void (Either e a)
observe p = do
    s <- get
    catchErrorWith
        (Right <$> p)
        (\ e -> put s $> Left e)
```

#### A. 3. 4. 3. The `Alternative` typeclass.

We now have all the ingredients to implement choice via `(<|>)` of the `Alternative` typeclass: try the first parser and return its result; if it errors, backtrack and try the second parser. There is one issue to be solved however, namely, what to do if _both_ parsers error out? One obvious answer is "combine the errors" and "combine the errors" is a code word for a `Monoid` constraint on the error type `e`. With this setup:

```haskell
(<|>) :: Monoid e => Parser s e a -> Parser s e a -> Parser s e a
(<|>) p q =
    first absurd (observe p) >>=
        either
            (\ e -> first absurd (observe q) >>= either (throwError . (e <>)) pure)
            pure
```

The `empty` parser is also easily implemented with a call to `throwError` with the monoid unit for `e`. The monoid laws for `e` imply that with this structure `Parser s e a` is indeed a monoid, but as we will see next, it implies much more.

##### A. 3. 4. 3. 1. The `Monoid e` constraint.

What does the constraint `Monoid e` mean in practice? Error types are usually plain data, mainly useful for developers, with no meaningful monoid operation. One of the most common things to do with an error is to just do away with it into a logger trash bin. But this, I contend, is a wrong way to look at the constraint. What the constraint really is, is a strategy for _accumulating errors_, e. g. maybe you need to gather them all in a list or keep the first one only. We revisit the problem below.

##### A. 3. 4. 3. 2. The `either` parser and an alternative to `Alternative`.

With the `Alternative` typeclass, we can write an `either` parser, that is more deserving of the name choice:

```haskell
either :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (Either a b)
either p q = (Left <$> p) <|> (Right <$> q)
```

Expanding that definition of `(<|>)` inside `either` we get its direct implementation:

```haskell
either :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (Either a b)
either p q =
    first absurd (observe p) >>=
        either
            (\ e -> first absurd (observe q) >>= either (throwError . (e <>)) (pure . Left))
            (pure . Right)
```

Uncurry-ing `either`, we get a natural transformation `(Parser s e a, Parser s e b) -> Parser s e (Either a b)`. Since there is also a map

```haskell
unit :: Monoid e => () -> Parser s e Void
unit = const (throwError mempty)
```

this gives a (symmetric) lax-monoidal structure to `Parser s e` between products and coproducts [^5]. Considering the _codiagonal_,

```haskell
codiagonal :: Either a a -> a
codiagonal = either id id
```

we have the equality

```haskell
(<|>) = fmap codiagonal . either
```

The upshot of all this is that the `Alternative` typeclass is a red herring, choice is really a lax-monoidal structure. Let us go through the steps one by one to make this clearer.

[^5]: The coherence laws hold up; exercise to the interested reader.

##### A. 3. 4. 3. 3. Monoidal categories and monoids.

Monoidal structures are to categories as monoids are to sets, that is, just as a monoid is a binary function satisfying some equational laws, a monoidal structure is a bifunctor `(:*:)` and an object `k`, the _unit_ object, together with natural isomorphisms

```haskell
associator  :: a :*: (b :*: c) -> (a :*: b) :*: c
leftUnitor  :: k :*: a -> a
rightUnitor :: a :*: k -> a
```

satisfying a bunch of equations, the so called _coherence laws_ [^6].

The value of any abstraction is in the list of examples it covers and the things you can do with it, both the new concepts that can be expressed and the theorems that can be derived. Starting with the examples, the two most important for us, are the monoidal structures given by products and coproducts respectively. As for concepts that can be expressed, one can now generalize the notion of monoid to a monoid in a monoidal category. The classical notion is recovered by using the product monoidal structure.

[^6]: See [Monoidal categories](https://ncatlab.org/nlab/show/monoidal+category) for the full story.

##### A. 3. 4. 3. 4. Why you never heard of monoids for coproducts.

We start with an existence theorem.

__Theorem__: The codiagonal `Either a a -> a` together with the unique initial `absurd :: Void -> a` is a monoid for the coproduct monoidal structure.

__Proof__: standard exercise in universal property juggling.

Let us call this monoid structure on `a` the _trivial_ one.

__Theorem__: For each `a` there is only one monoid structure for coproducts, the trivial one.

__Proof__: Since `Void` is initial, there is only one function `Void -> a`. By the universal property of coproducts, a function `Either a a -> a` is of the form `either f g` for functions `f :: a -> a` and `g :: a -> a`. Now plug this in the two identity laws to get `f = id` and `g = id`.

##### A. 3. 4. 3. 5. Monoids and lax-monoidal functors.

Just as there is a notion of _monoid morphism_, a function that is suitably compatible with monoid structures, there is a notion of _monoidal functor_, a functor `f` together with natural isomorphisms,

```haskell
u :: f a :+: f b -> f (a :*: b)
e :: j -> f k
```

where the objects `j` and `k` are the unit objects for `(:+:)` and `(:*:)` respectively, satisfying some equations [^7] . If we drop the requirements that the natural transformations are isomorphisms we obtain the notion of _lax monoidal functor_.

__Theorem__: Let `f` be a lax monoidal functor between monoidal structures `(:+:)` and `(:*:)` and

```haskell
m :: a :*: a -> a
v :: k -> a
```

a `(:*:)`-monoid structure on `a`. Then

```haskell
m' :: f a :+: f a -> f a
m' = fmap m . u

v' :: j -> f a
v' = fmap v . e
```

is a `(:+:)`-monoid structure on `f a`.

__Proof__: See [Monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor), proposition 3. 1. and references therein.

[^7]: See [Monoidal functors](https://ncatlab.org/nlab/show/monoidal+functor) for them.

##### A. 3. 4. 3. 6. The punchline.

Combining sections [Why you never heard of coproducts](#a-3-4-7-why-you-never-heard-of-monoids-for-coproducts) and [Monoids and lax monoidal functors](#a-3-4-8-monoids-and-lax-monoidal-functors), we have that the functions,

```haskell
u :: Monoid e => (Parser s e a, Parser s e b) -> Parser s e (Either a b)
u = uncurry (either)

e :: Monoid e => () -> Parser s e Void
e = unit
```

make `Parser s e` a lax monoidal functor between `(,)` and `Either`. By the preservation theorem, the functions

```haskell
m :: Monoid e => (Parser s e a, Parser s e a) -> Parser s e a
m = fmap codiagonal . uncurry (either)

v :: Monoid e => () -> Parser s e Void
v = const (throwError mempty)
```

give a monoid structure on `f a`, which is just the `Alternative` instance.

##### A. 3. 4. 3. 7. More laws.

Since every function is automatically a monoid morphism for the trivial monoid structures, it follows that, for every function `f` and parsers `p` and `q`:

```haskell
fmap f (p <|> q) = (fmap f p) <|> (fmap f q)
fmap f empty = empty
```

But this is also a consequence of naturality of `(<|>)`, and thus a consequence of the free theorem [^8]. Slightly more substantive (because it does not follow from any free theorem):

__Theorem__: The `Alternative` instance for `Parser s e a` satisfies _right absorption_, that is, for every `x :: Parser s e a`:

```haskell
empty <*> x = empty
```

If the monoid structure on the error type `e` is idempotent (that is, for all `x :: e`, `x <> x = x`), then, it satisfies both _left_ and _right distributivity_:

```haskell
f <*> (x <|> y) = (f <*> x) <|> (f <*> y)
(f <|> g) <*> x = (f <*> x) <|> (g <*> y)
```
__Proof__: Follows from the definition of `(<*>)` as

```haskell
p <*> q = do
    f <- p
    x <- q
    pure (f x)
```

and doing a case by case analysis on the failures.

As we will see in the next section, the monoid law that we will use in the library is idempotent.

[^8]: See [Theorems for Free!](https://dl.acm.org/doi/pdf/10.1145/99370.99404).

## A. 4. On Errors.

As discussed in [The Alternative Typeclass](#a-3-4-3-the-alternative-typeclass), the `Alternative` typeclass requires a `Monoid e` constraint on the error type `e` that determines how errors combine, or as we termed it, the error accumulation strategy. There are two basic options: either you accumulate all errors in a container like a list or you short-circuit at the first error. In its turn, short-circuiting completely determines the monoid operation:

```haskell
(<>) :: Eq e => e -> e -> e
(<>) x y
    | x == mempty = y
    | otherwise   = x
```

One advantage over the list-accumulation strategy is that this monoid is idempotent guaranteeing stronger laws for the `Alternative` instance -- see section [More laws](#a-3-4-3-7-more-laws).

### A. 4. 1. First attempt.

File(s):

  * [ParseError.hs](../src/Trisagion/Types/ParseError.hs)

The `ParseError e` type is a thin wrapper around `e` to implement the short-circuit accumulation strategy:

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

A little bit of staring and the reader should be able to convince of himself that this type is monoid isomorphic to `Maybe (First a)` with `First a` the newtype-wrapper from base with semigroup operation pick-the-first-element. The `Maybe` functor then freely adds the monoid unit.

### A. 4. 2. What is in an error?

File(s):

  * [HasPosition.hs](../src/Trisagion/Typeclasses/HasPosition.hs)

`ParseError e` is just a newtype-wrapper around `e` for the short-circuiting accumulation strategy; any information specific to the error must be packed in the type `e`. But there are pieces of information that are useful independently of the error type `e`, and that thus are a better fit as fields of `ParseError`, for example a notion of _stream position_ to better locate the source of the error. So we change the `ParseError` to

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
    {-# MINIMAL getPosition #-}

    {- | The type of the stream's position. -}
    type PositionOf s :: Type

    {- | Getter for the current position of the stream. -}
    getPosition :: s -> PositionOf s
```

As one can see, the entirety of `HasPosition` is nothing more than a getter for the input stream. Even more, every type `s` has an `HasPosition` instance by simply returning itself as the current position!

```haskell
instance HasPosition s
    type PositionOf s = s

    getPosition :: s -> s
    getPosition = id
```

And this notion of position is not entirely silly, because if the current position can be used to locate the source of the problem, much more so with the entire input stream. So strictly speaking there is no need for this lawless typeclass (and lawless typeclasses are a code smell). The major downside of having the error carry a reference to the input stream is that it potentially keeps it alive in memory for much longer than necessary. I have gone back and forth on this, and ended going for the most flexible solution: the error carries a reference to the input stream, but we also keep the typeclass `HasPosition`. The `Bifunctor` instance then allows to insert `getPosition` calls if desired.

### A. 4. 3. Backtraces.

