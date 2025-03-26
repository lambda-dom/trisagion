# Trisagion.

A _serializer_ or _encoder_, for a type `a` is a function `a -> ByteString` that given a value `x :: a` returns a `ByteString` encoding `x`. As a first generalization, the return type of the encoder being `ByteString` is not strictly necessary, so we generalize,

```haskell
newtype Encoder s a = Encoder (a -> s)
```

with serialization done via

```haskell
encode :: Encoder s a -> a -> s
encode (Encoder f) = f
```

What constraints should be put on `s` to serve as an adequate output type? By Quine's dictum "No entity without identity", which even if not true of being in general, it certainly is of mathematicals, we (implicitly) require `Eq s`, but a discussion of what else is needed will be deferred to [Typeclasses for the input](#a-4-typeclasses-for-the-input). For now, we content ourselves with saying that, up to some piece of state that it may carry around, `s` is isomorphic to `[a]` for some appropriate type `a`. The paradigmatical examples to have in mind are types like `ByteString`, `Text` and `[Char]`. Theoretically, we could even restrict ourselves to lists of bits, or `[Bool]`, but input types like `ByteString` and `Text` are important for pragmatic reasons.

Dually to serializing, _parsing_  or _decoding_ is a function `s -> a` that given some input `xs :: s` returns a decoded value `x :: a`. Wrapping in newtypes,

```haskell
newtype Parser s a = Parser (s -> a)
```

At this point we should also note that there is a bifurcation in the type of parsers. `ByteString` and `[Word8]`, and even `[Bool]` parsers, are parsers for binary formats, which generally have a fixed layout optimized for size, speed and simplicity of parsing, while `Text` parsers are really language parsers. In this exploratory document we will concentrate mainly on binary parsers but much of the commentary applies to language parsers as well.

# A. Parsing.

## A. 1. The `Parser s e a` type.

## A. 1. 1. Parsing functions.

File(s):

  * [Parser.hs](../src/Trisagion/Parser.hs)

In the introduction a parsing function was defined as a function `a -> s` but the problem with this definition is that it does not allow for composition. To be able to compose parsing functions, we need the parsing function to also return the rest of the input so that the next parser in the pipeline can continue. So we redefine the `Parser` type as

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

If error distinctions are not needed, then:

```haskell
decode :: Parser s e a -> s -> Maybe a
decode p = either (const Nothing) Just . eval p
```

Minus error handling, `decode` ought to be the inverse of `encode`.

### A. 1. 2. One implication and one design decision.

An immediate implication of the type signature of a parsing function is that it is all-or-nothing: _either_ it throws an error _or_ (exclusive or) it succeeds, returning the pair of the parsed result and the rest of the input.

note(s):

  * We will often use exception-like terminology, like "throws an error"; the meaning should be evident.

At this point, we note that `Parser` could be generalized to a transformer by,

```haskell
-- The ParserT monad transformer.
data ParserT m s e a = Parser (s -> m (Either e (a, s)))

-- The Parser monad.
type Parser = ParserT Identity
```

as is done in say, the [Megaparsec](https://hackage.haskell.org/package/megaparsec) library. In this library, we explicitly do _not_ make such a generalization; all code is pure (meaning: effect free). This design forces prospective library users to construct the parser and stuff it somewhere, gather the input from the IO layer and apply the parser via `run`. The expectation is that, for the cases where the input must be consumed incrementally, some scheme using a streaming library can be bolted on top.

### A. 1. 3. A small improvement: the type `Result s e a`.

File(s):

  * [Result.hs](../src/Trisagion/Types/Result.hs)

We make one small improvement by replacing the return type `Either e (a, s)` of a parsing function by the isomorph

```haskell
data Result s e a
    = Error !e
    | Success a !s 
```

This introduces strictness where laziness is almost surely not needed while keeping it in where it is useful. It also removes one layer of indirection in the `Success` case.

note(s):

  * In what follows, in all the code examples we keep `Either e (a, s)` as the return type of a parsing function.

## A. 2. Some definitions.

Given a parser `p :: Parser s e a` what is the relation of the input with the remainder, if any?

__Definition__: A parser `p :: Parser s e a` is _normal_ if for every input `xs :: s`, on success, the remainder is a (possibly improper) suffix of `xs`.

There is one obvious problem with this definition, in that being a suffix is not a definable relation for an arbitrary type `s`. However, it is certainly well-defined for types like `ByteString` and `Text`, so we leave for later the working out of what is needed to define such a relation -- see [Definable `isSuffix`](#a-4-1-5-definable-issuffix) for the details.

__Definition__: A parser `p :: Parser s e a` _does not consume input_ if there is one input `xs :: s` for which parsing succeeds and the remainder is equal to `xs`. The parser `p` _never consumes input_ if for every input `xs`, on success the remainder is equal to `xs`.

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

The attentive reader will surely have noticed that while the parsers `p_i` have types `Parser s e_i a_i`, the argument parsers for `(<*>)` are required to all have the same error type. So at a minimum, and assuming we have a cospan of conversion functions `f_i :: e_i -> e` for some type `e`, we need a `Bifunctor` instance for `Parser s e a` to write

```haskell
p :: Parser s e (T a_0 ... a_n)
p = T <$> (first f_0 p_0) <*> ... <*> (first f_n p_n)
```

The choice of `e` is left to the user, but there is a canonical, minimal one: take the coproduct of all the `e_i`. In the section [On Errors](#a-4-on-errors) we will see another way to deal with this problem that does not rely on coming up with a cospan `e_i -> e`.

Note also that as with the input stream type `s`, there are still no constraints on the error type `e`.

#### A. 3. 1. 3. The parser `pure x`.

File(s):

  * [Combinators.hs](../src/Trisagion/Parsers/Combinators.hs)

For a value `x`, the parser `pure x` does not throw an error and never consumes input. The two are inextricably linked, because if a parser does not throw an error then it must do something when there is no input to be had, which of course, requires that it does not consume input. However, even if the two are linked only the former can be reflected in the type signature, e. g. by:

```haskell
pure :: a -> Parser s Void a
```

Since the type signature of `pure` is already fixed by the `Applicative` typeclass, we instead provide an error-free version of it:

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

Given the latter, then `<*>` of `Applicative` is given by,

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

The universal properties of products and coproducts yield canonical maps,

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

If both `unzip` and `terminal :: f () -> ()` are isomorphisms then `f` is said to _preserve products_. `Parser s e a` does _not_ preserve products; we will not show this (it is not very difficult anyways) but it is an instructive exercise to see that `zip`, or more precisely its uncurried version, is _not_ an inverse of `unzip`, and the proof works for every monad. First, `zip` can be re-defined -- see [The Monad typeclass](#a-3-2-the-monad-typeclass) for the `Monad` instance for `Parser s e a` -- as

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

Assuming the existence of parsers `p_i :: Parser s e_i a_i` with `i` ranging from `0` to `n`, a commonly occurring idea for a serialization format is to first have a discriminating tag followed by the encoding of the relevant value. The tag can be implemented simply by enumerating the constructors top to bottom and return the corresponding ordinal:

```haskell
tag :: T a_0 ... a_n -> Word
tag x = case x of
    T_0 _ -> 0
    ...
    T_n _ -> n
```

This piece of bloatware can even be derived automatically using something like the [generics-sop library](https://hackage.haskell.org/package/generics-sop) or (God forbid) template Haskell, but we will not dwell on this detail here.

note(s):

  * The serializing format using the `tag` function is vulnerable to changes in `T` like reordering or addition of new constructors. A more robust version would use the constructor names, but this version is vulnerable to constructor renaming. Other refactorings of the `T` type like the deletion of constructors would need more sophisticated schemes like versioning to ensure backwards compatibility.

To parse this format, we assume the existence of a parser for `Word` [^4]

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

`catchErrorWith` and `throwError` have the right shape for a monad structure for `ParseError s e a` in the error type `e` but it is not difficult to see that, essentially because of short-circuiting, the `Monad` laws are violated.

### A. 3. 4. Coproducts and choice.

In the previous section [Coproducts](#a-3-3-coproducts) we derived a parser for coproduct types by assuming a format consisting of a prefix tag and then dispatch on the tag to call the appropriate parser. This format is natural for binary format parsers, but for text parsers (essentially, language parsers) often something else is needed and that something else is _choice_.

Getting back to the coproduct type

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

What does the constraint `Monoid e` mean in practice? Error types are usually plain data, mainly useful for developers, with no meaningful monoid operation. One of the most common things to do with an error is to just throw it away to a logger trash bin. But this, I contend, is a wrong way to look at the constraint. What the constraint really is, is a strategy for _accumulating errors_, e. g. maybe you need to gather them all in a list or keep the first one only. We revisit the problem below.

##### A. 3. 4. 3. 2. The `either` parser and an alternative to `Alternative`.

With the `Alternative` typeclass, we can write the `either` parser that is more deserving of the name choice:

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

Since every function is automatically a monoid morphism for the trivial monoid structures, it follows that for every function `f` and all parsers `p` and `q`:

```haskell
fmap f (p <|> q) = (fmap f p) <|> (fmap f q)
fmap f empty = empty
```

But this is also a consequence of naturality of `(<|>)` and thus a consequence of the free theorem [^8]. Slightly more substantive (because it does not follow from any free theorem):

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

As we will see in the next section, the monoid law that we will use in the library is idempotent. Another important case is the case when all the error distinctions are erased and the trivial monoid `()` is picked for error type; since `Either () a` is isomorphic to `Maybe a`, we are back at the `decode p` case.

[^8]: See [Theorems for Free!](https://dl.acm.org/doi/pdf/10.1145/99370.99404).

## A. 4. On Errors.

As discussed in [The Alternative Typeclass](#a-3-4-3-the-alternative-typeclass), the `Alternative` typeclass requires a `Monoid e` constraint on the error type `e` that determines how errors combine, or as we termed it, the error accumulation strategy. There are two basic options: either errors accumulate in a a list or the parsers short-circuit on the first error. Short-circuiting completely determines the monoid operation:

```haskell
(<>) :: Eq e => e -> e -> e
(<>) x y
    | x == mempty = y
    | otherwise   = x
```

One advantage of the short-circuiting strategy is that the monoid is idempotent guaranteeing stronger laws for the `Alternative` instance -- see section [More laws](#a-3-4-3-7-more-laws).

### A. 4. 1. First attempt.

File(s):

  * [ParseError.hs](../src/Trisagion/Types/ParseError.hs)

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

### A. 4. 2. What is in an error?

File(s):

  * [HasPosition.hs](../src/Trisagion/Typeclasses/HasPosition.hs)

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

And this notion of position is not entirely silly, because if the current position can be used to locate the source of the problem, much more so with the entire input stream. So strictly speaking there is no need for this lawless typeclass (and lawless typeclasses are a code smell). The major downside of having the error carry a reference to the input stream is that it potentially keeps it alive in memory for much longer than needed. I have gone back and forth on this and ended going for the most flexible solution: the error carries a reference to the input stream, but we also keep the typeclass `HasPosition`. The `Bifunctor` instance then allows to insert `getPosition` calls if desired.

### A. 4. 3. Backtraces.

File(s):

  * [ParseError.hs](../src/Trisagion/Parsers/ParseError.hs)

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

Another option would be to throw a different error `e'` with the thrown error from `p` attached like _exception backtraces_. The obvious problem is that the errors of `p` and `q` can be different so we must still find appropriate cospans; Haskell's GADT's and existentials to the rescue.

```haskell
data ParseError s e where
    Fail :: ParseError s e
    ParseError
        :: (Typeable d, Eq d, Show d)
        => !(Maybe (ParseError s d))    -- ^ Backtrace.
        -> !s                           -- ^ Input stream.
        -> !e                           -- ^ Error tag.
        -> ParseError s e
```

The reader can read up on existentials, but the one-line summary is that we can use _any_ `(Typeable d, Eq d, Show d) => ParseError s d` as a backtrace of an error but getting it back the only thing we know about it is that it is a `Maybe (ParseError s d)` with `d` satisfying the constraints `(Typeable d, Eq d, Show d)`.

note(s):

  * The actual shape of `ParseError` in [ParseError.hs](../src/Trisagion/Parsers/ParseError.hs) is slightly different.

With these changes to `ParseError`, we can now have a parser combinator that turns a thrown error into the backtrace of a new, contextually more useful, error:

```haskell
onParseError
    :: (Typeable d, Eq d, Show d)
    => e                                -- ^ Error tag of new error.
    -> Parser s (ParseError s d) a      -- ^ Parser to run.
    -> Parser s (ParseError s e) a
onParseError e p =
    catchErrorWith
        p
        (\ b -> do
            s <- get
            throwError $ makeParseError b s e)
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

without having to unify the error types of `p` and `q`.

#### A. 4. 3. 1. The backtrace getter.

With backtraces, a `ParseError` looks like,

>  error -> Just error_0 -> ... -> Just error_n -> Nothing

with `error_i` _not_ equal to a `Fail` by normalization. So the full backtrace is just a list of `(Typeable d, Eq d, Show d) => ParseError s d`. This leads to implement a getter for the backtrace as an elimination function:

```haskell
getBacktrace :: forall s e a . (forall d . s -> d -> a) -> ParseError s e -> [a]
getBacktrace f = go
    where
        go :: ParseError s c -> [a]
        go Fail               = []
        go (ParseError b s e) = f s e : maybe [] go b
```

#### A. 4. 3. 2. Anything else?

## A. 4. Typeclasses for the input.

We are at the point where we can finally tackle the constraints needed for the input type `s`; after all, at this point we cannot even get out one element from the input stream.

### A. 4. 1. One out of `s`: the `Streamable` typeclass.

The introductory description completely describes what we need from `s`: an `uncons` operation with the return type depending on `s`.

```haskell
{- | The @Streamable@ typeclass of monomorphic input streams. -}
class Streamable s where
    type ElementOf s :: Type

    {- | Get, or uncons, the first element from the streamable. -}
    splitOne :: s -> Maybe (ElementOf s, s)
```

#### A. 4. 1. 1. The `MonoFunctor` constraint.

All the paradigmatic examples of input streams like `ByteString` and `Text` have a `map`-like operation, a monomorphic variant of `fmap`. The `MonoFunctor` typeclass captures this; it is not a terribly useful typeclass but it ends up being very important as a base and to state some of the typeclass laws.

```haskell
{- | The @Streamable@ typeclass of monomorphic, streamable functors. -}
class MonoFunctor s => Streamable s where
    {- | Get, or uncons, the first element from the streamable. -}
    splitOne :: s -> Maybe (ElementOf s, s)
```

#### A. 4. 1. 2. No free laws.

Since monofunctors `f` are not fully polymorphic in `ElementOf f`, there are no free theorems available, and equational laws like naturality must be explicitly required.

__Definition__: Let `s` and `t` be two monofunctors with `a ~ ElementOf s ~ ElementOf t`. A function `h :: s -> t` is _mononatural_ if for every `f :: a -> a` we have the equality `monomap f . h = h . monomap f`.

notes(s):

  * There are equivalent descriptions of monofunctoriality and mononaturality in terms of monoid actions, but these trivial reformulations do not yield anything important for our purposes.

The first law for `Streamable` is that `splitOne` is mononatural. In case it is not clear, the `MonoFunctor` structure of the codomain is

```haskell
monomap :: MonoFunctor s => (s -> s) -> Maybe (ElementOf s, s) -> Maybe (ElementOf s, s)
monomap f = fmap (bimap f (monomap f))
```

#### A. 4. 1. 3. The `MonoFoldable` constraint.

Given the `uncons` operation, we can define a conversion to lists,

```haskell
toList :: Streamable s => s -> [ElementOf s]
toList = unfoldr splitOne
```

so that `Streamable` could / should have `MonoFoldable` as a superclass. There are two main reasons why `MonoFoldable` is not a superclass:

  1. For infinite lists, `monolength` diverges and we _do_ want infinite lists and other stream-like objects to be instances of `Streamable`.

  2. For input streams like `ByteString.Lazy` computing its length would force the entire bytestring into memory which is a big no-no.

Of course, _if_ `s` is an instance of `MonoFoldable` then the equality should hold and this is the second law for `Streamable`.

#### A. 4. 1. 4. Two fundamental parsers.

With the `Streamable` typeclass we can now extract one element from the input stream and also check if the input stream has more elements to yield.

```haskell
eoi :: Streamable s => Parser s Void Bool
eoi = gets (isNothing . splitOne)

one :: Streamable s => Parser s (ParseError s InputError) (ElementOf s)
one = do
    xs <- get
    case splitOne xs of
        Just (y, ys) -> put ys $> y
        Nothing      -> absurd <$> throwParseError (InputError 1)
```

#### A. 4. 1. 5. Definable `isSuffix`.

We can also close one of the loopholes in the introduction, the `isSuffix` relation. With `Streamable` this is simply defined as `isSuffix` at the level of lists:

```haskell
isSuffix :: (Streamable s, Eq (ElementOf s)) => s -> s -> Bool
isSuffix xs ys = toList xs `isSuffixOf` toList ys
```

### A. 4. 2. Optimization: prefixes and the `Splittable` typeclass.

The `Streamable` typeclass allows to write down all commonly used parsers, but alas, getting one element from the input stream at a time can be very inefficient. What we need is a notion of chunk, or stream prefix, and methods to cut prefixes out of streams.

#### A. 4. 2. 1. The `Splittable` class.

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

#### A. 4. 2. 2. The laws.

To state the laws, we must assume something of `PrefixOf s` that is not expressed directly in the typeclass. The first constraint is that `PrefixOf s` is a monofunctor with the same type of elements as `s`, that is, `ElementOf (PrefixOf s) ~ ElementOf s`. With this assumption: for every `n` and every `p`, both `splitAt n` and `splitWith p` are mononatural.

For the second law, put

```haskell
let (prefix, suffix) = spliAt n xs
```

for arbitrary `n` and `xs`. Given the `toList` function on `Streamable`, both `suffix` and `xs` can be converted to lists, and since as per the name `prefix` is supposed to be a prefix of `xs`, then there should be a unique list `l` such that

```haskell
monotoList xs = l ++ monotoList suffix
```

It follows that we have the equality,

```haskell
l = take (length xs - length suffix) (monotoList xs)
```

so it is not much of a stretch to assume that prefixes can be converted to lists. Note that the arguments above for not requiring `MonoFoldable s` on a `Streamable` do _not_ apply, that is, we are implicitly assuming that prefixes are _finite_ monofoldables -- as we will see below, some important parsers with a `Splittable s` constraint require computing the lengths of prefixes. Therefore, assuming a further `MonoFoldable (PrefixOf s)`, which is satisfied by all the `Splittable` instances defined by the library, the second typeclass law just says that at the level of lists `splitAt` is `splitAt` and `splitWith` is `span`:

```haskell
bimap monotoList monotoList . splitAt n = splitAt n . monotoList
bimap monotoList monotoList . splitWith p = span p . monotoList
```

The third and final law is a compatibility condition between `splitOne` and `splitAt`:

```haskell
maybe [] (bimap singleton monotoList) . splitOne = bimap monotoList monotoList . splitAt 1
```

#### A. 4. 2. 3. Derived operations.

#### A. 4. 2. 4. Isolating parsers.
