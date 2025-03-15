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

The choice of `e` is left to the user, but there is a canonical, minimal one: take the coproduct of all the `e_i`. In the chapter dedicated to errors, we will see another way to deal with this problem that does not rely on coming up with a unifier `e`.

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

If `unzip` and `terminal :: f () -> ()` are isomorphisms then `f` is said to _preserve products_. `Parser s e a` does _not_ preserve products; we will not show this (it is not very difficult anyways) but it is an instructive exercise to see that `zip`, or more precise its uncurried version, is _not_ an inverse to `unzip`, and this can be done for any monad. First, `zip` can be defined for any monad -- see [The Monad typeclass](#a-3-2-the-monad-typeclass) for the `Monad` instance for `Parser s e a` -- as

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

which is equal to `p` only if running `p` is idempotent. For the converse,

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

Assuming the existence of parsers `p_i :: Parser s e a_i` with `i` ranging from `0` to `n`, a commonly occuring idea for a serialization format is to first have a discriminating tag followed by the encoding of the relevant value. The tag can be implemented simply by enumerating the construtors top to bottom and return the corresponding ordinal:

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

### A. 3. 4. Coproducts: Take II.

In the previous section [Coproducts](#a-3-3-coproducts) we derived a parser for coproduct types by assuming a format consisting of a prefix tag and then dispatch on the tag to call the appropriate parser. This format is natural for binary format parsers, but for text parsers (essentially, language parsers) something else is needed and that something else is _choice_.

Getting back to our coproduct type

```haskell
data T a_0 ... a_n
    = T_0 a_0
    ...
    | T_n a_n
```

Assume parsers `p_i :: Parser s e_i a_i`; assume furthermore that the formats for `a_i` are _disjoint_ in the sense that for every input `xs :: s`, at most only one of the parsers `Parser s e_i a_i` will succeed. Then we could get a parser for `T a_0 ... a_n` by trying each `p_i` in turn and returning the result of the first success; this can be written as,

```haskell
p :: Parser s e (T a_0 ... a_n)
p = (bimap f_0 T_0) <|> ... <|> (bimap f_n T_n)
```

where `(<|>)` is the choice operator. In order to accomplish this, we will rely on implementing a parser combinator

```haskell
observe :: Parser s e a -> Parser s Void (Either e a)
```

that implements backtracking. Specifically, the `observe` parser runs the argument parser and if it succeeds return the result as a `Right` while if it errors, backtrack and return the error as a `Left`. In order to implement the backtracking bit, we need to be able to probe and change the input state `s` of the parser, so let us start with that first.

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

We now have all the ingredients to implement choice: try the first parser and return its result; if it errors, backtrack and try the second parser. There is one issue to be solved however: what to do if _both_ parsers error out? One obvious answer is to "combine the errors" and combining errors can be seen as a code word for a `Monoid` constraint on the error type `e`. With this:

```haskell
(<|>) :: Monoid e => Parser s e a -> Parser s e a -> Parser s e a
(<|>) p q = do
    first absurd (observe p) >>=
        either
            (\ e -> absurd (observe q) >>= either (throwError . (e <>)) pure)
            pure
```

The `empty` parser is also easily implemented with a call to `throwError` with the monoid unit for `e`. The monoid laws for `e` imply that this is indeed a monoid, but as we will see next, it implies much more.

#### A. 3. 4. 4. The `Monoid e` constraint.

What does the constraint `Monoid e` mean in practice? Error types are usually plain data, mainly useful for developers, with no meaningful monoid operation. One of the most common things to do with an error is to just do away with them into a logger trash bin. But this, I contend, is a wrong way to look at the constraint. What the constraint really is, is a strategy for _accumulating errors_, e. g. maybe you need to gather them all in a list or keep the first one only. We revisit the problem below.
