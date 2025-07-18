{- |
Module: Trisagion.Parser

The @Parser@ monad.
-}

module Trisagion.Parser (
    -- * Type operators.
    (:+:),

    -- * The parsing monad.
    Parser,

    -- ** Basic functions.
    parse,
    eval,
    remainder,

    -- * Error parsers.
    catch,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Bifunctor (Bifunctor (..))

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Optics.Core (view)

-- Package.
import Trisagion.Types.Result (Result (..), result)


-- $setup
-- >>> import Data.Bifunctor
-- >>> import Control.Applicative ((<|>))
-- >>> import Trisagion.Types.ParseError
-- >>> import Trisagion.Streams.Counter
-- >>> import Trisagion.Parsers.Combinators
-- >>> import Trisagion.Parsers.Streamable


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:


{- | The parsing monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor

{- | The 'Bifunctor' instance, providing functoriality in the error type.

For a function @f :: d -> e@, @'first' f@ preserves all the structure in sight. Specifically,
@'first' f@ preserves the 'Applicative' structure,

prop> first f . pure == pure
prop> (first f p) <*> (first f q) == first f (p <*> q)

the 'Monad' structure,

prop> (first f p) >>= (first f .  h) == first f (p >>= h)

and, assuming @f@ is a /monoid morphism/, the 'Alternative' structure,

prop> first f empty == empty
prop> (first f p) <|> (first f q) == first f (p <|> q)
-}
instance Bifunctor (Parser s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap g f p = Parser $ bimap g f . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @pure x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

  * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Parser s e) where
    {-# INLINE pure #-}
    pure :: a -> Parser s e a
    pure x = Parser $ \ s -> Success x s

    {-# INLINE (<*>) #-}
    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = Parser $ \ xs ->
        case run p xs of
            Error e1     -> Error e1
            Success f ys ->
                case run q ys of
                    Error e2     -> Error e2
                    Success x zs -> Success (f x) zs

{- | The 'Monad' instance.

The bind combinator @p >>= f@ first runs @p@ and then the parser obtained by applying the monadic
function @f@ to the result.

note(s):

  * As with @p \<*\> q@, @p >>= f@ short-circuits on @p@ erroring out.
-}
instance Monad (Parser s e) where
    {-# INLINE (>>=) #-}
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = Parser $ \ xs ->
        case run p xs of
            Error e      -> Error e
            Success x ys -> run (h x) ys

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

note(s):

  * The parser  @p \<|\> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.

The 'Alternative' instance obeys the /left catch/, /left absorption/ and /left zero/ laws,

prop> pure x <|> p == pure x
prop> empty <*> p == empty
prop> empty >>= h == empty

but /not/ their right-sided versions because of short-circuiting.

Furthermore, if the monoid @e@ is /idempotent/, that is, for all @x :: e@, @x <> x == x@, then the
'Alternative' instance also satisfies /left distributivity/:

prop> f <*> (x <|> y) == (f <*> x) <|> (f <*> y)

=== __Counterexample:__

The next example shows that right distributivity is violated even with an idempotent monoid.

>>> let unitError = mempty :: ParseError ()
>>> let f = bimap (const unitError) (const id) $ matchOne '0'
>>> let g = first (const unitError) (matchOne '0') *> bimap (const unitError) (const id) (matchOne '1')
>>> let x = first (const unitError) (matchOne '2')
>>> parse ((f <|> g) <*> x) (initialize "0123")
Left Nil
>>> parse ((f <*> x) <|> (g <*> x)) (initialize "0123")
Right ('2',Counter 3 "3")
-}
instance Monoid e => Alternative (Parser s e) where
    {-# INLINE empty #-}
    empty :: Parser s e a
    empty = Parser $ const (Error mempty)

    {-# INLINE (<|>) #-}
    (<|>) :: Parser s e a -> Parser s e a -> Parser s e a
    (<|>) p q = Parser $ \ s ->
        case run p s of
            r@(Success _ _) -> r
            Error e         ->
                case run q s of
                    r'@(Success _ _) -> r'
                    Error e'         -> Error $ e <> e'

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'Parser' state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The 'get' parser does not throw an error or consume input.

The 'put' parser allows changing the t'Parser' state.

__Definition__: a parser @p@ is /normal/ if for every input @xs@, on success, the 'remainder' is a,
possibly improper, suffix of @xs@.

All the parsers in the library are provably normal, and all parser combinators return normal
parsers on the assumption that the arguments are normal, but the 'MonadState' typeclass,
specifically the 'put' method, allows to construct non-normal parsers.

note(s):

  * To formalize the notion of /suffix/, a @'Trisagion.Typeclasses.Streamable.Streamable' s@
  constraint on @s@ is needed -- see 'Trisagion.Typeclasses.Streamable.isSuffixOf'.
-}
instance MonadState s (Parser s e) where
    {-# INLINE get #-}
    get :: Parser s e s
    get = Parser $ \ s -> Success s s

    {-# INLINE put #-}
    put :: s -> Parser s e ()
    put s = Parser $ \ _ -> Success () s


{- | The 'MonadError' instance.

The typeclass provides error handling for the t'Parser' monad. The @'throwError' e@ parser fails
unconditionally with @e@. The parser @'catchError' p h@ first tries @p@. If it succeeds, it returns
the parsed result, if it fails, it backtracks and runs the parser @h e@ where @e@ is the error
returned by @p@.

The difference between 'MonadError' and 'Alternative' regarding errors, is analogous to the
difference between 'Monad' and 'Applicative'. Just as with the former, 'MonadError' allows the
continuation to depend on the specific error that was thrown.

note(s):

  * The monad analogy is not precise, because even if we make the obvious generalization of
  @'catchError'@ to a type-changing version (see 'catch'), it does not satisfy associativity.
-}
instance MonadError e (Parser s e) where
    {-# INLINE throwError #-}
    throwError :: e -> Parser s e a
    throwError e = Parser $ const (Error e)

    {-# INLINE catchError #-}
    catchError :: Parser s e a -> (e -> Parser s e a) -> Parser s e a
    catchError p h = Parser $ \ s ->
        -- Case statement instead of 'catch' to make use of sharing in the Success branch.
        case run p s of
            r@(Success _ _) -> r
            Error e         -> run (h e) s


{- | Run the parser on the input and return the results. -}
{-# INLINE run #-}
run :: Parser s e a -> s -> Result s e a
run (Parser p) = p

{- | Parse the input and return the results. -}
{-# INLINE parse #-}
parse :: Parser s e a -> s -> e :+: (a, s)
parse p = view result . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Parser s e a -> s -> e :+: a
eval p = fmap fst . view result . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Parser s e a -> s -> e :+: s
remainder p =  fmap snd . view result . run p


{- | Type-changing version of 'catchError'. -}
{-# INLINE catch #-}
catch
    :: Parser s d a                     -- ^ Parser to try.
    -> (d -> Parser s e a)              -- ^ Error handler.
    -> Parser s e a
catch p h = Parser $ \ xs ->
    case run p xs of
        Error e      -> run (h e) xs
        Success x ys -> Success x ys
