{- |
Module: Trisagion.Parser

The @Parser@ monad.
-}

module Trisagion.Parser (
    -- * The parsing monad.
    Parser,

    -- * Basic functions.
    embed,
    run,
    parse,
    eval,
    remainder,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)

-- Libraries.
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Types.Result (Result (..), toEither)


{- | The parsing monad @Parser s e a@.

@s@ is the input stream type, @e@ is the type of parsing errors that the parser can throw and @a@
is the type of parsed values.
-}
type Parser ::  Type -> Type -> Type -> Type
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


-- Instances.
{- | The 'Bifunctor' instance provides functoriality in the error type. -}
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap f g p = embed $ bimap f g . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @'pure' x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

  * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Parser s e) where
    {-# INLINE pure #-}
    pure :: a -> Parser s e a
    pure x = embed $ \ xs -> Success x xs

    {-# INLINE (<*>) #-}
    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = embed $ \ xs ->
        case run p xs of
            Error d      -> Error d
            Success f ys -> case run q ys of
                    Error e      -> Error e
                    Success x zs -> Success (f x) zs

{- | The 'Monad' instance.

The bind combinator @p >>= h@ first runs @p@ and then the parser obtained by applying the monadic
function @h@ to the result.

note(s):

  * As with @p \<*\> q@, @p >>= h@ short-circuits on @p@ erroring out.
-}
instance Monad (Parser s e) where
    {-# INLINE (>>=) #-}
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = embed $ \ xs ->
        case run p xs of
            Error e      -> Error e
            Success x ys -> run (h x) ys

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

note(s):

  * The parser  @p \<|\> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.
-}
instance Monoid e => Alternative (Parser s e) where
    {-# INLINE empty #-}
    empty :: Parser s e a
    empty = embed $ const (Error mempty)

    {-# INLINE (<|>) #-}
    (<|>) :: Parser s e a -> Parser s e a -> Parser s e a
    (<|>) p q = embed $ \ xs ->
        case run p xs of
            x@(Success _ _) -> x
            Error e         ->
                case run q xs of
                    x'@(Success _ _) -> x'
                    Error e'         -> Error (e <> e')

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'ParserT' state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The 'get' parser does not throw an error or consume input while the 'put' parser allows changing
the t'ParserT' state.
-}
instance MonadState s (Parser s e) where
    {-# INLINE get #-}
    get :: Parser s e s
    get = embed $ \ xs -> Success xs xs

    {-# INLINE put #-}
    put :: s -> Parser s e ()
    put xs = embed $ const (Success () xs)


{- | Embed a parsing function in the t'Parser' monad. -}
{-# INLINE embed #-}
embed ::  (s ->  Result s e a) -> Parser s e a
embed = Parser

{- | Run the parser on the input and return the results.

The inverse of @'embed'@.
-}
{-# INLINE run #-}
run :: Parser s e a -> s ->  Result s e a
run (Parser f) = f

{- | Parse the input and return the result as an 'Either'. -}
{-# INLINE parse #-}
parse :: Parser s e a -> s -> e :+: (a, s)
parse p = toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Parser s e a -> s -> e :+: a
eval p = fmap fst . parse p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Parser s e a -> s -> e :+: s
remainder p = fmap snd . parse p
