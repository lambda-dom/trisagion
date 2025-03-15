{- |
Module: Trisagion.Parser

The @Parser@ parsing monad.
-}

module Trisagion.Parser (
    -- * The Parsing monad.
    Parser,

    -- ** Constructors.
    embed,

    -- ** Basic functions.
    run,
    eval,
    exec,

    -- * Error parsers.
    catchErrorWith,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Types.Result (Result (..), withResult)


{- | The @Parser@ monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap g f p = embed $ bimap g f . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @pure x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

    * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Parser s e) where
    pure :: a -> Parser s e a
    pure x = embed $ \ s -> Success x s

    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = embed $ withResult Error (\ f -> withResult Error (Success . f) . run q) . run p

{- | The 'Monad' instance.

The bind combinator @p >>= f@ first runs @p@ and then the parser obtained by applying the monadic
function @f@ to the result.

note(s):

    * As with @p \<*\> q@, @p >>= f@ short-circuits on @p@ erroring out.
-}
instance Monad (Parser s e) where
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = embed $ withResult Error (run . h) . run p

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
    @'catchError'@ to a type-changing version (see 'catchErrorWith'), it does not satisfy the
    associativity law.
-}
instance MonadError e (Parser s e) where
    throwError :: e -> Parser s e a
    throwError e = embed $ const (Error e)

    catchError :: Parser s e a -> (e -> Parser s e a) -> Parser s e a
    catchError p h = embed $ \ s ->
        -- Case statement instead of 'withResult' to make use of sharing in the Success branch.
        case run p s of
            r@(Success {}) -> r
            Error e        -> run (h e) s

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'Parser' state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The @'put'@ parser allows arbitrary state modifications. Provides monofunctoriality to @'Parser' s e a@
in @s@ via @'Control.MonadState.modify'@.
-}
instance MonadState s (Parser s e) where
    get :: Parser s e s
    get = embed $ \ s -> Success s s

    put :: s -> Parser s e ()
    put s = embed $ const (Success () s)


{- | Embed a parsing function in the t'Parser' monad. -}
embed :: (s -> Result s e a) -> Parser s e a
embed = Parser

{- | Run the parser on the input and return the results.

note(s):

    * The function @'run' :: 'Parser' s e a -> (s -> 'Result' s e a)@ is the inverse of 'embed'.
-}
run :: Parser s e a -> s -> Result s e a
run (Parser f) = f

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
eval :: Parser s e a -> s -> Either e a
eval p = withResult Left (\ x _ -> Right x) . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
exec :: Parser s e a -> s -> Either e s
exec p = withResult Left (\ _ s -> Right s) . run p


{- | Type-changing version of 'catchError'. -}
catchErrorWith
    :: Parser s e a                     -- ^ Parser to try.
    -> (e -> Parser s d a)              -- ^ Error handler.
    -> Parser s d a
catchErrorWith p h = embed $ \ s -> withResult (flip run s . h) Success $ run p s
