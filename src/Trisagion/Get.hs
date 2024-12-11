{- |
Module: Trisagion.Get

The @Get@ parsing monad.
-}

module Trisagion.Get (
    -- * The parsing monad.
    Get,

    -- ** Basic functions.
    embed,
    run,
    eval,
    exec,

    -- * Error parsers.
    handleError,

    -- * Handling t'ParseError'.
    throwParseError,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Types.Result (Result (..), withResult)
import Trisagion.Types.ParseError (ParseError (..), makeParseError)


{- | The @Get@ parsing monad. -}
newtype Get s e a = Get (s -> Result s e a)


-- Instances.
instance Functor (Get s e) where
    fmap :: (a -> b) -> Get s e a -> Get s e b
    fmap f p = embed $ fmap f . run p

{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Bifunctor (Get s) where
    bimap :: (d -> e) -> (a -> b) -> Get s d a -> Get s e b
    bimap g f p = embed $ bimap g f . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @pure x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

    * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Get s e) where
    pure :: a -> Get s e a
    pure x = embed $ \ s -> Success x s

    (<*>) :: Get s e (a -> b) -> Get s e a -> Get s e b
    (<*>) p q = embed $ \ s ->
        case run p s of
            Error e     -> Error e
            Success f t ->
                case run q t of
                    Error e'    -> Error e'
                    Success x u -> Success (f x) u

{- | The 'Monad' instance.

The bind combinator @p >>= f@ first runs @p@ and then the parser obtained by applying the monadic
function @f@ to the result.

note(s):

    * As with @p \<*\> q@, @p >>= f@ short-circuits on @p@ erroring out.
-}
instance Monad (Get s e) where
    (>>=) :: Get s e a -> (a -> Get s e b) -> Get s e b
    (>>=) p h = embed $ \ s ->
        case run p s of
            Error e     -> Error e
            Success x t -> run (h x) t

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

note(s):

    * The parser  @p \<|> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.
-}
instance Monoid e => Alternative (Get s e) where
    empty :: Get s e a
    empty = embed $ const (Error mempty)

    (<|>) :: Get s e a -> Get s e a -> Get s e a
    (<|>) p q = embed $ \ s ->
        case run p s of
            r@(Success _ _) -> r
            Error e         ->
                case run q s of
                    r'@(Success _ _) -> r'
                    Error e'         -> Error $ e <> e'

    many :: Get s e a -> Get s e [a]
    many p = go
        where
            go = embed $ \ s ->
                case run p s of
                    Error _     -> Success [] s
                    Success x t -> (x :) <$> run go t

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'Get' parser state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The @'put'@ parser allows arbitrary state modifications. Provides mono-functoriality in @s@ via
@'Control.MonadState.modify'@.
-}
instance MonadState s (Get s e) where
    get :: Get s e s
    get = embed $ \ s -> Success s s

    put :: s -> Get s e ()
    put s = embed $ const (Success () s)

{- | The 'MonadError' instance.

The typeclass provides error handling for the t'Get' monad. The @'throwError' e@ parser fails
unconditionally with @e@. The parser @'catchError' p h@ first tries @p@. If it succeeds, it returns
the parsed result, if it fails, it backtracks and runs the parser @h e@ where @e@ is the error
returned by @p@.

The difference between 'MonadError' and 'Alternative' regarding errors, is analogous to the
difference between 'Monad' and 'Applicative'. Just as with the former, 'MonadError' allows the
continuation to depend on the specific error that was thrown.

note(s):

    * The monad analogy is not precise, because even if we make the obvious generalization of
    @'catchError'@ to a type-changing version (see 'handleError'), it does not satisfy the
    associativity law.
-}
instance MonadError e (Get s e) where
    throwError :: e -> Get s e a
    throwError e = embed $ const (Error e)

    catchError :: Get s e a -> (e -> Get s e a) -> Get s e a
    catchError = handleError


{- | Embed a parsing function in the t'Get' monad. -}
embed :: (s -> Result s e a) -> Get s e a
embed = Get 

{- | Run the parser on the input and return the parsed result. -}
run :: Get s e a -> s -> Result s e a
run (Get f) = f

{- | Evaluate the parser on the input and return the result, discarding the state. -}
eval :: Get s e a -> s -> Either e a
eval p = withResult Left (\ _ x -> Right x) . run p

{- | Run the parser on the input and return the updated state, discarding the value. -}
exec :: Get s e a -> s -> Either e s
exec p = withResult Left (\ s _ -> Right s) . run p


{- | Type-changing version of 'catchError'. -}
handleError
    :: Get s e a            -- ^ Parser to try.
    -> (e -> Get s d a)     -- ^ Error handler.
    -> Get s d a
handleError p h = embed $ \ s ->
        case run p s of
            Success x t -> Success x t
            Error e     -> run (h e) s

{- | Parser that throws @t'ParseError' s e@ with specified state and tag.

The state component is the current parser state and the backtrace is a @'Nothing'@ of type
@'Maybe' (ParseError s 'Void')@.
-}
throwParseError :: e -> Get s (ParseError s e) Void
throwParseError e = embed $ \ s -> Error (makeParseError s e)
