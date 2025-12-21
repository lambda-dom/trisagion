{- |
Module: Trisagion.ParserT

The @ParserT@ monad transformer.
-}

module Trisagion.ParserT (
    -- * The parsing monad transformer.
    ParserT,
    Parser,

    -- * Basic functions.
    embed,
    run,
    parse,
    eval,
    remainder,

    -- * Functoriality.
    hoist,

    -- * Error parsers.
    throw,
    catch,
    try,

    -- * State parsers.
    lift,
    lookAhead,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Void (Void)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Types.Result (Result (..), toEither)


{- | The parsing monad transformer @ParserT m s e a@.

@m@ is the underlying monad, @s@ is the input stream type, @e@ is the type of parsing errors that
the parser can throw and @a@ is the type of parsed values.
-}
type ParserT :: (Type -> Type) -> Type -> Type -> Type -> Type
newtype ParserT m s e a = ParserT (s ->  m (Result s e a))
    deriving stock Functor


{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Functor m => Bifunctor (ParserT m s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> ParserT m s d a -> ParserT m s e b
    bimap g f p = embed $ fmap (bimap g f) . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @'pure' x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

  * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Monad m => Applicative (ParserT m s e) where
    {-# INLINE pure #-}
    pure :: a -> ParserT m s e a
    pure x = embed $ \ xs -> pure $ Success x xs

    {-# INLINE (<*>) #-}
    (<*>) :: ParserT m s e (a -> b) -> ParserT m s e a -> ParserT m s e b
    (<*>) p q = embed $ \ xs -> do
        r <- run p xs
        case r of
            Error d      -> pure $ Error d
            Success f ys -> do
                s <- run q ys
                case s of
                    Error e      -> pure $ Error e
                    Success x zs -> pure $ Success (f x) zs

{- | The 'Monad' instance.

The bind combinator @p >>= h@ first runs @p@ and then the parser obtained by applying the monadic
function @h@ to the result.

note(s):

  * As with @p \<*\> q@, @p >>= h@ short-circuits on @p@ erroring out.
-}
instance Monad m => Monad (ParserT m s e) where
    {-# INLINE (>>=) #-}
    (>>=) :: ParserT m s e a -> (a -> ParserT m s e b) -> ParserT m s e b
    (>>=) p h = embed $ \ xs -> do
        r <- run p xs
        case r of
            Error e      -> pure $ Error e
            Success x ys -> run (h x) ys

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

note(s):

  * The parser  @p \<|\> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.
-}
instance (Monad m, Monoid e) => Alternative (ParserT m s e) where
    {-# INLINE empty #-}
    empty :: ParserT m s e a
    empty = embed . const . pure $ Error mempty

    {-# INLINE (<|>) #-}
    (<|>) :: ParserT m s e a -> ParserT m s e a -> ParserT m s e a
    (<|>) p q = ParserT $ \ xs -> do
        r <- run p xs
        case r of
            x@(Success _ _) -> pure x
            Error e         -> do
                s <- run q xs
                case s of
                    x'@(Success _ _) -> pure x'
                    Error e'         -> pure $ Error (e <> e')

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
instance Monad m => MonadState s (ParserT m s e) where
    {-# INLINE get #-}
    get :: ParserT m s e s
    get = embed $ \ xs -> pure $ Success xs xs

    {-# INLINE put #-}
    put :: s -> ParserT m s e ()
    put xs = embed . const . pure $ Success () xs

{- | The 'MonadError' instance.

The typeclass provides error handling for the t'ParserT' monad. The @'throwError' e@ parser fails
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
instance Monad m => MonadError e (ParserT m s e) where
    {-# INLINE throwError #-}
    throwError :: e -> ParserT m s e a
    throwError = throw

    {-# INLINE catchError #-}
    catchError :: ParserT m s e a -> (e -> ParserT m s e a) -> ParserT m s e a
    catchError p h = embed $ \ s -> do
        r <- run p s
        -- Case statement instead of 'catch' to make use of sharing in the Success branch.
        case r of
            x@(Success _ _) -> pure x
            Error e         -> run (h e) s


{- | Type alias for @t'ParserT' m@ specialized to @m ~ 'Identity'@.-}
type Parser :: Type -> Type -> Type -> Type
type Parser = ParserT Identity


{- | Embed a parsing function in the t'ParserT' monad. -}
{-# INLINE embed #-}
embed ::  (s ->  m (Result s e a)) -> ParserT m s e a
embed = ParserT

{- | Run the parser on the input and return the results. -}
{-# INLINE run #-}
run :: ParserT m s e a -> s ->  m (Result s e a)
run (ParserT f) = f

{- | Parse the input and return the results. -}
{-# INLINE parse #-}
parse :: Functor m => ParserT m s e a -> s -> m (e :+: (a, s))
parse p = fmap toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Functor m => ParserT m s e a -> s -> m (e :+: a)
eval p = fmap (fmap fst) . parse p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Functor m => ParserT m s e a -> s -> m (e :+: s)
remainder p = fmap (fmap snd) . parse p


{- | Functoriality in the monad @m@ type parameter. -}
hoist :: (forall b . m b -> n b) -> ParserT m s e a -> ParserT n s e a
hoist f p = embed $ \ s -> f (run p s)


{- | Parser that fails unconditionally with error @e@. -}
{-# INLINE throw #-}
throw :: Applicative m => e -> ParserT m s e a
throw e = embed . const . pure $ Error e

{- | The type-changing version of 'catchError'.

The parser @'catch' p h@ runs @p@ and if it throws an error @e@, backtracks and runs @h e@.
-}
{-# INLINE catch #-}
catch
    :: Monad m
    => ParserT m s d a                  -- ^ Parser to try.
    -> (d -> ParserT m s e a)           -- ^ Error handler.
    -> ParserT m s e a
catch p h = embed $ \ xs -> do
    r <- run p xs
    case r of
        Error e      -> run (h e) xs
        Success x ys -> pure $ Success x ys

{- | Parser implementing backtracking.

The parser @'try' p@ runs @p@ and returns the result as a 'Right'; on @p@ throwing an error, it
backtracks and returns the error as a 'Left'.
-}
{-# INLINE try #-}
try :: Monad m => ParserT m s e a -> ParserT m s Void (e :+: a)
try p = embed $ \ xs -> do
    r <- run p xs
    case r of
        Error e      -> pure $ Success (Left e) xs
        Success x ys -> pure $ Success (Right x) ys


{- | Lift a monadic action to the t'ParserT' monad.

This parser does not consume input and does not throw errors.
-}
{-# INLINE lift #-}
lift :: Monad m => m a -> ParserT m s e a
lift h = embed $ \ xs -> do
    x <- h
    pure $ Success x xs

{- | Run the parser and return the result, but do not consume any input. -}
{-# INLINE lookAhead #-}
lookAhead :: Monad m => ParserT m s e a -> ParserT m s Void (e :+: a)
lookAhead p = get >>= \ xs -> lift (eval p xs)
