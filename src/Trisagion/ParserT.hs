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
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)

-- Package.
import Trisagion.Types.Result (Result (..), (:+:), toEither)


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
    pure x = embed $ pure . Success x

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
    empty = embed $ const . pure . Error $ mempty

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
                    Error e'         -> pure . Error $ e <> e'


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
