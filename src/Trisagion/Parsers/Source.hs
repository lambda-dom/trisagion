{- |
Module: Trisagion.Parsers.Streamable

Parsers with @'Source' a s@ constraints.
-}

module Trisagion.Parsers.Source (
    -- * Error types.
    InputError (..),
    ValidationError (..),

    -- * Parsers @'Source' a s => 'Parser' s e a@.
    one,
    eoi,
    skipOne,
    peek,
    satisfy,
    matchOne,
    oneOf,
) where

-- Imports.
-- Base.
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Except (MonadError (..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source (..))
import qualified Trisagion.Typeclasses.Source as Source (null)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (lookAhead, validate)


-- $setup
-- >>> import Trisagion.Parser


{- | The @InputError@ error type. -}
type InputError :: Type
newtype InputError = InputError Int
    deriving stock (Eq, Show)

{- | Monofunctoriality of t'InputError'. -}
instance MonoFunctor Int InputError where
    {-# INLINE monomap #-}
    monomap :: (Int -> Int) -> InputError -> InputError
    monomap f (InputError n) = InputError (f n)


{- | The @ValidationError@ error tag type thrown on failed validations. -}
type ValidationError :: Type -> Type
newtype ValidationError e = ValidationError e
    deriving stock (Eq, Show, Functor, Foldable, Traversable)
    deriving (Applicative, Monad) via Identity


{- | Parse one element from the input stream.

=== __Examples:__

>>> parse one "0123"
Right ('0',"123")

>>> parse one ""
Left (InputError 1)
-}
{-# INLINE one #-}
one :: Source a s => Parser s InputError a
one = do
    r <- gets uncons
    case r of
        Just (x, xs) -> put xs $> x
        _            -> throwError $ InputError 1

{- | Parser returning 'True' if there are no more elements in the input stream.

=== __Examples:__

>>> parse eoi "0123"
Right (False,"0123")

>>> parse eoi ""
Right (True,"")
-}
{-# INLINE eoi #-}
eoi :: Source a s => Parser s Void Bool
eoi = gets Source.null

{- | Skip one element from the input stream.

=== __Examples:__

>>> parse skipOne "0123"
Right ((),"123")

>>> parse skipOne ""
Right ((),"")
-}
{-# INLINE skipOne #-}
skipOne :: Source a s => Parser s Void ()
skipOne = gets dropOne >>= put

{- | Parse one element from the input stream but without consuming input.

=== __Examples:__

>>> parse peek "0123"
Right (Just '0',"0123")

>>> parse peek ""
Right (Nothing,"")
-}
{-# INLINE peek #-}
peek :: Source a s => Parser s Void (Maybe a)
peek = either (const Nothing) Just <$> lookAhead one

{- | Parse one element from the input stream satisfying a predicate.

=== __Examples:__

>>> parse (satisfy ('0' ==)) "0123"
Right ('0',"123")

>>> parse (satisfy ('1' ==)) "0123"
Left (Left (ValidationError '0'))

>>> parse (satisfy ('0' ==)) ""
Left (Right (InputError 1))
-}
{-# INLINE satisfy #-}
satisfy
    :: Source a s
    => (a -> Bool)                      -- ^ Predicate on @a@.
    -> Parser s (ValidationError a :+: InputError) a
satisfy p = validate (\ x -> if p x then Right x else Left $ ValidationError x) one

{- | Parse one element from the input stream matching an @x :: a@.

=== __Examples:__

>>> parse (matchOne '0') "0123"
Right ('0',"123")

>>> parse (matchOne '1') "0123"
Left (Left (ValidationError '0'))

>>> parse (matchOne '0') ""
Left (Right (InputError 1))
-}
{-# INLINE matchOne #-}
matchOne
    :: (Source a s, Eq a)
    => a                                -- ^ Matching @x :: a@.
    -> Parser s (ValidationError a :+: InputError) a
matchOne x = satisfy (== x)

{- | Parse one element from the input stream that is an element of a foldable.

=== __Examples:__

>>> parse (oneOf "01") "0123"
Right ('0',"123")

>>> parse (oneOf "12") "0123"
Left (Left (ValidationError '0'))

>>> parse (oneOf "12") ""
Left (Right (InputError 1))
-}
{-# INLINE oneOf #-}
oneOf
    :: (Source a s, Eq a, Foldable t)
    => t a                              -- ^ Foldable of @a@'s against which to test inclusion.
    -> Parser s (ValidationError a :+: InputError) a
oneOf xs = satisfy (`elem` xs)
