{- |
Module: Trisagion.Parsers.IStream

Parsers with @'IStream' a s@ constraints.
-}

module Trisagion.Parsers.IStream (
    -- * Error types.
    InputError (..),
    ValidationError (..),

    -- * Parsers @'IStream' a s => 'Parser' s e a@.
    one,
    eoi,
    skipOne,
    peek,
    satisfy,
    matchOne,
    oneOf,
    matchFoldable,
) where

-- Imports.
-- Base.
import qualified Data.Foldable as Foldable (toList)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Void (Void)

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.IStream (IStream (..))
import Trisagion.Parser (Parser, get, put, throw)
import Trisagion.Parsers.Combinators (lookAhead, validate, chain_)


-- $setup
-- >>> import Trisagion.Parser


{- | The @InputError@ error type. -}
type InputError :: Type
newtype InputError = InputError Int
    deriving stock (Eq, Show)


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
one :: IStream a s => Parser s InputError a
one = do
    r <- fmap uncons get
    case r of
        Just (x, xs) -> put xs $> x
        _            -> throw $ InputError 1

{- | Parser returning 'True' if there are no more elements in the input stream.

=== __Examples:__

>>> parse eoi "0123"
Right (False,"0123")

>>> parse eoi ""
Right (True,"")
-}
{-# INLINE eoi #-}
eoi :: IStream a s => Parser s Void Bool
eoi = fmap isNull get

{- | Skip one element from the input stream.

=== __Examples:__

>>> parse skipOne "0123"
Right ((),"123")

>>> parse skipOne ""
Right ((),"")
-}
{-# INLINE skipOne #-}
skipOne :: IStream a s => Parser s Void ()
skipOne = fmap dropOne get >>= put

{- | Parse one element from the input stream but without consuming input.

=== __Examples:__

>>> parse peek "0123"
Right (Just '0',"0123")

>>> parse peek ""
Right (Nothing,"")
-}
{-# INLINE peek #-}
peek :: IStream a s => Parser s Void (Maybe a)
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
    :: IStream a s
    => (a -> Bool)                      -- ^ Predicate on @a@.
    -> Parser s (InputError :+: ValidationError a) a
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
    :: (IStream a s, Eq a)
    => a                                -- ^ Matching @x :: a@.
    -> Parser s (InputError :+: ValidationError a) a
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
    :: (IStream a s, Eq a, Foldable t)
    => t a                              -- ^ Foldable of @a@'s against which to test inclusion.
    -> Parser s (InputError :+: ValidationError a) a
oneOf xs = satisfy (`elem` xs)

{- | Parse a matching prefix from the input stream against a foldable.

=== __Examples:__

>>> parse (matchFoldable "01") "0123"
Right ((),"23")

>>> parse (matchFoldable "02") "0123"
Left (Left (ValidationError '1'))
-}
{-# INLINE matchFoldable #-}
matchFoldable
    :: (IStream a s, Foldable t, Eq a)
    => t a                              -- ^ Foldable of @a@'s to match against.
    -> Parser s (InputError :+: ValidationError a) ()
matchFoldable xs = chain_  $ fmap matchOne (Foldable.toList xs)
