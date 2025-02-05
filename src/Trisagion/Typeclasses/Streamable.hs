{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Re-exports.
    -- ** 'MonoFunctor' re-exports.
    ElementOf,

    -- * The 'Streamable' typeclass.
    -- 
    -- $streamable
    Streamable (..),
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import qualified Data.List as List (uncons)

-- Libraries.
import Data.MonoTraversable (MonoFunctor (..), Element, MonoFoldable)
import Data.Sequence (Seq (..))
import Data.Vector (Vector)
import qualified Data.Text as Text (Text, uncons)
import qualified Data.Text.Lazy as LazyText (Text, uncons)
import qualified Data.ByteString as Bytes (ByteString, uncons)
import qualified Data.ByteString.Lazy as LazyBytes (ByteString, uncons)
import qualified Data.Vector as Vector (uncons)

-- Packages.
import qualified Trisagion.Lib.NonEmpty as NonEmpty (uncons)


{- | A type alias for @'Element' s@.-}
type ElementOf s = Element s


-- $streamable
--
-- To describe the three laws for the 'Streamable' typeclass, we start with a definition.
--
-- __Definition__: Let @s@ and @t@ be two monofunctors with @'Element' s ~ 'Element' t ~ a@. A
-- function @h :: s -> t@ is /natural/ if for every @f :: a -> a@ we have the equality
--
-- @
--   omap f . h = h . omap f
-- @
--
-- Since @s@ is not polymorphic we do not have free theorems to rely on, so naturality must be
-- explicitly required:
--
-- __Naturality__: The function @'getOne' :: s -> 'Maybe' ('Element' s, s)@ is natural.
--
-- In case it is not clear, the 'MonoFunctor' instance for @'Maybe' ('Element' s, s)@ is:
--
-- @
--   omap :: ('Element' s -> 'Element' s) -> 'Maybe' ('Element' s, s) -> 'Maybe' ('Element' s, s)
--   omap f = fmap (bimap f (omap f))
-- @
-- 
-- Given the function @'getOne' :: s -> 'Maybe' ('Element' s, s)@ we can define the function
-- @s -> [Element s]@ by @'Data.List.unfoldr' 'getOne'@. This justifies the @'MonoFoldable' s@
-- constraint and becomes the second law:
--
-- __Foldability__:
--
-- prop> otoList = unfoldr getOne
--
-- Finally, the third law says that 'getOne' /really/ is uncons-ing.
--
-- __Unconsing__:
--
-- prop> otoList = maybe [] (\ (x, xs) -> x : otoList xs) . getOne


{- | The @Streamable@ typeclass of monomorphic, streamable functors. -}
class (MonoFunctor s, MonoFoldable s) => Streamable s where
    {-# MINIMAL getOne #-}

    {- | Get, or uncons, the first element of the streamable. -}
    getOne :: s -> Maybe (ElementOf s, s)


-- Instances.
instance Streamable Bytes.ByteString where
    {-# INLINE getOne #-}
    getOne :: Bytes.ByteString -> Maybe (Word8, Bytes.ByteString)
    getOne = Bytes.uncons

instance Streamable LazyBytes.ByteString where
    {-# INLINE getOne #-}
    getOne :: LazyBytes.ByteString -> Maybe (Word8, LazyBytes.ByteString)
    getOne = LazyBytes.uncons

instance Streamable Text.Text where
    {-# INLINE getOne #-}
    getOne :: Text.Text -> Maybe (Char, Text.Text)
    getOne = Text.uncons

instance Streamable LazyText.Text where
    {-# INLINE getOne #-}
    getOne :: LazyText.Text -> Maybe (Char, LazyText.Text)
    getOne = LazyText.uncons

instance Streamable [a] where
    {-# INLINE getOne #-}
    getOne :: [a] -> Maybe (a, [a])
    getOne = List.uncons

instance Streamable (NonEmpty a) where
    {-# INLINE getOne #-}
    getOne :: NonEmpty a -> Maybe (a, NonEmpty a)
    getOne = NonEmpty.uncons

instance Streamable (Seq a) where
    {-# INLINE getOne #-}
    getOne :: Seq a -> Maybe (Element (Seq a), Seq a)
    getOne Empty      = Nothing
    getOne (x :<| xs) = Just (x, xs)

instance Streamable (Vector a) where
    {-# INLINE getOne #-}
    getOne :: Vector a -> Maybe (a, Vector a)
    getOne = Vector.uncons
