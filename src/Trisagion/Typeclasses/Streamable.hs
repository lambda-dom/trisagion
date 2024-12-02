{- |
Module: Trisagion.Typeclasses.Streamable

The @Streamable@ typeclass.
-}

module Trisagion.Typeclasses.Streamable (
    -- * Typeclasses.
    Streamable (..),
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import qualified Data.List as List (uncons)
import qualified Data.List.NonEmpty as NonEmpty (uncons)

-- Libraries.
import Data.ByteString (ByteString)
import Data.MonoTraversable (MonoFunctor (..), Element, MonoFoldable)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Text as Text (uncons)
import qualified Data.ByteString as Bytes (uncons)
import qualified Data.Vector as Vector (uncons)


{- | The @Streamable@ typeclass of monomorphic, streamable functors. -}
class (MonoFunctor s, MonoFoldable s) => Streamable s where
    {-# MINIMAL getOne #-}

    {- | Get, or uncons, the first element of the streamable. -}
    getOne :: s -> Maybe (Element s, s)


-- Instances.
instance Streamable [a] where
    getOne :: [a] -> Maybe (a, [a])
    getOne = List.uncons

instance Streamable (NonEmpty a) where
    getOne :: NonEmpty a -> Maybe (a, NonEmpty a)
    getOne = switch . NonEmpty.uncons
        where
            switch :: (a, Maybe (NonEmpty a)) -> Maybe (a, NonEmpty a)
            switch (_, Nothing) = Nothing
            switch (x, Just xs) = Just (x, xs)

instance Streamable ByteString where
    getOne :: ByteString -> Maybe (Word8, ByteString)
    getOne = Bytes.uncons

instance Streamable (Seq a) where
    getOne :: Seq a -> Maybe (Element (Seq a), Seq a)
    getOne Empty      = Nothing
    getOne (x :<| xs) = Just (x, xs)

instance Streamable Text where
    getOne :: Text -> Maybe (Char, Text)
    getOne = Text.uncons

instance Streamable (Vector a) where
    getOne :: Vector a -> Maybe (a, Vector a)
    getOne = Vector.uncons
