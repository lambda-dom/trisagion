{- |
Module: Trisagion.Typeclasses.Text

The @Text@ typeclass for text builders.
-}

module Trisagion.Typeclasses.Text (
    -- * Typeclasses.
    Textual (..),
) where

-- Imports.
-- Libraries.
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Builder as Bytes (Builder, charUtf8, stringUtf8, byteString)

-- Package.
import Trisagion.Typeclasses.Builder (Builder)



{- | The @Text@ typeclass for text builders.

This class, like its binary analog 'Trisagion.Typeclasses.Binary.Binary' is used for optimization
purposes only, as it does not add any new operations that cannot be done with the parent 'Builder'
superclass.
-}
class Builder m => Textual m where
    {-# MINIMAL char #-}

    {- | Serialize a utf8 'Char'. -}
    char :: Char -> m

    {- | Serialize a utf8-encoded 'String'. -}
    string :: String -> m
    string = foldMap char

    {- | Serialize a utf8-encoded (strict) 'Text'. -}
    text :: Text -> m
    text = string . unpack


-- Instances.
instance Textual Bytes.Builder where
    {-# INLINE char #-}
    char :: Char -> Bytes.Builder
    char = Bytes.charUtf8

    {-# INLINE string #-}
    string :: String -> Bytes.Builder
    string = Bytes.stringUtf8

    {-# INLINE text #-}
    text :: Text -> Bytes.Builder
    text = Bytes.byteString . encodeUtf8
