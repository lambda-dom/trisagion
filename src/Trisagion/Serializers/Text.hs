{- |
Module: Trisagion.Serializers.Text

Serializers @'Textual' m => 'Serializer' m a@.
-}

module Trisagion.Serializers.Text (
    -- * Serializers @'Textual' m => 'Serializer' m a@.
    char,
    string,
    text,
) where

-- Imports.
-- Libraries.
import Data.Text (Text)

-- Package.
import Trisagion.Typeclasses.Text (Textual)
import qualified Trisagion.Typeclasses.Text as Text (char, string, text)
import Trisagion.Serializer (Serializer, embed)


{- | Serialize a utf8-encoded 'Char'. -}
{-# INLINE char #-}
char :: Textual m => Serializer m Char
char = embed Text.char

{- | Serialize a utf8-encoded 'String'. -}
{-# INLINE string #-}
string :: Textual m => Serializer m String
string = embed Text.string

{- | Serialize a 'Text'. -}
{-# INLINE text #-}
text :: Textual m => Serializer m Text
text = embed Text.text
