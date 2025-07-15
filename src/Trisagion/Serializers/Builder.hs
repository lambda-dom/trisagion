{- |
Module: Trisagion.Serializers.Builder

Serializers @'Builder' m => 'Serializer' m a@.
-}

module Trisagion.Serializers.Builder (
    -- * Serializers @'Builder' m => 'Serializer' m a@.
    one,
    many,
    pack,
) where

-- Imports.
-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Builder (Builder (BuilderOf))
import qualified Trisagion.Typeclasses.Builder as Builder (one, many, pack)
import Trisagion.Serializer (Serializer, embed)


{- | Serialize a single @'ElementOf' ('BuilderOf' m)@. -}
one :: Builder m => Serializer m (ElementOf (BuilderOf m))
one = embed Builder.one

{- | Serialize a single @'ElementOf' ('BuilderOf' m)@. -}
many :: (Builder m, Foldable t) => Serializer m (t (ElementOf (BuilderOf m)))
many = embed Builder.many

{- | Serialize a single @'ElementOf' ('BuilderOf' m)@. -}
pack :: Builder m => Serializer m (BuilderOf m)
pack = embed Builder.pack
