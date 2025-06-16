module Lib.Properties.Builder (
    -- * 'Builder' property group.
    builderLaws,
) where

-- Imports.
-- Testing library.
import Hedgehog (Gen, Property, PropertyT, property)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Builder (Builder (..))

-- Package testing
import Lib.Property (prop_function_extensional_equality)


-- Properties.
-- Extra argument for type inference. GHC 9.10 should obviate the need for this ugly hack.
prop_one_singleton
    :: forall m a . (Monad m, Builder a, Eq (ElementOf (BuilderOf a)), Show (ElementOf (BuilderOf a)))
    => Gen a
    -> Gen (ElementOf (BuilderOf a))
    -> PropertyT m ()
prop_one_singleton _ = prop_function_extensional_equality
    (toList . unpack . (one :: ElementOf (BuilderOf a) -> a))
    ( : [])

prop_unpack_left_inverse
    :: forall m a . (Monad m, Builder a, Show (BuilderOf a), Eq (BuilderOf a))
    => Gen (BuilderOf a)
    -> Gen a
    -> PropertyT m ()
prop_unpack_left_inverse bs _ = prop_function_extensional_equality
    (unpack . (pack :: BuilderOf a -> a))
    id
    bs

{- | Builder laws. -}
builderLaws
    :: forall a . (Builder a, Eq (BuilderOf a), Show (BuilderOf a),
        Eq (ElementOf (BuilderOf a)), Show (ElementOf (BuilderOf a)))
    => Gen (ElementOf (BuilderOf a))
    -> Gen (BuilderOf a)
    -> Gen a
    -> [(String, Property)]
builderLaws es bs ls
    = ("unpack and one compatibility", property $ prop_one_singleton ls es)
    : [("pack a left inverse of unpack", property $ prop_unpack_left_inverse bs ls)]
    -- : monoidMorphismLaws unpack ls
