module Lib.Properties.Builder (
    -- * Generators.
    builders,

    -- * 'Builder' property group.
    builderLaws,
) where

-- Imports.
-- Base.
import Data.Word (Word32)

-- Testing library.
import Hedgehog (Gen, Property, PropertyT, property)
import qualified Hedgehog.Gen as Gen (list)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import qualified Hedgehog.Range as Range (linear)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Builder (Builder (..))

-- Package testing
import Lib.Properties.Monoid (monoidMorphismLaws)
import Lib.Property (prop_pair_isomorphism, prop_function_extensional_equality)


{- | Generator for 'Builder's. -}
builders :: Builder a => Word32 -> Gen (ElementOf (BuilderOf a)) -> Gen a
builders n elems = many <$> Gen.list (Range.linear 0 (fromIntegral n)) elems


-- Properties.
prop_pack_unpack_inverse
    :: (Monad m, Builder a, Eq a, Show a, Eq (BuilderOf a), Show (BuilderOf a))
    => Gen a
    -> Gen (BuilderOf a)
    -> PropertyT m ()
prop_pack_unpack_inverse = prop_pair_isomorphism
        unpack
        pack

-- Extra argument for type inference. GHC 9.10 should obviate the need for this ugly hack.
prop_one_singleton
    :: forall m a . (Monad m, Builder a, Eq (ElementOf (BuilderOf a)), Show (ElementOf (BuilderOf a)))
    => Gen a
    -> Gen (ElementOf (BuilderOf a))
    -> PropertyT m ()
prop_one_singleton _ = prop_function_extensional_equality
    (toList . unpack . (one :: ElementOf (BuilderOf a) -> a))
    ( : [])


-- Property groups.
builderLaws
    :: forall a . (Builder a, Monoid (BuilderOf a), Eq a, Show a, Eq (BuilderOf a), Show (BuilderOf a),
        Eq (ElementOf (BuilderOf a)), Show (ElementOf (BuilderOf a)))
    => Gen (ElementOf (BuilderOf a))
    -> Gen a
    -> Gen (BuilderOf a)
    -> [(String, Property)]
builderLaws es bs is
    = ("unpack and one compatibility", property $ prop_one_singleton bs es)
    : ("unpack and pack are mutually inverse", property $ prop_pack_unpack_inverse bs is)
    : monoidMorphismLaws unpack bs
