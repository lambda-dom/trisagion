module Lib.Properties.Adjoints (
    -- * Property groups.
    adjointParserLaws,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Testing library.
import Hedgehog (Gen, PropertyT, Property, property)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable)
import Trisagion.Typeclasses.Streamable as Streamable (null)
import Trisagion.Typeclasses.Builder (Builder (..))
import Trisagion.Parser (Parser, parse)
import Trisagion.Serializer (Serializer, serialize)

-- Package testing.
import Lib.Property (prop_function_extensional_equality)


-- Functions.
decoder :: Parser s e a -> s -> Maybe (a, s)
decoder p = either (const Nothing) Just . parse p

encoder :: Builder b => Serializer b a -> a -> BuilderOf b
encoder xs = unpack . serialize xs


-- Properties.
prop_left_adjoint
    :: (Monad m, Builder b, BuilderOf b ~ s, Monoid s, Eq s, Show s, Streamable s)
    => Parser s e a
    -> Serializer b a
    -> Gen s
    -> PropertyT m ()
prop_left_adjoint p s = prop_function_extensional_equality
    (fmap (uncurry (<>) . first (encoder s)) . decoder p)
    (\ xs -> if Streamable.null xs then Nothing else Just xs)

prop_right_adjoint
    :: (Monad m, Builder b, BuilderOf b ~ s, Monoid s, Eq a, Show a, Eq s, Show s)
    => Parser s e a
    -> Serializer b a
    -> Gen a
    -> PropertyT m ()
prop_right_adjoint p s = prop_function_extensional_equality
    (decoder p . encoder s)
    (Just . (, mempty))


-- Property groups.
adjointParserLaws
    :: (Builder b, BuilderOf b ~ s, Monoid s, Eq s, Show s, Eq a, Show a)
    => Parser s e a
    -> Serializer b a
    -> Gen s
    -> Gen a
    -> [(String, Property)]
adjointParserLaws p s xs as = fmap property <$> props
    where
        props = [
            ("Left adjoint law", prop_left_adjoint p s xs),
            ("Right adjoint law", prop_right_adjoint p s as)
            ]
