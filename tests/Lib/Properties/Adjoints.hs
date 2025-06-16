module Lib.Properties.Adjoints (
    -- * Property groups.
    adjointParserLaws,
) where

-- Imports.
-- Base.
-- Testing library.
import Hedgehog (Gen, PropertyT, Property, property)

-- Package.
import Trisagion.Typeclasses.Builder (Builder (..))
import Trisagion.Parser (Parser, eval)
import Trisagion.Serializer (Serializer, serialize)

-- Package testing.
import Lib.Property (prop_function_extensional_equality)


-- Functions.
decoder :: Parser s e a -> s -> Maybe a
decoder p = either (const Nothing) Just . eval p

encoder :: Builder b => Serializer b a -> a -> BuilderOf b
encoder xs = unpack . serialize xs


-- Properties.
prop_left_adjoint
    :: (Monad m, Builder b, BuilderOf b ~ s, Show s, Eq s)
    => Parser s e a
    -> Serializer b a
    -> Gen s
    -> PropertyT m ()
prop_left_adjoint p s = prop_function_extensional_equality
    (fmap (encoder s) . decoder p)
    Just

prop_right_adjoint
    :: (Monad m, Builder b, BuilderOf b ~ s, Show a, Eq a)
    => Parser s e a
    -> Serializer b a
    -> Gen a
    -> PropertyT m ()
prop_right_adjoint p s = prop_function_extensional_equality
    (decoder p . encoder s)
    Just


-- Property groups.
adjointParserLaws
    :: (Builder b, BuilderOf b ~ s, Eq s, Show s, Eq a, Show a)
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
