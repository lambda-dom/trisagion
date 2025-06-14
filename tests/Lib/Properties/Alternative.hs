{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Alternative law, right identity" #-}

module Lib.Properties.Alternative (
    -- * 'Alternative' properties.
    prop_alternative_left_identity,
    prop_alternative_right_identity,
    prop_alternative_associativity,
    prop_alternative_left_catch,
    prop_alternative_left_absorption,
    prop_alternative_left_zero,

    -- * Property groups.
    alternativeLaws,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word32)

-- Testing library.
import Hedgehog (Gen, PropertyT, forAll, Property, property)
import qualified Hedgehog.Gen as Gen (list)
import qualified Hedgehog.Range as Range (linear)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Streams.Counter (initialize, Counter)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (pairWith)

-- Package testing.
import Lib.ParserExp (ParserExp, fromParserExp, parserExps)
import Lib.Property (prop_parser_extensional_equality)


-- ParserExp conversion function.
nat :: (HasOffset s, Ord (ElementOf s)) => ParserExp e (ElementOf s) -> Parser s (ParseError e) (ElementOf s)
nat = fromParserExp id unaries binaries
    where
        unaries = id :| []
        binaries = (<|>) :| [(*>), (<*), pairWith min, pairWith max]


{- | Left identity for the 'Alternative' instance. -}
prop_alternative_left_identity
    :: forall m s . (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_alternative_left_identity elems streams = do
        p <- nat <$> forAll ps
        prop_parser_extensional_equality
            (empty <|> p)
            p
            streams
    where
        ps :: Gen (ParserExp (ElementOf s) (ElementOf s))
        ps = parserExps elems elems

{- | Left identity for the 'Alternative' instance. -}
prop_alternative_right_identity
    :: forall m s . (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_alternative_right_identity elems streams = do
        p <- nat <$> forAll (parserExps elems elems)
        prop_parser_extensional_equality
            (p <|> empty)
            p
            streams

{- | Associativity for the 'Alternative' instance. -}
prop_alternative_associativity
    :: forall m s . (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_alternative_associativity elems streams = do
    p <- nat <$> forAll (parserExps elems elems)
    q <- nat <$> forAll (parserExps elems elems)
    s <- nat <$> forAll (parserExps elems elems)
    prop_parser_extensional_equality
        ((p <|> q) <|> s)
        (p <|> (q <|> s))
        streams

{- | Left catch for the 'Alternative' instance. -}
prop_alternative_left_catch
    :: forall m s . (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_alternative_left_catch elems streams = do
    p <- nat <$> forAll (parserExps elems elems)
    x <- forAll elems
    prop_parser_extensional_equality
        (pure x <|> p)
        (pure x)
        streams

{- | Left absorption for the 'Alternative' instance. -}
prop_alternative_left_absorption
    :: forall m s . (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_alternative_left_absorption elems streams = do
        p <- nat <$> forAll (parserExps elems elems)
        prop_parser_extensional_equality
            (unit <*> p)
            empty
            streams
    where
        unit = empty :: Parser s (ParseError (ElementOf s)) (ElementOf s -> ElementOf s)

{- | Left zero for the 'Alternative' instance. -}
prop_alternative_left_zero
    :: forall m s . (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_alternative_left_zero elems streams = do
    p <- nat <$> forAll (parserExps elems elems)
    prop_parser_extensional_equality
        (empty >>= const p)
        empty
        streams

{- | 'Alternative' instance laws property group. -}
alternativeLaws
    :: forall s . (Ord (ElementOf s), Show (ElementOf s))
    => Word32
    -> Gen (ElementOf s)
    -> [(String, Property)]
alternativeLaws n elems = fmap (property <$>)
        [
            ("Left identity", prop_alternative_left_identity elems streams),
            ("Right identity", prop_alternative_right_identity elems streams),
            ("Associativity", prop_alternative_associativity elems streams),
            ("Left catch", prop_alternative_left_catch elems streams),
            ("Left absorption", prop_alternative_left_absorption elems streams),
            ("Left zero", prop_alternative_left_zero elems streams)
        ]
    where
        streams :: Gen (Counter [ElementOf s])
        streams = initialize <$> Gen.list (Range.linear 0 (fromIntegral n)) elems
