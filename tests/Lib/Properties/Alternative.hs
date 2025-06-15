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
    prop_alternative_left_distributivity,

    -- * Property groups.
    alternativeLaws,
    distributiveLaw,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (absurd)
import Data.Word (Word8)

-- Testing library.
import Hedgehog (Gen, PropertyT, Property, forAll, property)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError)
import Trisagion.Parsers.Combinators (pairWith, count)
import Trisagion.Parsers.Streamable (one)

-- Package testing.
import Lib.ParserExp (ParserExp, fromParserExp, parserExps)
import Lib.Property (prop_parser_extensional_equality)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


-- ParserExp conversion function.
nat :: (HasOffset s, Ord (ElementOf s)) => ParserExp e (ElementOf s) -> Parser s (ParseError e) (ElementOf s)
nat = fromParserExp id unaries binaries
    where
        unaries = id :| []
        binaries = (<|>) :| [(*>), (<*), pairWith min, pairWith max]


{- | A parser for functions @Word8 -> Word8@. -}
parseFunction :: (HasOffset s, ElementOf s ~ Word8) => Parser s (ParseError Word8) (Word8 -> Word8)
parseFunction = do
    x <- first (fmap absurd) one
    case x `rem` 4 of
        0 -> throwParseError x
        1 -> first (fmap absurd) one >>= \ n -> bimap (fmap absurd) (const (n *)) $ count (fromIntegral n) one
        2 -> first (fmap absurd) one >>= \ n -> pure (max n)
        _ -> pure id


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
    :: (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
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
    :: (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
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
    :: (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
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
        -- Needed for inference.
        unit = empty :: Parser s (ParseError (ElementOf s)) (ElementOf s -> ElementOf s)

{- | Left zero for the 'Alternative' instance. -}
prop_alternative_left_zero
    :: (Monad m, HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_alternative_left_zero elems streams = do
    p <- nat <$> forAll (parserExps elems elems)
    prop_parser_extensional_equality
        (empty >>= const p)
        empty
        streams

{- | Left distributivity. -}
prop_alternative_left_distributivity
    :: (Monad m, HasOffset s, ElementOf s ~ Word8, Eq s, Show s)
    => Gen s
    -> PropertyT m ()
prop_alternative_left_distributivity streams = do
        let f = parseFunction
        x <- nat <$> forAll (parserExps elems elems)
        y <- nat <$> forAll (parserExps elems elems)
        prop_parser_extensional_equality
            (f <*> (x <|> y))
            ((f <*> x) <|> (f <*> y))
            streams
    where
        elems :: Gen Word8
        elems = Gen.word8 Range.constantBounded

{- | 'Alternative' instance laws property group. -}
alternativeLaws
    :: (HasOffset s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> [(String, Property)]
alternativeLaws elems streams = fmap (property <$>)
        [
            ("Left identity", prop_alternative_left_identity elems streams),
            ("Right identity", prop_alternative_right_identity elems streams),
            ("Associativity", prop_alternative_associativity elems streams),
            ("Left catch", prop_alternative_left_catch elems streams),
            ("Left absorption", prop_alternative_left_absorption elems streams),
            ("Left zero", prop_alternative_left_zero elems streams)
        ]

{- | Left distributivity for 'Alternative'. -}
distributiveLaw
    :: (HasOffset s, ElementOf s ~ Word8, Eq s, Show s)
    => Gen s
    -> [(String, Property)]
distributiveLaw streams = fmap (property <$>)
    [("Left distributivity", prop_alternative_left_distributivity streams)]
