module Lib.ParserExp (
    -- * Types.
    ParserExp (..),

    -- * Functions.
    fromParserExp,

    -- * Generators.
    parserExps,
) where

-- Imports.
-- Base.
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word16)

-- Libraries.
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector (fromList)
import qualified Hedgehog.Range as Range (constantBounded)

-- Testing library.
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen (recursive, choice, word16, subterm2, subterm)

-- Package.
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError)
import Trisagion.Types.ParseError (ParseError)


{- | The @ParserExp@ type. -}
data ParserExp e a where
    Pure :: a -> ParserExp e a
    Throw :: e -> ParserExp e a
    Unary :: Word16 -> ParserExp e a -> ParserExp e a
    Binary :: Word16 -> ParserExp e a -> ParserExp e a -> ParserExp e a
    deriving stock (Eq, Show, Functor)


{- | Convert a 'ParserExp' into a parser. -}
fromParserExp
    :: forall s e a . (HasOffset s)
    => NonEmpty (Parser s (ParseError e) a -> Parser s (ParseError e) a)
    -> NonEmpty (Parser s (ParseError e) a -> Parser s (ParseError e) a -> Parser s (ParseError e) a)
    -> ParserExp e a
    -> Parser s (ParseError e) a
fromParserExp fs bs = go
    where
        uns :: Vector (Parser s (ParseError e) a -> Parser s (ParseError e) a)
        uns = Vector.fromList (toList fs)

        bns :: Vector (Parser s (ParseError e) a -> Parser s (ParseError e) a -> Parser s (ParseError e) a)
        bns = Vector.fromList (toList bs)

        go r = case r of
            Pure x       -> pure x
            Throw e      -> throwParseError e
            Unary i p    -> let h = uns ! (fromIntegral i `rem` length uns) in h (go p)
            Binary i p q -> let h = bns ! (fromIntegral i `rem` length bns) in h (go p) (go q)


{- | Generator for 'ParserExp'. -}
parserExps :: Gen a -> Gen e -> Gen (ParserExp e a)
parserExps as es = go
    where
        gen = Gen.word16 Range.constantBounded
        go = Gen.recursive
            Gen.choice
            [Pure <$> as, Throw <$> es]
            [
                gen >>= \ i -> Gen.subterm go (Unary i),
                gen >>= \ i -> Gen.subterm2 go go (Binary i)
            ]
