module Lib.Property (
    -- * Properties.
    -- ** Extensional functional equality.
    prop_function_extensional_equality,

    -- ** Isomorphisms.
    prop_pair_isomorphism,

    -- ** Parser equality.
    prop_parser_extensional_equality,

    -- * Property groups.
    makeGroup,

    -- * Combine testing groups.
    andM,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.String (IsString (..))

-- Testing library.
import Hedgehog (Gen, PropertyT, Property, Group (..), (===), forAll)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser, parse)


{- | Testing extensional equality of functions. -}
prop_function_extensional_equality
    :: (Monad m, Show a, Eq b, Show b)
    => (a -> b)
    -> (a -> b)
    -> Gen a
    -> PropertyT m ()
prop_function_extensional_equality f g gen = do
    x <- forAll gen
    f x === g x

{- | Testing that a pair of functions are mutually inverse. -}
prop_pair_isomorphism
    :: (Monad m, Eq a, Show a, Eq b, Show b)
    => (a -> b)
    -> (b -> a)
    -> Gen a
    -> Gen b
    -> PropertyT m ()
prop_pair_isomorphism f g elems invelems = do
    x <- forAll elems
    y <- forAll invelems
    ((g . f) x, (f . g) y) === (x, y)


{- | Testing extensional equality of parsers. -}
prop_parser_extensional_equality
    :: (Monad m, Eq a, Show a, Eq e, Show e, Eq s, Show s)
    => Parser s (ParseError e) a
    -> Parser s (ParseError e) a
    -> Gen s
    -> PropertyT m ()
prop_parser_extensional_equality p q streams = do
    xs <- forAll streams
    parse p xs === parse q xs


{- | Constructor for a property group. -}
makeGroup :: String -> [(String, Property)] -> Group
makeGroup name props = Group {
    groupName = fromString name,
    groupProperties = fmap (first fromString) props
    }


{- | Combine checking of test groups. -}
andM :: Monad m => [m Bool] -> m Bool
andM []       = pure True
andM (x : xs) = do
    b <- x
    if b then andM xs else pure False
