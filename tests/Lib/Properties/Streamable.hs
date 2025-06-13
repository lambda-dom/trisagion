module Lib.Properties.Streamable (
    -- * Streamable properties.
    prop_uncons_mononaturality,
    prop_uncons_lists,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..))

-- Testing library.
import Hedgehog (PropertyT, Gen, forAll)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor(..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))

-- Testing helpers.
import Trisagion.Lib.Utils (withBinary)
import Lib.Function (Function, functions, fromFunction)
import Lib.Property (prop_function_extensional_equality)


{- | Mononaturality of 'uncons'. -}
prop_uncons_mononaturality
    :: (Monad m, Streamable s, Eq s, Show s, Ord (ElementOf s), Show (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_uncons_mononaturality elems streams = do
        f <- nat <$> forAll (functions elems elems)
        prop_function_extensional_equality
            (uncons. monomap f)
            (fmap (bimap f (monomap f)) . uncons)
            streams
    where
        nat :: Ord a => Function a a -> a -> a 
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))

{- | 'Streamable' list law for 'uncons'. -}
prop_uncons_lists
    :: (Monad m, Streamable s, Eq (ElementOf s), Show (ElementOf s), Show s)
    => Gen s
    -> PropertyT m ()
prop_uncons_lists = prop_function_extensional_equality
    toList
    (maybe [] (\ (y, ys) -> y : toList ys) . uncons)
