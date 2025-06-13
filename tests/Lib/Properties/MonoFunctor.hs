module Lib.Properties.MonoFunctor (
    -- * Monofunctor properties.
    prop_monofunctor_identity,
    prop_monofunctor_composition,
) where

-- Imports.
-- Base.
import Data.List.NonEmpty (NonEmpty (..))

-- Testing library.
import Hedgehog (PropertyT, Gen, forAll)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Lib.Utils (withBinary)

-- Package testing.
import Lib.Function (Function, fromFunction, functions)
import Lib.Property (prop_function_extensional_equality)


{- | Monofunctors preserve the identity. -}
prop_monofunctor_identity
    :: (Monad m, MonoFunctor s, Eq s, Show s)
    => Gen s
    -> PropertyT m ()
prop_monofunctor_identity = prop_function_extensional_equality (monomap id) id

{- | Monofunctors preserve composition. -}
prop_monofunctor_composition
    :: forall m s .(Monad m, MonoFunctor s, Eq s, Show s, Show (ElementOf s), Ord (ElementOf s))
    => Gen (ElementOf s)
    -> Gen s
    -> PropertyT m ()
prop_monofunctor_composition elems streams = do
    f <- nat <$> forAll (functions elems elems)
    g <- nat <$> forAll (functions elems elems)
    prop_function_extensional_equality
        (monomap (f . g))
        (monomap f . monomap g)
        streams
    where
        nat :: Function (ElementOf s) (ElementOf s) -> ElementOf s -> ElementOf s
        nat = fromFunction (id :| []) ((.) :| (withBinary <$> [min, max]))
