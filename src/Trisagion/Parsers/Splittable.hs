{- |
Module: Trisagion.Parsers.Splittable

Parsers with @'Splittable' s@ constraints on the input stream @s@.
-}

module Trisagion.Parsers.Splittable (
    -- * Parsers with a @'Splittable' s => 'Parser' s e a@ constraint.
    takePrefix,
    dropPrefix,
    takeExact,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (splitAt)

-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.State (MonadState(..), gets)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.Splittable (Splittable(..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip)
import Trisagion.Parsers.ParseError (validate)
import Trisagion.Parsers.Streamable (InputError (..))


{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
takePrefix :: Splittable s => Word -> Parser s Void (PrefixOf s)
takePrefix n = do
    (prefix, suffix) <- gets $ splitAt n
    put suffix $> prefix

{- | Drop a fixed size prefix from the stream. -}
dropPrefix :: Splittable s => Word -> Parser s Void ()
dropPrefix = skip . takePrefix

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix, which is @O(n)@ for some types
    (e.g. @Text@). For other types like @LazyText@, it forces the entirety of the value into memory
    which is probably not desired.
-}
takeExact
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => Word -> Parser s (ParseError s InputError) (PrefixOf s)
takeExact n = first (fmap (either absurd id)) $ validate v (first absurd $ takePrefix n)
    where
        v prefix =
            if monolength prefix < n
                then Left $ InputError n
                else Right prefix

