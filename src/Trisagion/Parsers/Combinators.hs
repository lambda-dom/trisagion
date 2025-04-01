{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * 'Applicative' parsers.
    value,

    -- * 'Alternative' parsers.
    either,

) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (either)

-- Base.
import Control.Applicative (Alternative ((<|>)))
import Data.Void (Void)

-- Package.
import Trisagion.Parser (Parser, (:+:))


{- | Embed a value in the 'Parser' monad.

The difference with 'pure' from 'Applicative' is the more precise signature.
-}
value :: a -> Parser s Void a
value = pure


{- | Run the first parser and if it fails run the second. Return the results as an @'Either'@.

note(s):

    * The parser is @'Left'@-biased; if the first parser is successful the second never runs.
-}
either :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (a :+: b)
either q p = (Left <$> q) <|> (Right <$> p)
