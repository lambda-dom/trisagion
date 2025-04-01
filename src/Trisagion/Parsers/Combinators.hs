{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    lookAhead,
    maybe,

    -- * 'Applicative' parsers.
    value,

    -- * 'Alternative' parsers.
    either,

) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (either, maybe)
import qualified Prelude as Base (either)

-- Base.
import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- Package.
import Trisagion.Parser (Parser, (:+:), eval, get, backtrack)


{- | Run the parser and return the result, but do not consume any input. -}
lookAhead :: Parser s e a -> Parser s Void (e :+: a)
lookAhead p = eval p <$> first absurd get

{- | Run the parser and return the result as a 'Just'. If it errors, backtrack and return 'Nothing'.

The difference with @'Control.Applicative.optional'@ is the more precise type signature.
-}
maybe :: Parser s e a -> Parser s Void (Maybe a)
maybe p = Base.either (const Nothing) Just <$> backtrack p


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
