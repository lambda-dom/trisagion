{- |
Module: Trisagion.Examples.Table

Parser for the .tbl tabular format.
-}

module Trisagion.Examples.Table (
    -- * Error types.
    TableError (..),

    -- * Types.
    Table,

    -- ** Constructors.
    makeTable,

    -- * Parsers.
    parseTable,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty)

-- Libraries.
import Data.Text (Text)
import Data.Vector.Strict (Vector, fromList)

-- Package.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Get (Get)
import Trisagion.Getters.Combinators (onParseError)
import Trisagion.Examples.Table.Parsers (Lines, parseHeader, parseFields, parseRows)


{- | The @TableError@ error type. -}
data TableError
    = HeaderError
    | FieldsError
    | RowError
    deriving stock (Eq, Show)


{- | The @Table@ type. -}
newtype Table a = Table (Vector a)
    deriving stock (Eq, Show, Functor, Foldable, Traversable)


{- | Construct a table from a list. -}
makeTable :: [a] -> Table a
makeTable = Table . fromList


{- | Parser for an entire .tbl table. -}
parseTable
    :: (NonEmpty (Text, Text) -> a)     -- ^ Row conversion function.
    -> Get Lines (ParseError Lines TableError) (Table a)
parseTable f = do
    _      <- onParseError HeaderError parseHeader
    fields <- onParseError FieldsError parseFields
    bimap (fmap (const RowError)) (makeTable . fmap f) $ parseRows fields
