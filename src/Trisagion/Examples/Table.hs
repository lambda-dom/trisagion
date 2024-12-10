{- |
Module: Trisagion.Examples.Table

Parser for the .tbl tabular format.
-}

module Trisagion.Examples.Table (
    -- * Error types.
    MismatchError (..),
    TableError (..),

    -- * Types.
    Table,

    -- ** Constructors.
    makeTable,

    -- * Parsers.
    parseTable,

    -- ** Auxiliary parsers.
    parseRows,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Void (absurd)

-- Libraries.
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..))
import Data.Text (Text)
import Data.Vector.Strict (Vector, fromList)

-- Package.
import Trisagion.Lib.NonEmpty (zipExact)
import Trisagion.Types.ParseError (ParseError, makeParseError)
import Trisagion.Get (Get)
import Trisagion.Getters.Combinators (onParseError)
import Trisagion.Getters.Combinators qualified as Getters (maybe)
import Trisagion.Examples.Table.Parsers (Lines, parseHeader, parseFields, parseLine)


{- | The @MismatchError@ error type, raised when the number of values and field names are not equal. -}
data MismatchError = MismatchError
    deriving stock (Eq, Show)

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


{- | Parser for a non-empty list of .tbl table rows. -}
parseRows :: NonEmpty Text -> Get Lines (ParseError Lines TableError) [NonEmpty (Text, Text)]
parseRows fields = go
    where
        go = do
            s <- get
            r <- first absurd $ Getters.maybe parseLine
            case r of
                Nothing -> pure []
                Just ts ->
                    case nonEmpty ts of
                        -- Case of empty line.
                        Nothing -> go
                        Just fs ->
                            case zipExact fields fs of
                                Nothing -> throwError $ makeParseError s RowError
                                Just ps -> (ps :) <$> go

{- | Parser for an entire .tbl table. -}
parseTable
    :: (NonEmpty (Text, Text) -> a)     -- ^ Row conversion function.
    -> Get Lines (ParseError Lines TableError) (Table a)
parseTable f = do
        _      <- onParseError HeaderError parseHeader
        fields <- onParseError FieldsError parseFields
        makeTable . fmap f <$> parseRows fields
