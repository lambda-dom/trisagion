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
    parseRows,
    parseTable,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Void (absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Data.Text (Text)
import Data.Vector.Strict (Vector, fromList)

-- Package.
import Trisagion.Lib.NonEmpty (zipExact)
import Trisagion.Types.ParseError (ParseError (..), withParseError, makeParseError)
import Trisagion.Get (Get)
import Trisagion.Getters.Combinators (observe)
import Trisagion.Examples.Table.Parsers (Lines, parseHeader, parseFields)
import Control.Monad.State (MonadState(..))


{- | The @TableError@ error type. -}
data TableError e
    = HeaderError       -- ^ Error raised on incorrect header signature.
    | FieldsError       -- ^ Error raised on empty list of field names.
    | MismatchError     -- ^ Error raised on incorrect number of field values.
    | RowError !e       -- ^ Row validation error.
    deriving stock (Eq, Show, Functor, Foldable, Traversable)


{- | The @Table@ type. -}
newtype Table a = Table (Vector a)
    deriving stock (Eq, Show, Functor, Foldable, Traversable)


{- | Construct a table from a list. -}
makeTable :: [a] -> Table a
makeTable = Table . fromList


{- | Parser for a .tbl table rows. -}
parseRows
    :: (NonEmpty (Text, Text) -> Either e a)        -- ^ Row validator.
    -> NonEmpty Text                                -- ^ Field names.
    -> Get Lines (ParseError Lines (TableError e)) [a]
parseRows v fields = go
    where
        go = do
            s <- get
            first absurd (observe parseFields) >>=
                either
                    (withParseError
                        (throwError Fail)
                        (\ _ _ e -> either (const $ pure []) (const go) e))
                    (\ values ->
                        case zipExact fields values of
                            Nothing -> throwError $ makeParseError s MismatchError
                            Just ps ->
                                case v ps of
                                    Left e  -> throwError $ makeParseError s (RowError e)
                                    Right x -> (x :) <$> go)

{- | Parser for an entire .tbl table. -}
parseTable
    :: (NonEmpty (Text, Text) -> Either e a)        -- ^ Row conversion function.
    -> Get Lines (ParseError Lines (TableError e)) (Table a)
parseTable v = do
    _      <- first (fmap $ const HeaderError) parseHeader
    fields <- first (fmap $ const FieldsError) parseFields
    makeTable <$> parseRows v fields
