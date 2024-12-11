{- |
Module: Trisagion.Examples.Table.Parsers

Parsers for the .tbl tabular format.
-}

module Trisagion.Examples.Table.Parsers (
    -- * Streams.
    Lines,

    -- ** Constructors.
    initLines,

    -- * Error types.
    MismatchError (..),

    -- * Parsers.
    parseHeader,
    parseComment,
    parseFieldOrComment,
    parseLine,
    parseFields,
    parseRow,
    parseRows,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..))
import Data.Text (Text)
import Data.Text qualified as Text (pack, lines, strip, null)

-- Package.
import Trisagion.Lib.NonEmpty (zipExact)
import Trisagion.Types.ParseError (ParseError (..), withParseError, makeParseError)
import Trisagion.Streams.Streamable (Stream, initialize)
import Trisagion.Get (Get, eval)
import Trisagion.Getters.Combinators (validate, observe)
import Trisagion.Getters.Combinators qualified as Getters (maybe)
import Trisagion.Getters.Streamable (InputError, MatchError, matchElem, one)
import Trisagion.Getters.Splittable (remainder)
import Trisagion.Getters.Char (notSpaces, spaces)


{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @MismatchError@ error type, raised on a line with incorrect number of fields. -}
data MismatchError = MismatchError
    deriving stock (Eq, Show)


{- | Parser for the header of a .tbl table. -}
parseHeader :: Get Lines (ParseError Lines (Either InputError (MatchError Text))) Text
parseHeader = matchElem (Text.pack "tbl-v1.0")

{- | Parser for a comment in a .tbl row. -}
parseComment :: Get Text (ParseError Text (Either InputError (MatchError Char))) Text
parseComment = matchElem '#' *> first absurd remainder

{- | Parser for a either a comment or a field in a .tbl row. -}
parseFieldOrComment :: Get Text Void (Either Text Text)
parseFieldOrComment = Getters.maybe parseComment >>= maybe (Right <$> notSpaces) (pure . Left)

{- | Parse one line stripped of whitespace on both ends with a 'Text' parser.

note(s):

    * any unconsumed input by the 'Text' parser is silently discarded.
-}
parseLineWith
    :: Get Text e a
    -> Get Lines (ParseError Lines (Either InputError e)) a
parseLineWith p = validate (eval p) (Text.strip <$> one)

{- | Parser for a .tbl table row. -}
parseLine :: Get Lines (ParseError Lines InputError) [Text]
parseLine = first (fmap (either id absurd)) $ parseLineWith go
    where
        go :: Get Text Void [Text]
        go =
            spaces *> parseFieldOrComment >>=
                either
                    (const $ pure [])
                    (\ field -> if Text.null field then pure [] else (field :) <$> go)

{- | Parse one line as a 'NonEmpty' of field values. -}
parseFields :: Get Lines (ParseError Lines (Either InputError MismatchError)) (NonEmpty Text)
parseFields = validate (maybe (Left MismatchError) Right . nonEmpty) parseLine

{- | Parse one row of field values using a validating function. -}
parseRow
    :: NonEmpty Text
    -> (NonEmpty (Text, Text) -> Either e a)
    -> Get Lines (ParseError Lines (Either InputError MismatchError)) a
parseRow = undefined

{- | Parser for a list of .tbl table rows. -}
parseRows :: NonEmpty Text -> Get Lines (ParseError Lines MismatchError) [NonEmpty (Text, Text)]
parseRows fields = go
    where
        go = do
            s <- get
            first absurd (observe parseFields) >>=
                either
                    (withParseError
                        (throwError Fail)
                        (\ _ _ e -> either (const $ pure []) (const go) e))
                    (\ values -> case zipExact fields values of
                        Nothing -> throwError $ makeParseError s MismatchError
                        Just ps -> (ps :) <$> go)
