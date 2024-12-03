{- |
Module: Trisagion.Examples.Table

Parser for the .tbl tabular format.
-}

module Trisagion.Examples.Table (
    -- * Streams.
    Lines,

    -- ** Constructors.
    initLines,

    -- * Error types.
    TableError,
) where

-- Imports.
-- Libraries.
import Data.Text (Text)
import qualified Data.Text as Text (lines)

-- Package.
import Trisagion.Streams.Streamable (Stream, initialize)


{- | A type alias for @t'Stream' ['Text']@. -}
type Lines = Stream [Text]


{- | Construct a t'Lines' streamable from @'Text.lines' text@. -}
initLines :: Text -> Lines
initLines text = initialize (Text.lines text)


{- | The @TableError@ error type. -}
data TableError
    = HeaderError
    | FieldsError
    deriving stock (Eq, Show)
