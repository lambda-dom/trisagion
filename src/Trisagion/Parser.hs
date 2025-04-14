{- |
Module: Trisagion.Parser

The @Parser@ monad.
-}

module Trisagion.Parser (
    -- * Type operators.
    (:+:),
    (:*:),

    -- * Error types.
    InputError (..),

    -- * Type aliases.
    ParserPE,

    -- * The parsing monad.
    Parser,

    -- ** Basic functions.
    parse,
    eval,
    remainder,

    -- * State parsers.
    get,
    backtrack,

    -- * Error parsers.
    throw,
    catch,

    -- * Primitive parsers @'Streamable' s => 'Parser' s e a@.
    one,

    -- * Primitive parsers @'Splittable' s => 'Parser' s e a@.
    isolate,
    takePrefix,
    takePrefixWith,

    -- * Streams.
    Chunk,

    -- ** Constructors.
    initialize,
) where

-- Imports.
-- Prelude hiding.
import Prelude hiding (null, splitAt)

-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor (..))

-- Package.
import Trisagion.Types.Result (Result (..), toEither, withResult)
import Trisagion.Types.ParseError (ParseError, makeParseError)
import Trisagion.Typeclasses.HasPosition (HasPosition(..))
import Trisagion.Typeclasses.Streamable (Streamable (..))
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf, splitAt, splitWith))


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:

{- | Right-associative type operator version of the @(,)@ tuple type constructor. -}
type a :*: b = (a, b)
infixr 6 :*:


{- | The @InputError@ error type.

Error thrown when a parser requests more input than is available.
-}
data InputError
    -- | Generic failure case.
    = InsufficientInputError

    -- | Failure case when it is possible to determine the amount requested.
    | InputError {-# UNPACK #-} !Word
    deriving stock (Eq, Show)


{- | Type alias to shorten parser type signatures involving 'ParseError'. -}
type ParserPE s e a = Parser s (ParseError (PositionOf s) e) a


{- | The parsing monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor

{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Bifunctor (Parser s) where
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap g f p = Parser $ bimap g f . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @pure x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

  * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Parser s e) where
    pure :: a -> Parser s e a
    pure x = Parser $ \ s -> Success x s

    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = Parser $ \ xs ->
        case run p xs of
            Error e1     -> Error e1
            Success f ys ->
                case run q ys of
                    Error e2     -> Error e2
                    Success x zs -> Success (f x) zs

{- | The 'Monad' instance.

The bind combinator @p >>= f@ first runs @p@ and then the parser obtained by applying the monadic
function @f@ to the result.

note(s):

  * As with @p \<*\> q@, @p >>= f@ short-circuits on @p@ erroring out.
-}
instance Monad (Parser s e) where
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = Parser $ \ xs ->
        case run p xs of
            Error e      -> Error e
            Success x ys -> run (h x) ys

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

The 'Alternative' instance obeys the /left catch/ and /left zero/ laws,

prop> pure x <|> p == pure x
prop> empty >>= f == empty

but /not/ right catch and right zero @f >>= const empty == empty@, because of short-circuiting.

note(s):

  * The parser  @p \<|\> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.
-}
instance Monoid e => Alternative (Parser s e) where
    empty :: Parser s e a
    empty = Parser $ const (Error mempty)

    (<|>) :: Parser s e a -> Parser s e a -> Parser s e a
    (<|>) p q = Parser $ \ s ->
        case run p s of
            r@(Success _ _) -> r
            Error e        ->
                case run q s of
                    r'@(Success _ _) -> r'
                    Error e'        -> Error $ e <> e'

{- | The 'MonadError' instance.

The typeclass provides error handling for the t'Parser' monad. The @'throwError' e@ parser fails
unconditionally with @e@. The parser @'catchError' p h@ first tries @p@. If it succeeds, it returns
the parsed result, if it fails, it backtracks and runs the parser @h e@ where @e@ is the error
returned by @p@.

The difference between 'MonadError' and 'Alternative' regarding errors, is analogous to the
difference between 'Monad' and 'Applicative'. Just as with the former, 'MonadError' allows the
continuation to depend on the specific error that was thrown.

note(s):

  * The monad analogy is not precise, because even if we make the obvious generalization of
  @'catchError'@ to a type-changing version (see 'catch'), it does not satisfy associativity.
-}
instance MonadError e (Parser s e) where
    throwError :: e -> Parser s e a
    throwError = fmap absurd . throw

    catchError :: Parser s e a -> (e -> Parser s e a) -> Parser s e a
    catchError p h = Parser $ \ s ->
        -- Case statement instead of 'catch' to make use of sharing in the Success branch.
        case run p s of
            r@(Success _ _) -> r
            Error e         -> run (h e) s


{- | Run the parser on the input and return the results. -}
run :: Parser s e a -> s -> Result s e a
run (Parser p) = p

{- | Parse the input and return the results. -}
parse :: Parser s e a -> s -> e :+: (a :*: s)
parse p = toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
eval :: Parser s e a -> s -> e :+: a
eval p = withResult Left (\ x _ -> Right x) . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
remainder :: Parser s e a -> s -> e :+: s
remainder p =  withResult Left (\ _ xs -> Right xs). run p


{- | The @'get'@ parser allows probing the t'Parser' state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The 'get' parser satisfies the get-get law of lenses in the form:

prop> get *> get == get

The parser does not throw an error or consume input.
-}
get :: Parser s Void s
get = Parser $ \ s -> Success s s


{- | Run the parser and return the result as a 'Right'; on error, backtrack and return it as a 'Left'. -}
backtrack :: Parser s e a -> Parser s Void (e :+: a)
backtrack p = Parser $ \ xs ->
    case run p xs of
        Error e      -> Success (Left e) xs
        Success x ys -> Success (Right x) ys


{- | The parser @'throw' e@ unconditionally errors with @e@. -}
throw :: e -> Parser s e Void
throw e = Parser $ \ _ -> Error e

{- | Type-changing version of 'catchError'. -}
catch
    :: Parser s d a                     -- ^ Parser to try.
    -> (d -> Parser s e a)              -- ^ Error handler.
    -> Parser s e a
catch p h = Parser $ \ xs ->
    case run p xs of
        Error e      -> run (h e) xs
        Success x ys -> Success x ys


{- | Parse one @'ElementOf' s@ from the streamable. -}
one :: (Streamable s, HasPosition s) => ParserPE s InputError (ElementOf s)
one = Parser $ \ s ->
    case uncons s of
        Nothing      -> Error $ makeParseError s (InputError 1)
        Just (x, xs) -> Success x xs


{- | Run a parser isolated to a prefix of the stream.

Any unconsumed input in the prefix is silently discarded. If such behavior is undesirable, guard the
parser to run with an appropriate check -- see 'Trisagion.Parsers.ParseError.guardWith'.
-}
isolate
    :: (HasPosition s)
    => (s -> Maybe (s, s))              -- ^ Stream splitter. @'Nothing'@ means insufficient input.
    -> Parser s e a                     -- ^ Parser to run.
    -> ParserPE s (InputError :+: e) a
isolate h p = Parser $ \xs ->
    case h xs of
        Nothing               -> Error $ makeParseError xs (Left InsufficientInputError)
        Just (prefix, suffix) ->
            case eval p prefix of
                Left e -> Error $ makeParseError prefix (Right e)
                Right x -> Success x suffix

{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
takePrefix :: Splittable s => Word -> Parser s Void (PrefixOf s)
takePrefix n = Parser $ \ xs ->
    let
        (prefix, suffix) = splitAt n xs
    in
        Success prefix suffix

{- | Parse the longest prefix whose elements satisfy a predicate. -}
takePrefixWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void (PrefixOf s)
takePrefixWith p = Parser $ \ xs ->
    let
        (prefix, suffix) = splitWith p xs
    in
        Success prefix suffix


{- | Chunk a streamable with a parser. -}
data Chunk s e a where
    Chunk :: !Word -> !s -> !(Parser s e a) -> Chunk s e a

-- Instances.
instance MonoFunctor (Chunk s e a) where
    {- | The type of the elemnts of a chunked stream. -}
    type ElementOf (Chunk s e a) = a

    {- | Monomorphic map over a chunked stream. -}
    monomap :: (a -> a) -> Chunk s e a -> Chunk s e a
    monomap f (Chunk n stream parser) = Chunk n stream (f <$> parser)

instance HasPosition (Chunk s e a) where
    type PositionOf (Chunk s e a) = Word

    position :: Chunk s e a -> Word
    position (Chunk n _ _) = n

instance Streamable (Chunk s e a) where
    uncons ::Chunk s e a -> Maybe (a, Chunk s e a)
    uncons (Chunk n xs p) =
        let r = run p xs in
        case r of
            Error _      -> Nothing
            Success x ys -> Just (x, Chunk (succ n) ys p)


{- | Initialize a 'Chunk'-ing stream. -}
initialize :: s -> Parser s e a -> Chunk s e a
initialize = Chunk 0
