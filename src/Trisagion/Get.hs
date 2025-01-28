{- |
Module: Trisagion.Get

The @Get@ parsing monad.
-}

module Trisagion.Get (
    -- * The parsing monad.
    Get,

    -- ** Basic functions.
    embed,
    run,
    eval,
    exec,

    -- * Parser combinators.
    -- ** Error parsers.
    handleError,

    -- ** Parsers without errors.
    observe,
    lookAhead,
    maybe,

    -- ** Parsers without values.
    skip,

    -- ** Applicative parsers.
    zip,
    zipWith,
    before,
    after,
    between,

    -- ** Alternative parsers.
    either,
    choose,

    -- ** List parsers.
    sequence,
    repeat,
    unfold,
    many,
    some,
    atMostN,
    untilEnd,
    sepBy,
    sepBy1,
) where

-- Imports.
-- Prelude hiding.
import Prelude qualified as Base (either)
import Prelude hiding (either, maybe, repeat, sequence, zipWith, zip)

-- Base.
import Control.Applicative (Alternative ((<|>), empty), asum)
import Control.Monad (replicateM)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..), (<|), toList)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))

-- Package.
import Trisagion.Types.Result (Result (..), withResult)


{- | The @Get@ parsing monad. -}
newtype Get s e a = Get (s -> Result s e a)


-- Instances.
instance Functor (Get s e) where
    {-# INLINE fmap #-}
    fmap :: (a -> b) -> Get s e a -> Get s e b
    fmap f p = embed $ fmap f . run p

{- | The 'Bifunctor' instance, providing functoriality in the error type. -}
instance Bifunctor (Get s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Get s d a -> Get s e b
    bimap g f p = embed $ bimap g f . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @pure x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

    * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Get s e) where
    {-# INLINE pure #-}
    pure :: a -> Get s e a
    pure x = embed $ \ s -> Success x s

    {-# INLINE (<*>) #-}
    (<*>) :: Get s e (a -> b) -> Get s e a -> Get s e b
    (<*>) p q = embed $ \ s ->
        case run p s of
            Error e     -> Error e
            Success f t ->
                case run q t of
                    Error e'    -> Error e'
                    Success x u -> Success (f x) u

{- | The 'Monad' instance.

The bind combinator @p >>= f@ first runs @p@ and then the parser obtained by applying the monadic
function @f@ to the result.

note(s):

    * As with @p \<*\> q@, @p >>= f@ short-circuits on @p@ erroring out.
-}
instance Monad (Get s e) where
    {-# INLINE (>>=) #-}
    (>>=) :: Get s e a -> (a -> Get s e b) -> Get s e b
    (>>=) p h = embed $ \ s ->
        case run p s of
            Error e     -> Error e
            Success x t -> run (h x) t

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

note(s):

    * The parser  @p \<|> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.
-}
instance Monoid e => Alternative (Get s e) where
    {-# INLINE empty #-}
    empty :: Get s e a
    empty = embed $ const (Error mempty)

    {-# INLINE (<|>) #-}
    (<|>) :: Get s e a -> Get s e a -> Get s e a
    (<|>) p q = embed $ \ s ->
        case run p s of
            r@(Success _ _) -> r
            Error e         ->
                case run q s of
                    r'@(Success _ _) -> r'
                    Error e'         -> Error $ e <> e'

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'Get' parser state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The @'put'@ parser allows arbitrary state modifications. Provides mono-functoriality to @'Get' s e a@
in @s@ via @'Control.MonadState.modify'@.
-}
instance MonadState s (Get s e) where
    {-# INLINE get #-}
    get :: Get s e s
    get = embed $ \ s -> Success s s

    {-# INLINE put #-}
    put :: s -> Get s e ()
    put s = embed $ const (Success () s)

{- | The 'MonadError' instance.

The typeclass provides error handling for the t'Get' monad. The @'throwError' e@ parser fails
unconditionally with @e@. The parser @'catchError' p h@ first tries @p@. If it succeeds, it returns
the parsed result, if it fails, it backtracks and runs the parser @h e@ where @e@ is the error
returned by @p@.

The difference between 'MonadError' and 'Alternative' regarding errors, is analogous to the
difference between 'Monad' and 'Applicative'. Just as with the former, 'MonadError' allows the
continuation to depend on the specific error that was thrown.

note(s):

    * The monad analogy is not precise, because even if we make the obvious generalization of
    @'catchError'@ to a type-changing version (see 'handleError'), it does not satisfy the
    associativity law.
-}
instance MonadError e (Get s e) where
    {-# INLINE throwError #-}
    throwError :: e -> Get s e a
    throwError e = embed $ const (Error e)

    {-# INLINE catchError #-}
    catchError :: Get s e a -> (e -> Get s e a) -> Get s e a
    catchError = handleError


{- | Embed a parsing function in the t'Get' monad. -}
{-# INLINE embed #-}
embed :: (s -> Result s e a) -> Get s e a
embed = Get 

{- | Run the parser on the input and return the parsed result. -}
{-# INLINE run #-}
run :: Get s e a -> s -> Result s e a
run (Get f) = f

{- | Evaluate the parser on the input and return the result, discarding the state. -}
{-# INLINE eval #-}
eval :: Get s e a -> s -> Either e a
eval p = withResult Left (\ _ x -> Right x) . run p

{- | Run the parser on the input and return the updated state, discarding the value. -}
{-# INLINE exec #-}
exec :: Get s e a -> s -> Either e s
exec p = withResult Left (\ s _ -> Right s) . run p


{- | Type-changing version of 'catchError'. -}
{-# INLINE handleError #-}
handleError
    :: Get s e a            -- ^ Parser to try.
    -> (e -> Get s d a)     -- ^ Error handler.
    -> Get s d a
handleError p h = embed $ \ s ->
        case run p s of
            Success x t -> Success x t
            Error e     -> run (h e) s

{- | Run the parser and return the result as a @Right@; if it errors, backtrack and return the error as a @Left@. -}
{-# INLINE observe #-}
observe :: Get s e a -> Get s Void (Either e a)
observe p = do
    s <- get
    handleError
        (Right <$> p)
        (\ e -> put s $> Left e)

{- | Run the parser and return the result, but do not consume any input. -}
{-# INLINE lookAhead #-}
lookAhead :: Get s e a -> Get s Void (Either e a)
lookAhead p = eval p <$> first absurd get

{- | Run the parser and return the result as a @'Just'@. If it errors, backtrack and return @'Nothing'@.

The difference with @'Control.Applicative.optional'@ is the more precise type signature.
-}
{-# INLINE maybe #-}
maybe :: Get s e a -> Get s Void (Maybe a)
maybe p = Base.either (const Nothing) Just <$> observe p

{- | Run the parser but discard the result.

note(s):

    * Various combinators have more efficient @skip*@ versions that avoid constructing intermediate
    values; use those whenever possible.
-}
{-# INLINE skip #-}
skip :: Get s e a -> Get s e ()
skip = (() <$)

{- | Sequence two parsers and zip the results in a pair. -}
{-# INLINE zip #-}
zip :: Get s e a -> Get s e b -> Get s e (a, b)
zip = zipWith (,)

{- | Sequence two parsers and zip the results with a binary function. -}
{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> Get s e a -> Get s e b -> Get s e c
zipWith f p q = f <$> p <*> q

{- | The parser @'before' b p@ parses @b@ and @p@ in succession, returning the result of @p@. -}
{-# INLINE before #-}
before
    :: Get s e b                -- ^ Parser to run first.
    -> Get s e a                -- ^ Parser to run second.
    -> Get s e a
before b p = b *> p

{- | The parser @'after' a p@ parses @p@ and @a@ in succession, returning the result of @p@. -}
{-# INLINE after #-}
after
    :: Get s e b                -- ^ Parser to run after.
    -> Get s e a                -- ^ Parser to run first.
    -> Get s e a
after a p = p <* a

{- | The parser @'between' o c p@ parses @o@, @p@ and @c@ in succession, returning the result of @p@. -}
{-# INLINE between #-}
between
    :: Get s e b                -- ^ Opening parser.
    -> Get s e c                -- ^ Closing parser.
    -> Get s e a                -- ^ Parser to run in-between.
    -> Get s e a
between open close = before open . after close

{- | Run the first parser and if it fails run the second. Return the result as an @'Either'@.

note(s):

    * The parser is @'Left'@-biased; if the first parser is successful the second never runs.
-}
{-# INLINE either #-}
either :: Monoid e => Get s e a -> Get s e b -> Get s e (Either a b)
either q p = (Left <$> q) <|> (Right <$> p)

{- | Run the parsers in succession, returning the result of the first successful one. -}
{-# INLINE choose #-}
choose :: (Foldable t, Monoid e) => t (Get s e a) -> Get s e a
choose = asum

{- | Sequence a traversable of parsers and return the traversable of results. -}
{-# INLINE sequence #-}
sequence :: Traversable t => t (Get s e a) -> Get s e (t a)
sequence = sequenceA

{- | Run the parser @n@ times and return the list of results.

It is guaranteed that the list of results has exactly @n@ elements.
-}
{-# INLINE repeat #-}
repeat :: Word -> Get s e a -> Get s e [a]
repeat = replicateM . fromIntegral

{- | Lift @'List.unfoldr'@ over the @t'Get'@ monad. -}
unfold :: (r -> Get s e (a, r)) -> r -> Get s Void [a]
unfold h = go
    where
        go s = do
            r <- maybe (h s)
            case r of
                Nothing     -> pure []
                Just (x, t) -> (x : ) <$> go t

{- | Run the parser zero or more times until it fails, returning the list of results.

The difference with @'Control.Applicative.many'@ from 'Alternative' is the more precise type signature.

note(s):

    * The @'many' p@ parser can loop forever if fed a parser @p@ that does not fail and that
    may not consume input, e.g. any parser with @'Void'@ in the error type or their polymorphic
    variants, like @'pure' x@, @'Control.Applicative.many' p@, etc.
-}
many :: Get s e a -> Get s Void [a]
many p = go
    where
        go = do
            r <- maybe p
            case r of
                Nothing -> pure []
                Just x  -> (x :) <$> go

{- | Run the parser one or more times and return the results as a @'NonEmpty'@.

The difference with @'Control.Applicative.some'@ from 'Alternative' is the more precise type signature.
-}
{-# INLINE some #-}
some :: Get s e a -> Get s e (NonEmpty a)
some p = zipWith (:|) p (first absurd $ many p)

{- | Run the parser @n@ times or until it errors and return the list of results.

The parser does not error and it is guaranteed that the list of results has @n@ or less elements.
-}
atMostN :: Word -> Get s e a -> Get s Void [a]
atMostN n p = go n
    where
        go 0 = pure []
        go i = do
            r <- maybe p
            case r of
                Nothing -> pure []
                Just x  -> (x :) <$> go (pred i)

{- | The parser @'untilEnd' end p@ runs @p@ zero or more times until @end@ succeeds, returning the results of @p@ and @end@. -}
untilEnd :: Monoid e => Get s e a -> Get s e a -> Get s e (NonEmpty a)
untilEnd end p = go
    where
        go = do
            r <- either end p
            case r of
                Left e -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy #-}
sepBy :: Get s e a -> Get s e b -> Get s Void [b]
sepBy sep p = fromMaybe [] <$> maybe (toList <$> sepBy1 sep p)

{- | The parser @'sepBy' sep p@ parses one or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy1 #-}
sepBy1 :: Get s e a -> Get s e b -> Get s e (NonEmpty b)
sepBy1 sep p = zipWith (:|) p (first absurd $ many (sep *> p))
