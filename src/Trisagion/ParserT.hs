{- |
Module: Trisagion.ParserT

The @ParserT@ monad transformer.
-}

module Trisagion.ParserT (
    -- * The parsing monad transformer.
    ParserT,
    Parser,

    -- * Basic functions.
    embed,
    run,
    parse,
    eval,
    remainder,

    -- * Functoriality.
    mapError,
    hoist,

    -- * Error parsers.
    throw,
    catch,
    try,
    validate,
    lookAhead,

    -- * Parsers with @'HasOffset' m s@ constraints.
    getOffset,

    -- * Parsers throwing t'ParseError'-errors.
    throwParseError,
    capture,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Void (Void)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))

-- Package.
import Trisagion.Types.Either ((:+:))
import Trisagion.Types.Result (Result (..), toEither)
import Trisagion.Types.ParseError (ParseError (..))
import Trisagion.Typeclasses.HasOffset (HasOffset (..))


{- | The parsing monad transformer @ParserT s e m a@.

@m@ is the underlying monad, @s@ is the input stream type, @e@ is the type of parsing errors that
the parser can throw and @a@ is the type of parsed values.
-}
type ParserT ::  Type -> Type -> (Type -> Type) -> Type -> Type
newtype ParserT s e m a = ParserT (s ->  m (Result s e a))
    deriving stock Functor


{- | Type alias for @t'ParserT' s e m@ specialized to @m ~ 'Identity'@. -}
type Parser ::  Type -> Type -> Type -> Type
type Parser s e = ParserT s e Identity


{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @'pure' x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

  * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Monad m => Applicative (ParserT s e m) where
    {-# INLINE pure #-}
    pure :: a -> ParserT s e m a
    pure x = embed $ \ xs -> pure $ Success x xs

    {-# INLINE (<*>) #-}
    (<*>) :: ParserT s e m (a -> b) -> ParserT s e m a -> ParserT s e m b
    (<*>) p q = embed $ \ xs -> do
        r <- run p xs
        case r of
            Error d      -> pure $ Error d
            Success f ys -> do
                s <- run q ys
                case s of
                    Error e      -> pure $ Error e
                    Success x zs -> pure $ Success (f x) zs

{- | The 'Monad' instance.

The bind combinator @p >>= h@ first runs @p@ and then the parser obtained by applying the monadic
function @h@ to the result.

note(s):

  * As with @p \<*\> q@, @p >>= h@ short-circuits on @p@ erroring out.
-}
instance Monad m => Monad (ParserT s e m) where
    {-# INLINE (>>=) #-}
    (>>=) :: ParserT s e m a -> (a -> ParserT s e m b) -> ParserT s e m b
    (>>=) p h = embed $ \ xs -> do
        r <- run p xs
        case r of
            Error e      -> pure $ Error e
            Success x ys -> run (h x) ys

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

note(s):

  * The parser  @p \<|\> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.
  * The default way to get a monoid structure on @e@ is to embed @e@ in @t'ParseError' s e@ -- see
  'throwParseError'.
-}
instance (Monad m, Monoid e) => Alternative (ParserT s e m) where
    {-# INLINE empty #-}
    empty :: ParserT s e m a
    empty = embed . const . pure $ Error mempty

    {-# INLINE (<|>) #-}
    (<|>) :: ParserT s e m a -> ParserT s e m a -> ParserT s e m a
    (<|>) p q = ParserT $ \ xs -> do
        r <- run p xs
        case r of
            x@(Success _ _) -> pure x
            Error e         -> do
                s <- run q xs
                case s of
                    x'@(Success _ _) -> pure x'
                    Error e'         -> pure $ Error (e <> e')

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'ParserT' state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The 'get' parser does not throw an error or consume input while the 'put' parser allows changing
the t'ParserT' state.
-}
instance Monad m => MonadState s (ParserT s e m) where
    {-# INLINE get #-}
    get :: ParserT s e m s
    get = embed $ \ xs -> pure $ Success xs xs

    {-# INLINE put #-}
    put :: s -> ParserT s e m ()
    put xs = embed . const . pure $ Success () xs

{- | The 'MonadError' instance.

The typeclass provides error handling for the t'ParserT' monad. The @'throwError' e@ parser fails
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
instance Monad m => MonadError e (ParserT s e m) where
    {-# INLINE throwError #-}
    throwError :: e -> ParserT s e m a
    throwError = throw

    {-# INLINE catchError #-}
    catchError :: ParserT s e m a -> (e -> ParserT s e m a) -> ParserT s e m a
    catchError p h = embed $ \ s -> do
        r <- run p s
        -- Case statement instead of 'catch' to make use of sharing in the Success branch.
        case r of
            x@(Success _ _) -> pure x
            Error e         -> run (h e) s

{- | Lift a monadic action to the t'ParserT' monad. -}
instance MonadTrans (ParserT s e) where
    {- | The @'lift' h@ parser does not consume input and does not throw an error.-}
    {-# INLINE lift #-}
    lift :: Monad m => m a -> ParserT s e m a
    lift h = embed $ \ xs -> do
        x <- h
        pure $ Success x xs


{- | Embed a parsing function in the t'ParserT' monad. -}
{-# INLINE embed #-}
embed ::  (s ->  m (Result s e a)) -> ParserT s e m a
embed = ParserT

{- | Run the parser on the input and return the results. -}
{-# INLINE run #-}
run :: ParserT s e m a -> s ->  m (Result s e a)
run (ParserT f) = f

{- | Parse the input and return the result as an 'Either'. -}
{-# INLINE parse #-}
parse :: Functor m => ParserT s e m a -> s -> m (e :+: (a, s))
parse p = fmap toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Functor m => ParserT s e m a -> s -> m (e :+: a)
eval p = fmap (fmap fst) . parse p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Functor m => ParserT s e m a -> s -> m (e :+: s)
remainder p = fmap (fmap snd) . parse p


{- | Functoriality in the error type. -}
{-# INLINE mapError #-}
mapError :: Functor m => (d -> e) -> ParserT s d m a -> ParserT s e m a
mapError f p = embed $ fmap (first f) . run p

{- | Functoriality in the monad @m@ type parameter. -}
{-# INLINE hoist #-}
hoist :: (forall b . m b -> n b) -> ParserT s e m a -> ParserT s e n a
hoist f p = embed $ \ s -> f (run p s)


{- | Parser that fails unconditionally with error @e@. -}
{-# INLINE throw #-}
throw :: Applicative m => e -> ParserT s e m a
throw e = embed . const . pure $ Error e

{- | The type-changing version of 'catchError'.

The parser @'catch' p h@ runs @p@ and if it throws an error @e@, backtracks and runs @h e@.
-}
{-# INLINE catch #-}
catch
    :: Monad m
    => ParserT s d m a                  -- ^ Parser to try.
    -> (d -> ParserT s e m a)           -- ^ Error handler.
    -> ParserT s e m a
catch p h = embed $ \ xs -> do
    r <- run p xs
    case r of
        Error e      -> run (h e) xs
        Success x ys -> pure $ Success x ys

{- | Parser implementing backtracking.

The parser @'try' p@ runs @p@ and returns the result as a 'Right'; on @p@ throwing an error, it
backtracks and returns the error as a 'Left'.
-}
{-# INLINE try #-}
try :: Monad m => ParserT s e m a -> ParserT s Void m (e :+: a)
try p = embed $ \ xs -> do
    r <- run p xs
    case r of
        Error e      -> pure $ Success (Left e) xs
        Success x ys -> pure $ Success (Right x) ys

{- | Run the parser and return the result, validating it. -}
{-# INLINE validate #-}
validate
    :: Monad m
    => (a -> d :+: b)                   -- ^ Validator.
    -> ParserT s e m a                  -- ^ Parser to run.
    -> ParserT s (d :+: e) m b
validate v p = do
    x <- mapError Right p
    case v x of
        Left d  -> throw (Left d)
        Right y -> pure y

{- | Run the parser and return the result, but do not consume any input. -}
{-# INLINE lookAhead #-}
lookAhead :: Monad m => ParserT s e m a -> ParserT s Void m (e :+: a)
lookAhead p = get >>= lift . eval p


{- | Parser returning the current stream offset. -}
{-# INLINE getOffset #-}
getOffset :: (Monad m, HasOffset m s) => ParserT s Void m Word
getOffset = get >>= lift . offset


{- | Transform a parser throwing @e@-errors into a parser throwing (@t'ParseError' e@)-errors. -}
{-# INLINE throwParseError #-}
throwParseError
    :: Monad m
    => ParserT s e m a
    -> ParserT s (ParseError s e) m a
throwParseError p = do
    xs <- get
    mapError (ParseError xs) p

{- | Capture the offset of the input stream at the entry point in case of an error.

A parser,

@
parser = do
    ...
    x <- p -- Can throw here.
    ...
@

can now be written as:

@
parser = capture $ do
    -- Capture the input stream @xs@ here.
    ...
    x <- p -- Can throw here. If it throws, the error's input stream will be @xs@.
    ...
@
-}
{-# INLINE capture #-}
capture :: Monad m => ParserT s (ParseError s e) m a -> ParserT s (ParseError s e) m a
capture p = do
        xs  <- get
        mapError (set xs) p
    where
        set :: s -> ParseError s e -> ParseError s e
        set _ Failure          = Failure
        set xs (ParseError _ e) = ParseError xs e
