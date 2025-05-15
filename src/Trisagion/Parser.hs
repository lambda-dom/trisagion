{- |
Module: Trisagion.Parser

The @Parser@ monad.
-}

module Trisagion.Parser (
    -- * Type operators.
    (:+:),

    -- * The parsing monad.
    Parser,

    -- ** Basic functions.
    parse,
    eval,
    remainder,

    -- * State parsers.
    get,
    try,
    isolate,

    -- * Error parsers.
    throw,
    catch,

    -- * Primitive parsers @'Streamable' s => 'Parser' s e a@.
    one,
    skipOne,

    -- * Primitive parsers @'Splittable' s => 'Parser' s e a@.
    takePrefix,
    skipPrefix,
    takeWith,
    skipWith,
    takeRemainder,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Optics.Core ((%), review)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import qualified Trisagion.Typeclasses.Streamable as Streamable (tail)
import Trisagion.Typeclasses.Splittable (Splittable (..), dropPrefix, dropWith)
import Trisagion.Types.Result (Result (..), toEither, withResult)
import Trisagion.Types.ParseError (ParseError, singleton)
import Trisagion.Types.ErrorItem (endOfInput)


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:


{- | The parsing monad. -}
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor

{- | The 'Bifunctor' instance, providing functoriality in the error type.

For a function @f :: d -> e@, @'first' f@ preserves all the structure in sight. Specifically,
@'first' f@ preserves the 'Applicative' structure,

prop> first f . pure == pure
prop> (first f p) <*> (first f q) == first f (p <*> q)

the 'Monad' structure,

prop> (first f p) >>= (first f .  h) == first f (p >>= h)

and the 'Alternative' structure,

prop> first f empty == empty
prop> (first f p) <|> (first f q) == first f (p <|> q)
-}
instance Bifunctor (Parser s) where
    {-# INLINE bimap #-}
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
    {-# INLINE pure #-}
    pure :: a -> Parser s e a
    pure x = Parser $ \ s -> Success x s

    {-# INLINE (<*>) #-}
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
    {-# INLINE (>>=) #-}
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = Parser $ \ xs ->
        case run p xs of
            Error e      -> Error e
            Success x ys -> run (h x) ys

{- | The 'Alternative' instance.

The @'empty'@ parser fails unconditionally with the monoid unit for @e@. The parser  @p \<|\> q@
represents choice. First run @p@ and if successful return the result. If it throws an error,
backtrack and run @q@ on the same input.

note(s):

  * The parser  @p \<|\> q@ is first, or left, biased; if @p@ succeeds, @q@ never runs.

The 'Alternative' instance obeys the /left catch/, /left absorption/ and /left zero/ laws,

prop> pure x <|> p == pure x
prop> empty <*> p == empty
prop> empty >>= h == empty

but /not/ their right-sided versions because of short-circuiting.

Furthermore, if the monoid @e@ is /idempotent/, that is, for all @x :: e@, @x <> x == x@, then the
'Alternative' structure also satisfies /left distributivity/:

prop> f <*> (x <|> y) == (f <*> x) <|> (f <*> y)

note(s):

  * Right distributivity is violated even with an idempotent monoid. See the tests for an example.
-}
instance Monoid e => Alternative (Parser s e) where
    {-# INLINE empty #-}
    empty :: Parser s e a
    empty = Parser $ const (Error mempty)

    {-# INLINE (<|>) #-}
    (<|>) :: Parser s e a -> Parser s e a -> Parser s e a
    (<|>) p q = Parser $ \ s ->
        case run p s of
            r@(Success _ _) -> r
            Error e         ->
                case run q s of
                    r'@(Success _ _) -> r'
                    Error e'         -> Error $ e <> e'

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
    {-# INLINE throwError #-}
    throwError :: e -> Parser s e a
    throwError = fmap absurd . throw

    {-# INLINE catchError #-}
    catchError :: Parser s e a -> (e -> Parser s e a) -> Parser s e a
    catchError p h = Parser $ \ s ->
        -- Case statement instead of 'catch' to make use of sharing in the Success branch.
        case run p s of
            r@(Success _ _) -> r
            Error e         -> run (h e) s


{- | Run the parser on the input and return the results. -}
{-# INLINE run #-}
run :: Parser s e a -> s -> Result s e a
run (Parser p) = p

{- | Parse the input and return the results. -}
{-# INLINE parse #-}
parse :: Parser s e a -> s -> e :+: (a, s)
parse p = toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Parser s e a -> s -> e :+: a
eval p = withResult Left (\ x _ -> Right x) . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Parser s e a -> s -> e :+: s
remainder p =  withResult Left (\ _ xs -> Right xs) . run p


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
{-# INLINE get #-}
get :: Parser s Void s
get = Parser $ \ s -> Success s s

{- | Parser implementing backtracking.

The parser @'try' p@ runs @p@ and returns the result as a 'Right'; on @p@ throwing an error, it
backtracks and returns the error as a 'Left'.
-}
{-# INLINE try #-}
try :: Parser s e a -> Parser s Void (e :+: a)
try p = Parser $ \ xs ->
    case run p xs of
        Error e      -> Success (Left e) xs
        Success x ys -> Success (Right x) ys

{- | Run a parser isolated to a prefix of the stream.

Any unconsumed input in the prefix is silently discarded. If such behavior is undesirable,
'Control.Monad.guard' the parser to run with an appropriate check -- see
'Trisagion.Parsers.Streamable.ensureEOI'.
-}
{-# INLINE isolate #-}
isolate
    :: (s -> d :+: (s, s))              -- ^ Stream splitter with @'Left' d@ signalling an error.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s (d :+: e) a
isolate h p = Parser $ \xs ->
    case h xs of
        Left d                 -> Error $ Left d
        Right (prefix, suffix) ->
            case eval p prefix of
                Left e  -> Error $ Right e
                Right x -> Success x suffix


{- | The parser @'throw' e@ unconditionally errors with @e@. -}
{-# INLINE throw #-}
throw :: e -> Parser s e Void
throw e = Parser $ const (Error e)

{- | Type-changing version of 'catchError'. -}
{-# INLINE catch #-}
catch
    :: Parser s d a                     -- ^ Parser to try.
    -> (d -> Parser s e a)              -- ^ Error handler.
    -> Parser s e a
catch p h = Parser $ \ xs ->
    case run p xs of
        Error e      -> run (h e) xs
        Success x ys -> Success x ys


{- | Parse one @'ElementOf' s@ from the input stream. -}
{-# INLINE one #-}
one :: Streamable s => Parser s (ParseError s Void) (ElementOf s)
one = Parser $ \ s ->
    case uncons s of
        Nothing      -> Error $ review (singleton % endOfInput) 1
        Just (x, xs) -> Success x xs

{- | Skip one @'ElementOf' s@ from the input stream. -}
{-# INLINE skipOne #-}
skipOne :: Streamable s => Parser s Void ()
skipOne = Parser $ \ s -> Success () (fromMaybe s $ Streamable.tail s)

{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.
-}
{-# INLINE takePrefix #-}
takePrefix :: Splittable s => Word -> Parser s Void (PrefixOf s)
takePrefix n = Parser $ \ xs -> uncurry Success (splitPrefix n xs)

{- | Drop a fixed size prefix from the stream. -}
{-# INLINE skipPrefix #-}
skipPrefix :: Splittable s => Word -> Parser s Void ()
skipPrefix n = Parser $ \ xs -> Success () (dropPrefix n xs)

{- | Parse the longest prefix whose elements satisfy a predicate. -}
{-# INLINE takeWith #-}
takeWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void (PrefixOf s)
takeWith p = Parser $ \ xs -> uncurry Success (splitWith p xs)

{- | Drop the longest prefix whose elements satisfy a predicate. -}
{-# INLINE skipWith #-}
skipWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void ()
skipWith p = Parser $ \ xs -> Success () (dropWith p xs)

{- | Parse the remainder of the stream as a prefix. -}
{-# INLINE takeRemainder #-}
takeRemainder :: Splittable s => Parser s Void (PrefixOf s)
takeRemainder = Parser $ \ xs -> uncurry Success (splitRemainder xs)
