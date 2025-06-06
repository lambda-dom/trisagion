{- |
Module: Trisagion.Parser

The @Parser@ monad.
-}

module Trisagion.Parser (
    -- * Type operators.
    (:+:),

    -- * Type aliases.
    InputError,

    -- * The parsing monad.
    Parser,

    -- ** Basic functions.
    parse,
    eval,
    remainder,

    -- * State parsers.
    try,
    lookAhead,

    -- * Error parsers.
    throw,
    catch,

    -- * Parsers @'Streamable' s => 'Parser' s e a@.
    eoi,
    ensureEOI,
    one,
    skipOne,
    peek,

    -- * Parsers @'Splittable' s => 'Parser' s e a@.
    -- $splittable-parsers
    takePrefix,
    takeExact,
    skipPrefix,
    takeWith,
    skipWith,
    takeRemainder,
    consumed,
    isolate,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Optics.Core ((%), review, view)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (MonoFunctor (..))
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))

-- Package.
import Trisagion.Typeclasses.Streamable (Streamable (..))
import qualified Trisagion.Typeclasses.Streamable as Streamable (null)
import Trisagion.Typeclasses.HasOffset (HasOffset (..))
import Trisagion.Typeclasses.Splittable (Splittable (..))
import Trisagion.Types.Result (Result (..), result)
import Trisagion.Types.ErrorItem (endOfInput)
import Trisagion.Types.ParseError (ParseError, singleton)


-- $setup
-- >>> import Trisagion.Streams.Counter


{- | Right-associative type operator version of the 'Either' type constructor. -}
type (:+:) = Either
infixr 6 :+:


{- | Type alias to make signatures of parsers that only fail on insufficient input clearer. -}
type InputError = ParseError Void


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

and, assuming @f@ is a /monoid morphism/, the 'Alternative' structure,

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
'Alternative' instance also satisfies /left distributivity/:

prop> f <*> (x <|> y) == (f <*> x) <|> (f <*> y)

note(s):

  * Right distributivity is violated even with an idempotent monoid. See the
  [Parser tests module](../../tests/Tests/ParserSpec.hs) for an example.
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

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'Parser' state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The 'get' parser does not throw an error or consume input.

The 'put' parser allows changing the t'Parser' state and is the way, the /only/ way to construct
/non-normal/ parsers.
-}
instance MonadState s (Parser s e) where
    {-# INLINE get #-}
    get :: Parser s e s
    get = Parser $ \ s -> Success s s

    {-# INLINE put #-}
    put :: s -> Parser s e ()
    put s = Parser $ \ _ -> Success () s


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
    throwError = throw

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
parse p = view result . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Parser s e a -> s -> e :+: a
eval p = fmap fst . view result . run p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Parser s e a -> s -> e :+: s
remainder p =  fmap snd . view result . run p


{- | Parser implementing backtracking.

The parser @'try' p@ runs @p@ and returns the result as a 'Right'; on @p@ throwing an error, it
backtracks and returns the error as a 'Left'.

=== __Examples:__

>>> parse (try one) "0123"
Right (Right '0',"123")

>>> parse (try one) ""
Right (Left (Cons (EndOfInput 1) []),"")
-}
{-# INLINE try #-}
try :: Parser s e a -> Parser s Void (e :+: a)
try p = do
    r <- gets (parse p)
    case r of
        Left e        -> pure (Left e)
        Right (x, xs) -> put xs $> pure x

{- | Run the parser and return the result, but do not consume any input.

=== __Examples:__

>>> parse (lookAhead one) "0123"
Right (Right '0',"0123")

>>> parse (lookAhead $ matchOne '1') (initialize "0123")
Right (Left (Cons (ErrorItem 1 (ValidationError '0')) []),Counter 0 "0123")

>>> parse (lookAhead one) ""
Right (Left (Cons (EndOfInput 1) []),"")
-}
{-# INLINE lookAhead #-}
lookAhead :: Parser s e a -> Parser s Void (e :+: a)
lookAhead p = gets (eval p)


{- | The parser @'throw' e@ unconditionally errors with @e@. -}
{-# INLINE throw #-}
throw :: e -> Parser s e a
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

{- | Return @'True'@ if all input is consumed.

=== __Examples:__

>>> parse eoi "0123"
Right (False,"0123")

>>> parse eoi ""
Right (True,"")
-}
{-# INLINE eoi #-}
eoi :: Streamable s => Parser s Void Bool
eoi = gets Streamable.null

{- | Run parser @p@ and if not all input is consumed, error out.

=== __Examples:__

>>> parse (ensureEOI () (one *> one)) "01"
Right ('1',"")

>>> parse (ensureEOI () one) "01"
Left (Left ())
-}
{-# INLINE ensureEOI #-}
ensureEOI :: Streamable s => d -> Parser s e a -> Parser s (d :+: e) a
ensureEOI err p = do
    x <- first Right p
    b <- first absurd eoi
    if b
        then pure x
        else throw $ Left err

{- | Parse one @'ElementOf' s@ from the input stream.

=== __Examples:__

>>> parse one "0123"
Right ('0',"123")

>>> parse one ""
Left (Cons (EndOfInput 1) [])
-}

{-# INLINE one #-}
one :: Streamable s => Parser s InputError (ElementOf s)
one = Parser $ \ s ->
    case uncons s of
        Nothing      -> Error $ review (singleton % endOfInput) 1
        Just (x, xs) -> Success x xs

{- | Skip one @'ElementOf' s@ from the input stream.

=== __Examples:__

>>> parse skipOne "0123"
Right ((),"123")

>>> parse skipOne ""
Right ((),"")
 -}
{-# INLINE skipOne #-}
skipOne :: Streamable s => Parser s Void ()
skipOne = Parser $ \ s -> Success () (dropOne s)

{- | Extract the first @'ElementOf' s@ from the streamable but without consuming input.

=== __Examples:__

>>> parse peek "0123"
Right (Just '0',"0123")

>>> parse peek ""
Right (Nothing,"")
-}
{-# INLINE peek #-}
peek :: Streamable s => Parser s Void (Maybe (ElementOf s))
peek = do
    c <- gets uncons
    pure $ fmap fst c


{- $splittable-parsers
Implementations requiring the computation of the length of a prefix have a @'MonoFoldable' s@
constraint. This can be important for performance because for a `Splittable` like @Text@ this is
@O(n)@, while for other types like lazy bytestrings, it forces the entirety of the value into
memory which is probably not what is desired.
-}

{- | Parse a fixed size prefix.

The parser does not error and it is guaranteed that the prefix has length equal or less than @n@.

=== __Examples:__

>>> parse (takePrefix 2) "0123"
Right ("01","23")

>>> parse (takePrefix 10) "0123"
Right ("0123","")
-}
{-# INLINE takePrefix #-}
takePrefix :: Splittable s => Word -> Parser s Void (PrefixOf s)
takePrefix n = Parser $ \ xs -> uncurry Success (splitPrefix n xs)

{- | Parse an exact, fixed size prefix.

note(s):

    * Implementation requires computing the length of the prefix.

=== __Examples:__

>>> parse (takeExact 2) "0123"
Right ("01","23")

>>> parse (takeExact 10) "0123"
Left (Cons (EndOfInput 10) [])
-}
{-# INLINE takeExact #-}
takeExact
    :: (Splittable s, MonoFoldable (PrefixOf s))
    => Word -> Parser s InputError (PrefixOf s)
takeExact n = do
    prefix <- first absurd $ takePrefix n
    if monolength prefix /= n
        then throw $ review (singleton % endOfInput) n
        else pure prefix

{- | Drop a fixed size prefix from the stream.

=== __Examples:__

>>> parse (skipPrefix 2) "0123"
Right ((),"23")

>>> parse (skipPrefix 10) "0123"
Right ((),"")
-}
{-# INLINE skipPrefix #-}
skipPrefix :: Splittable s => Word -> Parser s Void ()
skipPrefix n = Parser $ \ xs -> Success () (dropPrefix n xs)

{- | Parse the longest prefix whose elements satisfy a predicate.

=== __Examples:__

>>> parse (takeWith ('3' /=)) "0123"
Right ("012","3")

>>> parse (takeWith ('3' /=)) ""
Right ("","")
-}
{-# INLINE takeWith #-}
takeWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void (PrefixOf s)
takeWith p = Parser $ \ xs -> uncurry Success (splitWith p xs)

{- | Drop the longest prefix whose elements satisfy a predicate.

=== __Examples:__

>>> parse (skipWith ('3' /=)) "0123"
Right ((),"3")

>>> parse (skipWith ('3' /=)) ""
Right ((),"")
-}
{-# INLINE skipWith #-}
skipWith :: Splittable s => (ElementOf s -> Bool) -> Parser s Void ()
skipWith p = Parser $ \ xs -> Success () (dropWith p xs)

{- | Parse the remainder of the stream as a prefix. -}
{-# INLINE takeRemainder #-}
takeRemainder :: Splittable s => Parser s Void (PrefixOf s)
takeRemainder = Parser $ \ xs -> uncurry Success (splitRemainder xs)

{- | Run the parser and return its result along with the prefix of consumed input.

note(s):

  * Implementation implicitly relies on normality of @p@.

=== __Examples:__

>>> parse (consumed one) (initialize "0123")
Right (("0",'0'),Counter 1 "123")

>>> parse (consumed one) (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE consumed #-}
consumed :: (HasOffset s, Splittable s) => Parser s e a -> Parser s e (PrefixOf s, a)
consumed p = do
    xs <- get
    x  <- p
    n  <- gets offset
    -- Implicitly relies on the parser @p@ being normal, for positivity of @n - offset xs@.
    pure (fst $ splitPrefix (n - offset xs) xs, x)

{- | Run a parser isolated to a fixed size prefix of the stream.

The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
in the stream. Any unconsumed input in the prefix is silently discarded. If such behavior is
undesirable, 'Control.Monad.guard' the parser to run with an appropriate check -- see
'Trisagion.Parser.ensureEOI'.

=== __Examples:__

>>> parse (isolate 2 one) "0123"
Right ('0',"23")

>>> parse (isolate 2 one) "0"
Right ('0',"")

>>> parse (isolate 2 one) ""
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE isolate #-}
isolate
    :: Splittable s
    => Word                             -- ^ Prefix size.
    -> Parser (PrefixOf s) e a          -- ^ Parser to run.
    -> Parser s e a
isolate n p = do
    prefix <- first absurd $ takePrefix n
    case eval p prefix of
        Left e  -> throw e
        Right x -> pure x
