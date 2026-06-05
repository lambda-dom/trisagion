{- |
Module: Trisagion.Parser

The @Parser@ monad.
-}

module Trisagion.Parser (
    -- * The parsing monad.
    Parser,

    -- * Basic functions.
    embed,
    run,
    parse,
    eval,
    remainder,

    -- * Error parsers.
    catch,
    try,
    validate,
    lookAhead,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Void (Void)

-- Libraries.
import Control.Monad.State (MonadState (..))
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Types.Result (Result (..), toEither)


-- $setup
-- >>> import Data.Bifunctor
-- >>> import Control.Applicative (Alternative (..))
-- >>> import Trisagion.Parsers.Streamable


{- | The parsing monad @Parser s e a@.

@s@ is the input stream type, @e@ is the type of parsing errors that the parser can throw and @a@
is the type of parsed values.

note(s):

    * Two parsers @p@ and @q@ are equal, written as @p == q@, iff their parsing functions are
    extensionally equal, that is, for all @xs@, we have @parse p xs == parse q xs@. Obviously,
    this definition is not effective, so it cannot be turned into an 'Eq' instance.
-}
type Parser ::  Type -> Type -> Type -> Type
newtype Parser s e a = Parser (s -> Result s e a)
    deriving stock Functor


{- | The 'Bifunctor' instance, providing functoriality in the error type.

For a function @f :: d -> e@, @'first' f@ preserves all the structure in sight. Specifically,
@'first' f@ preserves the 'Applicative' structure,

@
first f . pure == pure
first f (p \<*\> q) == (first f p) \<*\> (first f q) 
@

the 'Monad' structure,

@
first f (p >>= h) == (first f p) >>= (first f .  h) 
@

and, assuming @f@ is a /monoid morphism/, the 'Alternative' structure,

@
first f empty == empty
first f (p \<|\> q) == (first f p) \<|\> (first f q)
@
-}
instance Bifunctor (Parser s) where
    {-# INLINE bimap #-}
    bimap :: (d -> e) -> (a -> b) -> Parser s d a -> Parser s e b
    bimap f g p = embed $ bimap f g . run p

{- | The 'Applicative' instance.

Allows sequencing of parsers and combine their results. With @'pure' x@ values can be embedded in a
parser. The parser @p \<*\> q@ first runs @p@ then @q@, and returns the result of @p@ applied to
the result of @q@.

note(s):

  * The parser @p \<*\> q@ short-circuits on @p@ erroring out, that is, @q@ never runs.
-}
instance Applicative (Parser s e) where
    {-# INLINE pure #-}
    pure :: a -> Parser s e a
    pure x = embed $ \ xs -> Success x xs

    {-# INLINE (<*>) #-}
    (<*>) :: Parser s e (a -> b) -> Parser s e a -> Parser s e b
    (<*>) p q = embed $ \ xs ->
        case run p xs of
            Error d      -> Error d
            Success f ys -> case run q ys of
                    Error e      -> Error e
                    Success x zs -> Success (f x) zs

{- | The 'Monad' instance.

The bind combinator @p >>= h@ first runs @p@ and then the parser obtained by applying the monadic
function @h@ to the result.

note(s):

  * As with @p \<*\> q@, @p >>= h@ short-circuits on @p@ erroring out.
-}
instance Monad (Parser s e) where
    {-# INLINE (>>=) #-}
    (>>=) :: Parser s e a -> (a -> Parser s e b) -> Parser s e b
    (>>=) p h = embed $ \ xs ->
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

@
pure x \<|\> p == pure x
empty \<*\> p == empty
empty >>= h == empty
@

but /not/ their right-sided versions because of short-circuiting.

Furthermore, if the monoid @e@ is /idempotent/, that is, for all @x :: e@, @x <> x == x@, then the
'Alternative' instance also satisfies /left distributivity/:

@
(f \<*\> x) \<|\> (f \<*\> y) == f \<*\> (x \<|\> y)
@

=== __Counterexample:__

The next example shows that right distributivity is violated even with an idempotent monoid.

>>> let f = bimap (const ()) (const id) $ matchOne '0'
>>> let g = first (const ()) (matchOne '0') *> bimap (const ()) (const id) (matchOne '1')
>>> let x = first (const ()) (matchOne '2')
>>> parse ((f <|> g) <*> x) ("0123")
Left ()
>>> parse ((f <*> x) <|> (g <*> x)) ("0123")
Right ('2',"3")
-}
instance Monoid e => Alternative (Parser s e) where
    {-# INLINE empty #-}
    empty :: Parser s e a
    empty = embed $ const (Error mempty)

    {-# INLINE (<|>) #-}
    (<|>) :: Parser s e a -> Parser s e a -> Parser s e a
    (<|>) p q = embed $ \ xs ->
        case run p xs of
            x@(Success _ _) -> x
            Error e         ->
                case run q xs of
                    x'@(Success _ _) -> x'
                    Error e'         -> Error (e <> e')

{- | The 'MonadState' instance.

The @'get'@ parser allows probing the t'Parser' state, e.g.:

@
    do
        s <- get
        -- Do something with @s@.
@

The 'get' parser does not throw an error or consume input while the 'put' parser allows changing
the t'Parser' state.

__Definition__: a parser @p@ is /normal/ if for every input @xs@, on success, the 'remainder' is a,
possibly improper, suffix of @xs@.

All the parsers in the library are provably normal, and all parser combinators return normal
parsers on the assumption that the arguments are normal, but the 'MonadState' typeclass,
specifically the 'put' method, allows the construction of non-normal parsers.

note(s):

  * To formalize the notion of /suffix/, a @'Trisagion.Typeclasses.Streamable.Streamable' a s@
  constraint on @s@ is needed -- see 'Trisagion.Typeclasses.Streamable.isSuffix'.
-}
instance MonadState s (Parser s e) where
    {-# INLINE get #-}
    get :: Parser s e s
    get = embed $ \ xs -> Success xs xs

    {-# INLINE put #-}
    put :: s -> Parser s e ()
    put xs = embed $ const (Success () xs)

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
    throwError e = embed $ const (Error e)

    {-# INLINE catchError #-}
    catchError :: Parser s e a -> (e -> Parser s e a) -> Parser s e a
    catchError p h = embed $ \ s ->
        -- Case statement instead of 'catch' to make use of sharing in the Success branch.
        case run p s of
            x@(Success _ _) -> x
            Error e         -> run (h e) s


{- | Embed a parsing function in the t'Parser' monad. -}
{-# INLINE embed #-}
embed ::  (s ->  Result s e a) -> Parser s e a
embed = Parser

{- | Run the parser on the input and return the results.

The inverse of @'embed'@.
-}
{-# INLINE run #-}
run :: Parser s e a -> s ->  Result s e a
run (Parser f) = f

{- | Parse the input and return the result as an 'Either'. -}
{-# INLINE parse #-}
parse :: Parser s e a -> s -> e :+: (a, s)
parse p = toEither . run p

{- | Evaluate the parser on the input and return the result, discarding the remainder. -}
{-# INLINE eval #-}
eval :: Parser s e a -> s -> e :+: a
eval p = fmap fst . parse p

{- | Run the parser on the input and return the remainder, discarding the parsed value. -}
{-# INLINE remainder #-}
remainder :: Parser s e a -> s -> e :+: s
remainder p = fmap snd . parse p


{- | The type-changing version of 'catchError'.

The parser @'catch' p h@ runs @p@ and if it throws an error @e@, backtracks and runs @h e@.
-}
{-# INLINE catch #-}
catch
    :: Parser s d a                     -- ^ Parser to try.
    -> (d -> Parser s e a)              -- ^ Error handler.
    -> Parser s e a
catch p h = embed $ \ xs ->
    case run p xs of
        Error e      -> run (h e) xs
        Success x ys -> Success x ys

{- | Parser implementing backtracking.

The parser @'try' p@ runs @p@ and returns the result as a 'Right'; on @p@ throwing an error, it
backtracks and returns the error as a 'Left'.

=== __Examples:__

>>> parse (try one) "0123"
Right (Right '0',"123")

>>> parse (try $ matchOne '1') "0123"
Right (Left (Left (ValidationError '0')),"0123")

>>> parse (try one) ""
Right (Left (InputError 1),"")
-}
{-# INLINE try #-}
try :: Parser s e a -> Parser s Void (e :+: a)
try p = embed $ \ xs ->
    case run p xs of
        Error e      -> Success (Left e) xs
        Success x ys -> Success (Right x) ys

{- | Run the parser and return the result, validating it.

=== __Examples:__

>>> parse (validate (\ c -> if c == '0' then Right c else Left ()) one) "0123"
Right ('0',"123")

>>> parse (validate (\ c -> if c == '0' then Right c else Left ()) one) "123"
Left (Left ())

>>> parse (validate (\ c -> if c == '0' then Right c else Left ()) one) ""
Left (Right (InputError 1))
-}
{-# INLINE validate #-}
validate
    :: (a -> d :+: b)                   -- ^ Validator.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s (d :+: e) b
validate v p = do
    x <- first Right p
    case v x of
        Left d  -> throwError $ Left d
        Right y -> pure y

{- | Run the parser and return the result, but do not consume any input.

=== __Examples:__

>>> parse (lookAhead one) "0123"
Right (Right '0',"0123")

>>> parse (lookAhead $ matchOne '1') "0123"
Right (Left (Left (ValidationError '0')),"0123")

>>> parse (lookAhead one) ""
Right (Left (InputError 1),"")
-}
{-# INLINE lookAhead #-}
lookAhead :: Parser s e a -> Parser s Void (e :+: a)
lookAhead p = fmap (eval p) get
