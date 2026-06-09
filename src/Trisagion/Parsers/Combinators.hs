{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    value,
    optional,

    -- * Error parsers.
    failIff,
    validate,
    lookAhead,

    -- * 'Applicative' parsers.
    skip,
    between,
    zipP,
    zipWithP,
    count,
    sequenceP,

    -- * 'Alternative' parsers.
    eitherP,
    choice,
    many,
    some,
    skipMany,
    skipSome,
    untilEnd,
    manyTill,
    manyTillEnd,
    sepBy,
    sepBy1,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative ((<|>)), asum)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import Data.Void (Void, absurd)

-- Libraries.
import Control.Monad.Except (MonadError (..))

-- Package.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser, try, eval)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Control.Monad.State (MonadState(..))


-- $setup
-- >>> import Data.Bifunctor
-- >>> import Trisagion.Streams.Counter
-- >>> import Trisagion.Parser
-- >>> import Trisagion.Parsers.Source
-- >>> import Trisagion.Parsers.ParseError


{- | Embed a value in the t'Parser' monad.

The difference with 'pure' is the more precise type signature.
-}
{-# INLINE value #-}
value :: a -> Parser s Void a
value = pure

{- | @'optional' p@ runs @p@ returning the result as a 'Just'. On error, backtrack and return 'Nothing'.

The difference with 'Control.Applicative.optional' from 'Control.Applicative.Alternative' is the
more precise type signature.

=== __Examples:__

>>> parse (optional $ matchOne '0') "0123"
Right (Just '0',"123")

>>> parse (optional $ matchOne '1') "0123"
Right (Nothing,"0123")

>>> parse (optional $ matchOne '0') ""
Right (Nothing,"")
-}
{-# INLINE optional #-}
optional :: Parser s e a -> Parser s Void (Maybe a)
optional p = either (const Nothing) Just <$> try p

{- | The parser @'failIff' p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws the monoid unit for @e@ if @p@ succeeds.

note(s):

  * This parser can be used to implement the longest match rule -- see 'untilEnd'.

=== __Examples:__

>>> parse (failIff (withParseError $ matchOne '1')) (initialize "0123")
Right ((),Counter 0 "0123")

>>> parse (failIff (withParseError $ matchOne '0')) (initialize "0123")
Left Failure

>>> parse (failIff (withParseError $ matchOne '0')) (initialize "")
Right ((),Counter 0 "")
-}
{-# INLINE failIff #-}
failIff :: Monoid e => Parser s e a -> Parser s e ()
failIff p = do
    r <- first absurd $ lookAhead p
    case r of
        Left  _ -> pure ()
        Right _ -> throwError mempty

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


{- | Run the parser and discard the result. -}
{-# INLINE skip #-}
skip :: Parser s e a -> Parser s e ()
skip = ($> ())

{- | The parser @'between' o c p@ runs @o@, @p@ and @c@, returning the result of @p@.

=== __Examples:__

>>> parse (between (matchOne '{') (matchOne '}') (first Right one)) "{1}3"
Right ('1',"3")

>>> parse (between (matchOne '{') (matchOne '}') (first Right one)) "11}3"
Left (Left (ValidationError '1'))

>>> parse (between (matchOne '{') (matchOne '}') (first Right one)) "{123"
Left (Left (ValidationError '2'))

>>> parse (between (matchOne '{') (matchOne '}') (first Right one)) ""
Left (Right (InputError 1))

>>> parse (between (matchOne '{') (matchOne '}') (first Right one)) "{"
Left (Right (InputError 1))

>>> parse (between (matchOne '{') (matchOne '}') (first Right one)) "{1"
Left (Right (InputError 1))
-}
{-# INLINE between #-}
between
    :: Parser s e b                     -- ^ Opening parser.
    -> Parser s e c                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run in-between.
    -> Parser s e a
between open close p = open *> p <* close

{- | Sequence two parsers and pair up the results.

=== __Examples:__

>>> parse (zipP one one) "0123"
Right (('0','1'),"23")

>>> parse (zipP (matchOne '1') (matchOne '1')) "0123"
Left (Left (ValidationError '0'))

>>> parse (zipP (matchOne '0') (matchOne '2')) "0123"
Left (Left (ValidationError '1'))
-}
{-# INLINE zipP #-}
zipP :: Parser s e a -> Parser s e b -> Parser s e (a, b)
zipP = zipWithP (,)

{- | Sequence two parsers and pair the results with a binary function.

=== __Examples:__

>>> parse (zipWithP max one one) "0123"
Right ('1',"23")
-}
{-# INLINE zipWithP #-}
zipWithP :: (a -> b -> c) -> Parser s e a -> Parser s e b -> Parser s e c
zipWithP = liftA2

{- | Run the parser @n@ times and return the list of results.

=== __Examples:__

>>> parse (count 2 one) "0123"
Right ("01","23")

>>> parse (count 10 one) "0123"
Left (InputError 1)

>>> parse (count 2 (matchOne '0')) "0123"
Left (Left (ValidationError '1'))
-}
{-# INLINEABLE count #-}
count :: Word -> Parser s e a -> Parser s e [a]
count n p = go n
    where
        go 0 = pure []
        go m = (:) <$> p <*> go (pred m)

{- | Sequence together a traversable of parsers and return the traversable of results. -}
{-# INLINE sequenceP #-}
sequenceP :: Traversable t => t (Parser s e a) -> Parser s e (t a)
sequenceP = sequenceA


{- | Choose between two alternative parsers.

Run the first parser and if it fails run the second. Return the results as an @'Either'@.

=== __Examples:__

The parser 'eitherP' requires a 'Monoid' constraint so the error type must be embedded in a monoid.
That is the function of 'Trisagion.Parsers.ParseError.withParseError' in the tests below.

>>> parse (eitherP (withParseError one) (withParseError one)) (initialize "0123")
Right (Left '0',Counter 1 "123")

>>> parse (eitherP (withParseError $ matchOne '1') (withParseError $ first Right one)) (initialize "0123")
Right (Right '0',Counter 1 "123")

>>> parse (eitherP (withParseError $ matchOne '1') (withParseError $ matchOne '2')) (initialize "0123")
Left (ParseError 0 (Left (ValidationError '0')))
-}
{-# INLINE eitherP #-}
eitherP :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (a :+: b)
eitherP q p = (Left <$> q) <|> (Right <$> p)

{- | Choose between a foldable of parsers.

Run the parsers in succession, returning the result of the first successful one.
-}
{-# INLINE choice #-}
choice :: (Foldable t, Monoid e) => t (Parser s e a) -> Parser s e a
choice = asum

{- | Run the parser zero or more times until it fails, returning the list of results.

The difference with @'Control.Applicative.many'@ from 'Control.Applicative.Alternative' is the more
precise type signature and the fact that it does not require a @'Monoid' e@ constraint.

note(s):

  * The @'many' p@ parser can loop forever if fed a parser @p@ that does not throw an error and
  possibly does not consume input, e.g. any parser with @'Void'@ in the error type or their
  polymorphic variants, like @'pure' x@, @'Control.Applicative.many' p@, etc.

=== __Examples:__

>>> parse (many (satisfy ('{' /=))) "01{3"
Right ("01","{3")

>>> parse (many (satisfy ('0' /=))) "0123"
Right ("","0123")

>>> parse (many (satisfy ('0' ==))) ""
Right ("","")

>>> parse (take 2 <$> many (satisfy ('0' ==))) "0000456"
Right ("00","456")
-}
{-# INLINEABLE many #-}
many :: Parser s e a -> Parser s Void [a]
many p = go
    where
        go = do
            r <- try p
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | Run the parser one or more times until it fails and discard the results.

The difference with @'Control.Applicative.some'@ from 'Control.Applicative.Alternative' is the more
precise type signature and the fact that it does not require a @'Monoid' e@ constraint.

=== __Examples:__

>>> parse (some (satisfy ('{' /=))) "01{3"
Right ('0' :| "1","{3")

>>> parse (some (satisfy ('0' /=))) "0123"
Left (Left (ValidationError '0'))

>>> parse (some (satisfy ('0' /=))) ""
Left (Right (InputError 1))
-}
{-# INLINE some #-}
some :: Parser s e a -> Parser s e (NonEmpty a)
some p = zipWithP (:|) p (first absurd (many p))

{- | Run the parser zero or more times until it fails and discard the results. -}
{-# INLINEABLE skipMany #-}
skipMany :: Parser s e a -> Parser s Void ()
skipMany p = go
    where
        go = do
            r <- try p
            case r of
                Left _  -> pure ()
                Right _ -> go

{- | Run the parser one or more times until it fails and discard the results. -}
{-# INLINE skipSome #-}
skipSome :: Parser s e a -> Parser s e ()
skipSome p = p *> first absurd (skipMany p)

{- | The parser @'untilEnd' end p@ runs @p@ zero or more times until @end@ succeeds.

note(s):

  * The difference with 'manyTill' is the need for the monoid constraint and the fact that the
  @end@ parser will not consume any input.

=== __Examples:__

The parser 'untilEnd' requires a 'Monoid' constraint so the error type must be embedded in a monoid.
That is the function of 'Trisagion.Parsers.ParseError.withParseError' in the tests below.

>>> parse (untilEnd (withParseError $ matchOne '}') (withParseError $ first Right one)) (initialize "01}3")
Right ("01",Counter 2 "}3")

>>> parse (untilEnd (withParseError $ matchOne '}') (withParseError $ first Right one)) (initialize "}123")
Right ("",Counter 0 "}123")

>>> parse (untilEnd (withParseError $ matchOne '}') (withParseError $ first Right one)) (initialize "")
Right ("",Counter 0 "")
-}
{-# INLINE untilEnd #-}
untilEnd
    :: Monoid e
    => Parser s e b                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s Void [a]
untilEnd end p = many $ failIff end *> p

{- | @'manyTill' end p@ runs @p@ until @end@ succeeds, returning the results of @p@.

=== __Examples:__

>>> parse (manyTill (matchOne '}') (first Right one)) "01}3"
Right ("01","3")
-}
{-# INLINEABLE manyTill #-}
manyTill :: Parser s e a -> Parser s e b -> Parser s e [b]
manyTill end p = go
    where
        go = do
            r <- catchError (fmap Left end) (const (fmap Right p))
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | @'manyTillEnd' end p@ runs @p@ until @end@ succeeds, returning the results of @p@ and @end@.

=== __Examples:__

>>> parse (manyTillEnd (matchOne '}') (first Right one)) "01}3"
Right ('0' :| "1}","3")
-}
{-# INLINEABLE manyTillEnd #-}
manyTillEnd
    :: Parser s e a                     -- ^ Closing parser.
    -> Parser s e a                     -- ^ Parser to run.
    -> Parser s e (NonEmpty a)
manyTillEnd end p = go
    where
        go = do
            r <- catchError (fmap Left end) (const (fmap Right p))
            case r of
                Left e  -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurrences of @p@ separated by @sep@.-}
{-# INLINE sepBy #-}
sepBy :: Parser s e a -> Parser s e b -> Parser s Void [b]
sepBy sep p = do
    x <- try p
    case x of
        Left _  -> pure []
        Right y -> (y :) <$> many (sep *> p)

{- | The parser @'sepBy1' sep p@ parses one or more occurrences of @p@ separated by @sep@.

=== __Examples:__

>>> parse (sepBy1 (matchOne ',') (first Right one)) "0,1,2,345"
Right ('0' :| "123","45")

>>> parse (sepBy1 (matchOne ',') (first Right one)) "0123"
Right ('0' :| "","123")

>>> parse (sepBy1 (matchOne ',') (first Right one)) ""
Left (Right (InputError 1))
-}
{-# INLINE sepBy1 #-}
sepBy1 :: Parser s e a -> Parser s e b -> Parser s e (NonEmpty b)
sepBy1 sep p = liftA2 (:|) p (first absurd $ many (sep *> p))
