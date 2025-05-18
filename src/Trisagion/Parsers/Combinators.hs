{- |
Module: Trisagion.Parsers.Combinators

Various parser combinators. The module should be imported qualified as some of the exported names
conflict with base, specifically, methods of 'Control.Applicative.Alternative'.
-}

module Trisagion.Parsers.Combinators (
    -- * Parsers without errors.
    optional,
    failIff,

    -- * 'Applicative' parsers.
    skip,
    between,
    pair,
    pairWith,
    count,
    chain,

    -- * 'Alternative' parsers.
    many,
    some,
    skipMany,
    skipSome,
    choose,
    pick,
    untilEnd,
    manyTillEnd,
    manyTill,

    -- * List parsers.
    sepBy,
    sepBy1,
) where

-- Imports.
-- Base.
import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Void (Void, absurd)

-- Package.
-- Import entire module for doctests.
import Trisagion.Parser


{- | @'maybe' p@ runs @p@ returning the result as a 'Just'. On error, backtrack and return 'Nothing'.

The difference with 'Control.Applicative.optional' from 'Control.Applicative.Alternative' is the
more precise type signature.

=== __Examples:__

>>> parse (optional (matchOne '0')) (initialize "0123")
Right (Just '0',Counter 1 "123")

>>> parse (optional (matchOne '1')) (initialize "0123")
Right (Nothing,Counter 0 "0123")

>>> parse (optional (matchOne '1')) (initialize "")
Right (Nothing,Counter 0 "")
-}
{-# INLINE optional #-}
optional :: Parser s e a -> Parser s Void (Maybe a)
optional p = either (const Nothing) Just <$> try p

{- | The parser @'failIff' p@ fails if and only if @p@ succeeds.

The parser does not consume input and throws the monoid unit for @e@ if @p@ succeeds.

note(s):

  * This parser can be used to implement the longest match rule -- see 'until'.

=== __Examples:__

>>> parse (failIff (matchOne '1')) (initialize "0123")
Right ((),Counter 0 "0123")

>>> parse (failIff (matchOne '0')) (initialize "0123")
Left Nil

>>> parse (failIff (matchOne '0')) (initialize "")
Right ((),Counter 0 "")
-}
{-# INLINE failIff #-}
failIff :: Monoid e => Parser s e a -> Parser s e ()
failIff p = do
    r <- first absurd $ lookAhead p
    case r of
        Left  _ -> pure ()
        Right _ -> throw mempty


{- | Run the parser and discard the result. -}
{-# INLINE skip #-}
skip :: Parser s e a -> Parser s e ()
skip = ($> ())

{- | The parser @'between' o c p@ runs @o@, @p@ and @c@, returning the result of @p@.

=== __Examples:__

>>> parse (between (matchOne '{') (matchOne '}') (first (fmap absurd) one)) (initialize "{1}3")
Right ('1',Counter 3 "3")

>>> parse (between (matchOne '{') (matchOne '}') (first (fmap absurd) one)) (initialize "11}3")
Left (Cons (ErrorItem 1 (ValidationError '1')) [])

>>> parse (between (matchOne '{') (matchOne '}') (first (fmap absurd) one)) (initialize "{123")
Left (Cons (ErrorItem 3 (ValidationError '2')) [])

>>> parse (between (matchOne '{') (matchOne '}') (first (fmap absurd) one)) (initialize "")
Left (Cons (EndOfInput 1) [])

>>> parse (between (matchOne '{') (matchOne '}') (first (fmap absurd) one)) (initialize "{")
Left (Cons (EndOfInput 1) [])

>>> parse (between (matchOne '{') (matchOne '}') (first (fmap absurd) one)) (initialize "{1")
Left (Cons (EndOfInput 1) [])
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

>>> parse (pair one one) "0123"
Right (('0','1'),"23")

>>> parse (pair (matchOne '1') (matchOne '1')) (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse (pair (matchOne '0') (matchOne '0')) (initialize "0123")
Left (Cons (ErrorItem 2 (ValidationError '1')) [])
-}
{-# INLINE pair #-}
pair :: Parser s e a -> Parser s e b -> Parser s e (a, b)
pair = liftA2 (,)

{- | Sequence two parsers and pair the results with a binary function. -}
{-# INLINE pairWith #-}
pairWith :: (a -> b -> c) -> Parser s e a -> Parser s e b -> Parser s e c
pairWith = liftA2

{- | Run the parser @n@ times and return the list of results.

=== __Examples:__

>>> parse (count 2 one) "0123"
Right ("01","23")

>>> parse (count 10 one) "0123"
Left (Cons (EndOfInput 1) [])

>>> parse (count 2 (matchOne '0')) (initialize "0123")
Left (Cons (ErrorItem 2 (ValidationError '1')) [])
-}
{-# INLINEABLE count #-}
count :: Word -> Parser s e a -> Parser s e [a]
count n p = go n
    where
        go 0 = pure []
        go m = (:) <$> p <*> go (pred m)

{- | Chain together a traversable of parsers and return the traversable of results. -}
{-# INLINE chain #-}
chain :: Traversable t => t (Parser s e a) -> Parser s e (t a)
chain = sequenceA


{- | Choose between two parsers.

Run the first parser and if it fails run the second. Return the results as an @'Either'@.

=== __Examples:__

>>> parse (choose one one) (initialize "0123")
Right (Left '0',Counter 1 "123")

>>> parse (choose (matchOne '1') (first (fmap absurd) one)) (initialize "0123")
Right (Right '0',Counter 1 "123")

>>> parse (choose (matchOne '1') (matchOne '2')) (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])
-}
{-# INLINE choose #-}
choose :: Monoid e => Parser s e a -> Parser s e b -> Parser s e (a :+: b)
choose q p = (Left <$> q) <|> (Right <$> p)

{- | Pick between a foldable of parsers.

Run the parsers in succession returning the result of the first successful one.
-}
{-# INLINE pick #-}
pick :: (Foldable t, Monoid e) => t (Parser s e a) -> Parser s e a
pick = asum

{- | Run the parser zero or more times until it fails, returning the list of results.

The difference with @'Control.Applicative.many'@ from 'Control.Applicative.Alternative' is the more
precise type signature.

note(s):

  * The @'many' p@ parser can loop forever if fed a parser @p@ that does not throw an error and
  does not consume input, e.g. any parser with @'Void'@ in the error type or their polymorphic
  variants, like @'pure' x@, @'Control.Applicative.many' p@, etc.

=== __Examples:__

>>> parse (many (satisfy ('{' /=))) (initialize "01{3")
Right ("01",Counter 2 "{3")

>>> parse (many (satisfy ('0' /=))) (initialize "0123")
Right ("",Counter 0 "0123")

>>> parse (many (satisfy ('0' ==))) (initialize "")
Right ("",Counter 0 "")

>>> parse (take 2 <$> many (satisfy ('0' ==))) (initialize "0000456")
Right ("00",Counter 4 "456")
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

{- | Run the parser one or more times until it fails and return the results as a @'NonEmpty'@.

The difference with @'Control.Applicative.some'@ from 'Control.Applicative.Alternative' is the more
precise type signature.

=== __Examples:__

>>> parse (some (satisfy ('{' /=))) (initialize "01{3")
Right ('0' :| "1",Counter 2 "{3")

>>> parse (some (satisfy ('0' /=))) (initialize "0123")
Left (Cons (ErrorItem 1 (ValidationError '0')) [])

>>> parse (some (satisfy ('0' /=))) (initialize "")
Left (Cons (EndOfInput 1) [])
-}
{-# INLINE some #-}
some :: Parser s e a -> Parser s e (NonEmpty a)
some p = liftA2 (:|) p (first absurd $ many p)

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

  * The difference with 'manyTillEnd' is that the @end@ parser will not consume any input.

=== __Examples:__

>>> parse (untilEnd (matchOne '}') (first (fmap absurd) one)) (initialize "01}3")
Right ("01",Counter 2 "}3")

>>> parse (untilEnd (matchOne '}') (first (fmap absurd) one)) (initialize "}123")
Right ("",Counter 0 "}123")

>>> parse (untilEnd (matchOne '}') (first (fmap absurd) one)) (initialize "")
Right ("",Counter 0 "")
-}
{-# INLINE untilEnd #-}
untilEnd
    :: Monoid e
    => Parser s e b                   -- ^ Closing parser.
    -> Parser s e a                   -- ^ Parser to run.
    -> Parser s Void [a]
untilEnd end p = many $ failIff end *> p

{- | @'manyTillEnd' end p@ runs @p@ until @end@ succeeds, returning the results of @p@ and @end@.

=== __Examples:__

>>> parse (manyTillEnd (matchOne '}') (first (fmap absurd) one)) (initialize "01}3")
Right ('0' :| "1}",Counter 3 "3")
-}
{-# INLINEABLE manyTillEnd #-}
manyTillEnd :: Monoid e => Parser s e a -> Parser s e a -> Parser s e (NonEmpty a)
manyTillEnd end p = go
    where
        go = do
            r <- choose end p
            case r of
                Left e  -> pure $ e :| []
                Right x -> (x <|) <$> go

{- | @'manyTill' end p@ runs @p@ until @end@ succeeds, returning the results of @p@. -}
{-# INLINEABLE manyTill #-}
manyTill :: Monoid e => Parser s e a -> Parser s e b -> Parser s e [b]
manyTill end p = go
    where
        go = do
            r <- choose end p
            case r of
                Left _  -> pure []
                Right x -> (x :) <$> go

{- | The parser @'sepBy' sep p@ parses zero or more occurences of @p@ separated by @sep@. -}
{-# INLINE sepBy #-}
sepBy :: Parser s e a -> Parser s e b -> Parser s Void [b]
sepBy sep p = do
    x <- try p
    case x of
        Left _  -> pure []
        Right y -> (y :) <$> many (sep *> p)

{- | The parser @'sepBy1' sep p@ parses one or more occurences of @p@ separated by @sep@.

=== __Examples:__

>>> parse (sepBy1 (matchOne ',') (first (fmap absurd) one)) (initialize "0,1,2,345")
Right ('0' :| "123",Counter 7 "45")

>>> parse (sepBy1 (matchOne ',') (first (fmap absurd) one)) (initialize "0123")
Right ('0' :| "",Counter 1 "123")

>>> parse (sepBy1 (matchOne ',') (first (fmap absurd) one)) (initialize "")
Left (Cons (EndOfInput 1) [])
 -}
{-# INLINE sepBy1 #-}
sepBy1 :: Parser s e a -> Parser s e b -> Parser s e (NonEmpty b)
sepBy1 sep p = liftA2 (:|) p (first absurd $ many (sep *> p))
