-- {- | Drop a fixed size prefix from the stream. -}
-- drop :: Word -> ParserT s Void m ()
-- drop = skip . take

-- {- | Drop the longest prefix from the stream whose elements satisfy a predicate. -}
-- dropWhile :: (a -> Bool) -> ParserT s Void m ()
-- dropWhile = skip . takeWhile

-- {- | Parse the longest prefix with at least one element, whose elements satisfy a predicate. -}
-- takeWhile1 :: (a -> Bool) -> ParserT s (ValidationError a :+: InputError) m b
-- takeWhile1 p = do
--     x <- mapError absurd $ lookAhead (satisfy p)
--     case x of
--         Left e  -> throw e
--         Right _ -> mapError absurd $ takeWhile p

-- {- | Run a parser isolated to a fixed size prefix of the stream.

-- The prefix on which the parser runs may have a size smaller than @n@ if there is not enough input
-- in the stream. Any unconsumed input in the prefix is returned along with the result.
-- -}
-- {-# INLINE isolate #-}
-- isolate
--     :: Splittable m a b s
--     => Word                             -- ^ Prefix size.
--     -> ParserT b e m a                  -- ^ Parser to run on the prefix.
--     -> ParserT s e m (a, b)
-- isolate n p = do
--     prefix <- mapError absurd $ take n
--     r <- lift (parse p prefix)
--     case r of
--         Left e  -> throw e
--         Right x -> pure x

-- {- | Run the parser and return its result along with the prefix of consumed input.

-- note(s):

--   * Implementation requires computing the difference of offsets, so it implicitly relies on
--     normality of @p@.
-- -}
-- {-# INLINE consumed #-}
-- consumed
--     :: (HasOffset m s, Splittable m a b s)
--     => ParserT s e m a                  -- ^ Parser to run.
--     -> ParserT s e m (b, a)
-- consumed p = do
--     xs    <- get
--     start <- mapError absurd offset
--     x     <- p
--     end   <- mapError absurd offset
--     -- Implicitly relies on the parser @p@ being normal, for positivity of @end - start@.
--     ys    <- either absurd id <$> lift (eval (take (end - start)) xs)
--     pure (ys, x)
