{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- https://stackoverflow.com/questions/18957873/haskell-parenthesis-matching-for-find-and-replace
-- import Text.ParserCombinators.Parsec

import Network.Wreq (get, responseBody)
import Control.Lens
import Data.Aeson.Lens
import Data.Text (unpack)

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Void
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Bifunctor
import Data.Proxy
import Data.Foldable
import Data.Traversable

main :: IO ()
main = do
    input <- getContents
    let inputtest = "`==` `Eq` ` __`Ord`__ `T.D` `T dd` "
    -- let inputtest = "  `==` `Eq` ` ` __`Ord`__ `T.D` `T dd` "
    --- case (parse (finall backtickSymbol >> many anySingle) "" inputtest) of
    ---     Left err -> print err
    ---     Right r -> print r
    --- mzero

    case (parse (findall backtickSymbol) "" input) of
        Left err -> print err
        Right groups -> do
            -- print groups
            -- putStr =<< fmap join (mapM replace groups)
            putStr =<< fmap unlines (mapM replace groups)

replace :: Either String (String, (String, String, String)) -> IO String
replace (Left cap) = return "" -- cap
replace (Right (cap, (tickOpen, symbol, tickClose))) =
    --- runMaybeT $ fromMaybe cap $ do
    -- If anything doesn't work out then just use the original capture `cap`.
    -- TODO don't use runMaybeT
    fmap (fromMaybe cap) $ runMaybeT $ do
        -- Bail out if this doesn't look like a legitimate Haskell token
        -- guard $ all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ".<>+=-$/:'")) token
        -- MaybeT $ return $ guard $ flip all token $ flip elem $ ['a'..'z'] ++ ['A'..'Z'] ++ ".<>+=-$/:'"

        lift $ putStrLn $ "query " ++ symbol
        -- Query hoogle for the symbol, only in the base package, only 1 result.
        hoogleResult <- lift $ get $  "https://hoogle.haskell.org?mode=json&hoogle="
                            ++ symbol
                            ++ "&scope=package%3Abase&start=1&count=1"

        -- If hoogle returns a documentation URL
        docUrl <- MaybeT $ return $ listToMaybe $ hoogleResult ^.. responseBody . nth 0 . key "url" . _String

        -- Construct a Markdown link with the documentation URL
        MaybeT $ return $ Just $ "[" ++ tickOpen ++ symbol ++ tickClose ++ "](" ++ unpack docUrl ++ ")"

type Parser = Parsec Void String


-- Parse something that looks like a symbol from Prelude in backticks.
backtickSymbol :: Parser (String, String, String)
backtickSymbol = do
    tickOpen  <- chunk "`"
    symbol     <- Text.Megaparsec.some $ Text.Megaparsec.oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ".<>+=-$/:'" :: String)
    tickClose <- chunk "`"
    return (tickOpen, symbol, tickClose)


-- Find and parse all of the non-overlapping substrings of a string which match
-- a pattern given by a parser.
--
-- This is a parser combinator which can be used for pattern capture
-- situations similar to when one would use the Python
-- [`re.findall`](https://docs.python.org/3/library/re.html#re.findall)
-- or
-- Unix [`grep`](https://www.gnu.org/software/grep/).
--
-- This combinator can also be used as a stream editor, in situations
-- when one would use Python
-- [`re.sub`](https://docs.python.org/3/library/re.html#re.sub)
-- , or Unix
-- [`sed` substitute](https://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html),
-- or
-- [`awk`](https://www.gnu.org/software/gawk/manual/gawk.html).
--
-- The `findall` function takes a pattern parser as an argument, and returns a
-- parser which will consume an entire input stream and find all the places
-- in the input stream which match the pattern.
-- The result is a list of `Right` pattern matches and `Left` unmatched sections
-- of the input stream.
-- The result can be examined for parsed matches, or reconstructed
-- into an edited output stream.
--
-- Examples
--
-- Given a parser for numbers in simple scientific notation like `"1E2"`:
--
--     scinum :: Parsec Void String (Double, Integer)
--     scinum = do
--         m <- some digitChar
--         string "E"
--         e <- some digitChar
--         return (read m, read e)
--
--     import Data.Either
--     import Data.Maybe
--
--     let input = "1E2 xxx 2E3"
--
-- 1. Parse the structure of the entire input string:
--
--        print $ fromJust $ parseMaybe (findall scinum) input
--
--    Entire input structure:
--
--        [Right ("1E2",(1.0,2)), Left " xxx ", Right ("2E3",(2.0,3))]
--
-- 2. Capture the parsed pattern matches:
--
--        print $ fmap snd
--              $ rights
--              $ fromJust $ parseMaybe (findall scinum) input
--
--    Parsed pattern matches:
--
--        [(1.0,2), (2.0,3)]
--
-- 3. Replace all of the matched numbers with decimal notation:
--
--        print $ foldMap (either id (\(_,(m,e)) -> show $ m * (10 ^^ e)))
--              $ fromJust $ parseMaybe (findall scinum) input
--
--    Input string with scientific notation replaced by decimal notation:
--
--        "100.0 xxx 2000.0"
--
-- Make sure we test that the parser is correctly consuming its input.
--
-- Make sure we test that the parser is correctly calculating its position.
--
-- Make sure we test that the parser will continue in event of a parse that
-- fails on an operation like `read`.
--
-- TODO We actually don't need to call `match`, we could let the user choose
-- to call `match`.
-- TODO generalize from Parser to Parsec
--
-- We need the Semigroup instance for `s` because a Megaparsec Stream has
-- methods for unconsing the Stream, but no methods for consing the Stream,
-- and findall needs to build an output stream, not just parse the input stream.
findall :: forall e s m a. (MonadParsec e s m) => m a -> m [Either (Tokens s) (Tokens s, a)]
-- findall :: forall e s m a. MonadParsec e s m => m a -> m [Either (Tokens s) (Tokens s, a)]
-- findall :: forall a. ParsecT e s m a -> ParsecT e s m [Either String (String, a)]
findall pattern = do
    (fmap.fmap) (first $ tokensToChunk (Proxy::Proxy s)) loop
   -- st0 <- getParserState
   -- (st', caps) <- loop st0
   -- (st', caps) <- runParserT'
   --updateParserState (const st') -- Update state so Parser has consumed all input.
   -- return caps
  where
    -- take a parser state and build up a list of capture results
    -- loop :: State s -> m (State s, [(Either (Tokens s) (Tokens s, a))])
    loop =
        (observing eof) >>= \case
            (Right _) -> return []
            (Left _) ->
                --- case (runParserT' (match pattern) st) of
                (observing (try (match pattern))) >>= \case
                    (Left _) ->
                        -- No match, so add the first character to the Left non-matching
                        -- results and then step forward and loop.
                        -- TODO use `token` instead of `anySingle`
                        anySingle >>= \ oneChar ->
                                -- now collect the oneChar and loop with stIncrem
                                loop >>= \case
                                    ((Left cap):caps) ->
                                        return (Left (oneChar:cap):caps)
                                    caps ->
                                        return (Left ([oneChar]):caps)
                                --TODO generalized Token cons instead of tokenToChunk?
                    (Right mach) ->
                        -- Found a match, add to results and loop.
                        fmap ((Right mach):) loop


-- two new functions:
--

-- | Separate and Capture
-- Separate a stream into sections which match a pattern,
-- and non-matching sections.
sepCap
    :: MonadParsec e s m
    -- :: (MonadParsec e s m, Tokens s ~ s)
    => m a
    -> m [Either (Tokens s) a]
    --- -> m [Either s a]
sepCap = undefined

findAll
    --- :: forall e s m a. (MonadParsec e s m)
    :: MonadParsec e s m
    -- :: (MonadParsec e s m, Tokens s ~ s)
    => m a
    -> m [Either (Tokens s) (Tokens s, a)]
    --- -> m [Either s (s, a)]
findAll sep = sepCap (match sep)

-- | Stream editor. Also can be considered "find-and-replace". Finds all
-- of the sections of the stream which match the pattern `sep`, and replaces
-- them with the result of the `editor` function.
streamEdit
    :: (MonadParsec e s m)
    => m a
        -- ^ The parser `sep` for the pattern of interest.
    -> (a -> Tokens s -> Tokens s)
        -- ^ The `editor` function. Takes a parsed result of `sep`, and
        -- the section of the stream which was matched by the pattern `sep`,
        -- and returns a new stream section for the replacement.
    -> Tokens s
    -> Tokens s
streamEdit sep editor input = undefined
--     foldMap (either id (uncurry $ flip editor))
--         $ fromMaybe input $ parseMaybe (findAll sep) input

--        print $ foldMap (either id (\(_,(m,e)) -> show $ m * (10 ^^ e)))
--              $ fromJust $ parseMaybe (findall scinum) input

-- | Same as `streamEdit`, but the `editor` function is in a monadic context.
-- This allows the `editor` function to perform, for example, IO.
streamEditT
    :: forall e s m a. (Ord e, Stream s, Monad m, Monoid s, Tokens s ~ s)
    => ParsecT e s m a
        -- ^ The parser `sep` for the pattern of interest.
    -> (a -> s -> m s)
        -- ^ The `editor` function. Takes a parsed result of `sep`, and
        -- the section of the stream which was matched by the pattern `sep`,
        -- and returns a new stream section for the replacement.
    -> s
    -> m s
streamEditT sep editor input = do
    runParserT (findAll sep) "" input >>= \case
        (Left _) -> return input
        (Right r) -> fmap fold $ traverse (either return (uncurry $ flip editor)) r

--- instance SemiGroup (Tokens String) where

-- this is actually the best solution
-- let sed :: Parser a -> Parser [Either String (String, a)]
-- -- let sed :: Parser a -> Parser [Either String a]
--     sed p = do
--         many $ fmap Right (try $ match p) <|> fmap (Left . return) anySingle
