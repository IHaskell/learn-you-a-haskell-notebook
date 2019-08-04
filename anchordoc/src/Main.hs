{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

-- https://stackoverflow.com/questions/18957873/haskell-parenthesis-matching-for-find-and-replace
-- import Text.ParserCombinators.Parsec

import Network.Wreq (get, responseBody)
import Control.Lens
import Data.Aeson.Lens
import Data.Text (unpack)

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Proxy

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
findall :: forall e s m a. (MonadParsec e s m, Semigroup s) => m a -> m [Either (Tokens s) (Tokens s, a)]
-- findall :: forall e s m a. MonadParsec e s m => m a -> m [Either (Tokens s) (Tokens s, a)]
-- findall :: forall a. ParsecT e s m a -> ParsecT e s m [Either String (String, a)]
findall pattern = do
   st0 <- getParserState
   -- (st', caps) <- loop st0
   (st', caps) <- runParserT'
   updateParserState (const st') -- Update state so Parser has consumed all input.
   return caps
  where
    -- take a parser state and build up a list of capture results
    -- loop :: State s -> m (State s, [(Either (Tokens s) (Tokens s, a))])
    loop st =
        --- case (runParserT' (match pattern) st) of
        (runParserT' (match pattern) st) >>= \case
            (_, Left _) ->
                -- No match, so add the first character to the Left non-matching
                -- results and then step forward and loop.
                -- TODO use `token` instead of `anySingle`
                (runParserT' anySingle st) >>= \case
                    (stIncrem, Left _) ->
                        -- Failed to get a single char, so end of input stream.
                        return (stIncrem, [])
                    (stIncrem, Right oneChar) ->
                        -- now collect the oneChar and loop with stIncrem
                        (loop stIncrem) >>= \case
                            (st', ((Left cap):caps)) ->
                                return (st', (Left (tokenToChunk (Proxy::Proxy s) oneChar <> cap)):caps)
                            (st', caps) ->
                                return (st', (Left (tokenToChunk (Proxy::Proxy s) oneChar)):caps)
                        --TODO generalized Token cons instead of tokenToChunk?
            (stMatch, Right mach) ->
                -- Found a match, add to results and loop.
                (fmap.fmap) ((Right mach):) (loop stMatch)

