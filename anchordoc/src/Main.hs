{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Main where


import Network.Wreq (getWith, responseBody, param, defaults)
import Control.Lens ((^..), (&), (.~))
import Data.Aeson.Lens
import Data.Text (unpack, pack)
import Text.Megaparsec

import Data.Void
import Data.Maybe
import Data.Bifunctor
import Data.Functor.Identity
import Data.Proxy
import Data.Foldable
import Control.Exception (throw)
import Data.Typeable
import Control.Monad

type Parser = Parsec Void String

main :: IO ()
main = do
    getContents >>= streamEditT pattern hoogleReplace >>= putStr
  where
    pattern = match $ eitherP doubleBacktickMask backtickSymbol
    hoogleReplace (orig, Left _) = return orig -- masked by doubleBacktickMask
    hoogleReplace (orig, Right (_, "=", _)) = return orig -- not acually a symbol
    hoogleReplace (orig, Right (tickOpen, symbol, tickClose)) = do

        --cases that don't work:
        -- infix backtick functions like `elem` in code blocks. we're going to
        -- need to only search in "cell-type":"markdown".

        -- https://github.com/ndmitchell/hoogle/blob/master/docs/API.md#json-api
        hoogleResult <- flip getWith
                            "https://hoogle.haskell.org"
                            $ defaults
                            & param "mode"   .~ ["json"]
                            & param "hoogle" .~ [pack symbol] -- will be url encoded
                            & param "scope"  .~ ["package:base"]
                            & param "start"  .~ ["1"]
                            & param "count"  .~ ["1"]


        let hoogleReturnItem = fmap unpack $ listToMaybe $ hoogleResult ^.. responseBody . nth 0 . key "item" . _String

        -- If hoogle returns a documentation URL, and the same symbol
        -- that was queried, then substitute a markdown link
        case
            ( maybe False (==symbol) $ parseMaybe (hoogleReturnItemSymbol :: Parser String) =<< hoogleReturnItem
            , listToMaybe $ hoogleResult ^.. responseBody . nth 0 . key "url" . _String
            ) of
            (True, Just docUrl) ->
                -- Construct a Markdown link with the documentation URL
                return
                    $  "["
                    ++ tickOpen
                    ++ symbol
                    ++ tickClose
                    ++ "]("
                    ++ unpack docUrl
                    ++ ")"
            _ -> return orig

    hoogleReturnItemSymbol :: Parser String
    hoogleReturnItemSymbol = do
        void $ manyTill anySingle $ chunk "<s0>"
        s <- fmap (tokensToChunk (Proxy::Proxy String))
             $ someTill anySingle $ chunk "</s0>"
        void $ takeRest -- must consume all input for parseMaybe success
        return s

    -- exclude special Markdown backtick escape like ``92 `div` 10``.
    doubleBacktickMask = do
        void $ chunk "``"
        void $ manyTill anySingle $ chunk "``"
        -- we have to succeed and return something or 'sepCap' will backtrack


    -- Parse something that looks like a symbol from Prelude in backticks.
    -- backtickSymbol :: Parser (String, String, String)
    backtickSymbol = do
        tickOpen  <- chunk "`"
        symbol     <- Text.Megaparsec.some $ Text.Megaparsec.oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "|&?%^*#~.<>+=-$/:'" :: String)
        tickClose <- chunk "`"
        return (tickOpen, symbol, tickClose)


-- We need to parse only markdown cells
--  {
--    "cell_type": "markdown",
--    "metadata": {},
--    "source": [
--     "__`repeat`__ takes an element and produces an infinite list of just that\n",
--     "element. It's like cycling a list with only one element."
--    ]
--  },
    -- markdownCell :: Parser ()
    -- markdownCell = do
    --     chunk "{"
    --     space
    --     chunk "\"cell_type\": \"markdown\","
    --     space
    --     chunk "\"metadata\": {},"
    --     space
    --     chunk "\"source\": ["
    --     space
    --     chunk "]" >> newline
    --     return ()


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

-- StreamEdit.Megaparsec
--
--
-- two new functions:
--

-- | Separate and Capture
--
-- Parser combinator to separate a stream into sections which match a pattern
-- in 'Right', and non-matching sections in 'Left'. Capture both the
-- matching and non-matching sections.
--
-- This parser will always consume its entire input and can never fail.
-- If there are no matching patterns, then the entire input stream is returned
-- as a non-matching section.
sepCap
    :: forall e s m a. (MonadParsec e s m)
    => m a
    -> m [Either (Tokens s) a]
sepCap sep = (fmap.fmap) (first $ tokensToChunk (Proxy::Proxy s))
             $ fmap sequenceLeft
             $ many $ fmap Right (try sep) <|> fmap Left anySingle
  where
    sequenceLeft :: [Either l r] -> [Either [l] r]
    sequenceLeft = foldr consLeft []
      where
        consLeft :: Either l r -> [Either [l] r] -> [Either [l] r]
        consLeft (Left l) ((Left ls):xs) = (Left (l:ls)):xs
        consLeft (Left l) xs = (Left [l]):xs
        consLeft (Right r) xs = (Right r):xs

findAll
    :: MonadParsec e s m
    => m a
    -> m [Either (Tokens s) (Tokens s, a)]
findAll sep = sepCap (match sep)

-- | Stream editor. Pure version of 'streamEditT'.
streamEdit
    :: forall s a. (Stream s, Monoid s, Tokens s ~ s, Show s, Show (Token s), Typeable s)
    => Parsec Void s a
        -- ^ The parser `sep` for the pattern of interest.
    -> (a -> s)
        -- ^ The `editor` function. Takes a parsed result of `sep`
        -- and returns a new stream section for the replacement.
    -> s
    -> s
streamEdit sep editor = runIdentity . streamEditT sep (Identity . editor)

-- | Stream editor. Also can be considered "find-and-replace". Finds all
-- of the sections of the stream which match the pattern `sep`, and replaces
-- them with the result of the `editor` function.
--
-- This function is not a "parser combinator," it is more like
-- an alternate "way to run a parser", like 'Text.Megaparsec.parse'
-- or 'Text.Megaparsec.runParserT'.
--
-- This only works for `Stream s` such that `Tokens s ~ s`, because we want
-- to output the same type of stream that was input. It is true
-- that `Tokens s ~ s` for all the 'Text.Megaparsec.Stream' instances included
-- with Megaparsec:
-- Strict 'Data.Text',
-- Lazy 'Data.Text.Lazy',
-- Strict 'Data.Bytestring',
-- Lazy 'Data.Bytestring.Lazy',
-- and String.
--
-- We also need the `Monoid s` instance so that we can construct the output
-- stream.
--
-- If you want access to the matched string in the editing function,
-- then combine the pattern parser with 'Text.Megaparsec.match', like
--
-- > streamEdit (match sep) (\(matchString, a) -> return "")
--
-- If you want to do 'IO' operations in the `editor` function, then run this in
-- 'IO'.
--
-- If you want the `editor` function to remember some state, then run this in
-- a stateful 'Monad'.
--
-- Replace all carriage-return-newline instances with newline.
--
-- > streamEdit crlf (const "\n")
--
-- Replace all numbers in scientific notation with decimal notation, but
-- only if the value of the number is smaller than 20.
streamEditT
    :: forall s m a. (Stream s, Monad m, Monoid s, Tokens s ~ s, Show s, Show (Token s), Typeable s)
    => ParsecT Void s m a
        -- ^ The parser `sep` for the pattern of interest.
    -> (a -> m s)
        -- ^ The `editor` function. Takes a parsed result of `sep`
        -- and returns a new stream section for the replacement.
    -> s
    -> m s
streamEditT sep editor input = do
    runParserT (sepCap sep) "" input >>= \case
        (Left err) -> throw err -- parser should never fail, but if it does, throw
        (Right r) -> fmap fold $ traverse (either return editor) r



-- HAQ (Hypothetically Asked Questions)
--
-- Q: Is it fast?
--
-- A: Meh. (benchmark comparison to sed).


-- attoparsec has match
-- http://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-ByteString.html#v:match
--
-- attoparsec has a Monoid instance for Chunk
-- http://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-Types.html#t:Chunk
--
-- but attoparsec does not work for String. so.

-- https://stackoverflow.com/questions/18957873/haskell-parenthesis-matching-for-find-and-replace
