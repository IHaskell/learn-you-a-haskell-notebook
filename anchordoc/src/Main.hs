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
import Replace.Megaparsec

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

        -- cases that don't work:
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

-- TODO: We need to parse only markdown cells
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

