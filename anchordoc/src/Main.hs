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
import Text.Megaparsec as Mp
import Replace.Megaparsec

import Data.Void
import Data.Maybe
import Data.Proxy
import Control.Monad

type Parser = Parsec Void String

main :: IO ()
main = do
    getContents >>= streamEditT pattern hoogleReplace >>= putStr
  where
    pattern = match $ eitherP doubleBacktickMask backtickSymbol
    hoogleReplace (orig, Left _) = return orig -- masked by doubleBacktickMask
    hoogleReplace (orig, Right (_, _, "=", _, _)) = return orig -- not acually a symbol
    hoogleReplace (orig, Right (tickPrefix, tickOpen, symbol, tickClose, tickSuffix)) = do

        -- https://github.com/ndmitchell/hoogle/blob/master/docs/API.md#json-api
        --
        -- Query looks like this:
        -- https://hoogle.haskell.org/?mode=json&hoogle=head&scope=package:base&start=1&count=1
        --
        -- Query looks like this:
        -- https://hoogle.haskell.org/?mode=json&hoogle=%3E%3E%3D&scope=package:base&start=1&count=1

        hoogleResult <- flip getWith
                            "https://hoogle.haskell.org"
                            $ defaults
                            & param "mode"   .~ ["json"]
                            & param "hoogle" .~ [pack symbol] -- will be url encoded
                            & param "scope"  .~ ["package:base"]
                            & param "start"  .~ ["1"]
                            & param "count"  .~ ["1"]

        -- Result looks like this:
        -- [
        --   {
        --     "url": "https://hackage.haskell.org/package/base/docs/Prelude.html#v:head",
        --     "module": {
        --       "url": "https://hackage.haskell.org/package/base/docs/Prelude.html",
        --       "name": "Prelude"
        --     },
        --     "package": {
        --       "url": "https://hackage.haskell.org/package/base",
        --       "name": "base"
        --     },
        --     "item": "<span class=name><s0>head</s0></span> :: [a] -&gt; a",
        --     "type": "",
        --     "docs": "Extract the first element of a list, which must be non-empty.\n"
        --   }
        -- ]
        --
        -- Result looks like this:
        -- [
        --   {
        --     "url": "https://hackage.haskell.org/package/base/docs/Prelude.html#v:-62--62--61-",
        --     "module": {
        --       "url": "https://hackage.haskell.org/package/base/docs/Prelude.html",
        --       "name": "Prelude"
        --     },
        --     "package": {
        --       "url": "https://hackage.haskell.org/package/base",
        --       "name": "base"
        --     },
        --     "item": "<span class=name>(<s0>&gt;&gt;=</s0>)</span> :: forall a b . Monad m =&gt; m a -&gt; (a -&gt; m b) -&gt; m b",
        --     "type": "",
        --     "docs": "Sequentially compose two actions, passing any value produced by the\nfirst as an argument to the second.\n"
        --   }
        -- ]

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
                    $  tickPrefix
                    ++ "["
                    ++ tickOpen
                    ++ symbol
                    ++ tickClose
                    ++ "]("
                    ++ unpack docUrl
                    ++ ")"
                    ++ tickSuffix
            _ -> return orig

    hoogleReturnItemSymbol :: Parser String
    hoogleReturnItemSymbol = do
        void $ manyTill anySingle $ chunk "<s0>"
        s <- fmap (tokensToChunk (Proxy::Proxy String))
             $ someTill anySingle $ chunk "</s0>"
        void $ takeRest -- must consume all input for parseMaybe success
        return $ streamEdit (chunk "&lt;") (const "<")
               $ streamEdit (chunk "&gt;") (const ">")
               $ streamEdit (chunk "&amp;") (const "&") s

    -- exclude special Markdown backtick escape like ``92 `div` 10``.
    doubleBacktickMask = do
        void $ chunk "``"
        void $ manyTill anySingle $ chunk "``"
        -- we have to succeed and return something or 'sepCap' will backtrack

    -- Parse something that looks like a symbol from Prelude in backticks.
    backtickSymbol = do
        tickPrefix <- noneOf ("[" :: String)
        tickOpen   <- chunk "`"
        symbol     <- Mp.some $ Mp.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "|&?%^*#~.<>+=-$/:'"
        tickClose  <- chunk "`"
        tickSuffix <- noneOf ("]" :: String)
        return ([tickPrefix], tickOpen, symbol, tickClose, [tickSuffix])

-- TODO:
--
-- cases that don't work:
-- infix backtick functions like `elem` in code blocks. we're going to
-- need to only search in "cell-type":"markdown".
--
-- We need to parse only markdown cells, like:
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

