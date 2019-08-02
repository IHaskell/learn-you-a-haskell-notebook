{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- https://stackoverflow.com/questions/18957873/haskell-parenthesis-matching-for-find-and-replace
-- import Text.ParserCombinators.Parsec

import Network.Wreq
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

-- main :: IO ()
-- main = do
--   let input = "`==` `Eq` ` __`Ord`__ `T.D` `T dd` "
--   let ss = split (oneOf "`") input
--   print ss
--   print =<< replace ss

--- replace' :: [String] -> IO String
--- --- replace [s1] = [s1]
--- --- replace [s1,s2] = [s1,s2]
--- replace' ("`":token:"`":ss) =
---   if (all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ".<>+=-$/:'")) token)
---     then do
---       -- https://github.com/ndmitchell/hoogle/blob/master/docs/API.md#json-api
---       --
---       r <- get $ "https://hoogle.haskell.org?mode=json&hoogle=" ++ token ++ "&start=1&count=1"
---       case (r ^.. responseBody . nth 0 . key "url" . _String) of
---         [hoogleurl] -> (("[`" ++ token ++ "`](" ++ unpack hoogleurl ++ ")") ++) <$> replace ss
---         _ -> (("`" ++ token) ++) <$> replace ("`" : ss)
---       --- (("[`" ++ token ++ "`](http:hackage.haskell.org)") ++) <$> replace ss
---     else (("`" ++ token) ++) <$> replace ("`" : ss)
--- replace' (x:xs) = (x ++) <$> replace xs
--- replace' [] = return ""

main :: IO ()
main = do
    -- input <- getContents
    -- let input = "`==` `Eq` ` __`Ord`__ `T.D` `T dd` "
    let input = "  `==` `Eq` ` ` __`Ord`__ `T.D` `T dd` "
    case (parse (capture' backtickSymbol) "" input) of
        Left err -> print err
        Right groups -> do
            print groups
            putStr =<< fmap join (mapM replace groups)

replace :: Either String (String, (String, String, String)) -> IO String
replace (Left cap) = return cap
replace (Right (cap, (tickOpen, symbol, tickClose))) =
    --- runMaybeT $ fromMaybe cap $ do
    -- If anything doesn't work out then just use the original capture `cap`.
    fmap (fromMaybe cap) $ runMaybeT $ do
        -- Bail out if this doesn't look like a legitimate Haskell token
        -- guard $ all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ".<>+=-$/:'")) token
        -- MaybeT $ return $ guard $ flip all token $ flip elem $ ['a'..'z'] ++ ['A'..'Z'] ++ ".<>+=-$/:'"

        -- Query hoogle for the symbol, only in the base package, only 1 result.
        hoogleResult <- lift $ get $  "https://hoogle.haskell.org?mode=json&hoogle="
                            ++ symbol
                            ++ "&scope=package%3Abase&start=1&count=1"

        -- If hoogle returns a documentation URL
        docUrl <- MaybeT $ return $ listToMaybe $ hoogleResult ^.. responseBody . nth 0 . key "url" . _String

        -- Construct a Markdown link with the documentation URL
        MaybeT $ return $ Just $ "[" ++ tickOpen ++ symbol ++ tickClose ++ "](" ++ unpack docUrl ++ ")"

type Parser = Parsec Void String


backtickSymbol :: Parser (String, String, String)
backtickSymbol = do
    tickOpen  <- chunk "`"
    --- token     <- Text.Megaparsec.some $ Text.Megaparsec.noneOf ['`',' ']
    symbol     <- Text.Megaparsec.some $ Text.Megaparsec.oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ".<>+=-$/:'" :: String)
    tickClose <- chunk "`"
    return (tickOpen, symbol, tickClose)

capture :: Parser a -> Parser [Either String (String, a)]
capture group = do
    initmatch <- fmap join $ many $ try $ do
        (nomatch, yesmatch) <- manyTill_ anySingle (match group)
        case nomatch of
          [] -> return [Right yesmatch]
          _  -> return [Left nomatch, Right yesmatch]
    tailmatch <- many anySingle
    case tailmatch of
      [] -> return initmatch
      _ -> return $ initmatch <> [Left tailmatch]
  where
    -- https://hackage.haskell.org/package/parser-combinators-1.2.0/docs/src/Control.Monad.Combinators.html#manyTill_
    manyTill_ :: MonadPlus m => m a -> m end -> m ([a], end)
    manyTill_ p end = go id
      where
        go f = do
          done <- optional end
          case done of
            Just done' -> return (f [], done')
            Nothing  -> do
              x <- p
              go (f . (x:))


capture' :: forall a. Parser a -> Parser [Either String (String, a)]
capture' group = do
   st0 <- getParserState
   return $ loop st0
  where
    loop :: State String -> [(Either String (String, a))]
    loop st =
       let (stNew, result) = runParser' (match group) st
       in
       case result of
            Left _ ->
                let (stIncrem, resultIncrem) = runParser' (anySingle :: Parser Char) st
                in
                case resultIncrem of
                    Left _ -> []-- Can't get a single char, so end of input
                    Right nonMatchChar -> -- now collect nonMatchChar and loop with stIncrem
                      case (loop stIncrem) of
                        ((Left s):ss) -> (Left (nonMatchChar:s)):ss
                        ss -> (Left (nonMatchChar:[])):ss
            Right mach -> -- now collect the mach and loop with stNew
                (Right mach):(loop stNew)

