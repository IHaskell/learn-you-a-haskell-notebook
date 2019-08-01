{-# LANGUAGE OverloadedStrings #-}

module Main where

-- https://stackoverflow.com/questions/18957873/haskell-parenthesis-matching-for-find-and-replace
-- import Text.ParserCombinators.Parsec

-- http://hackage.haskell.org/package/split-0.2.3.3/docs/Data-List-Split.html
import Data.List.Split
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Text (unpack)

-- codeToken :: GenParser Char st String
-- codeToken :: Parser String
-- codeToken = do
--     em <- string "**" <|> string "*" <|> string "__" <|> string "_"
--     string "`"
--     tokencandidate <- many (oneOf ['a'..'z'] ++ ['A'..'Z'] ++ ".")
--     string "`"
--     string em
--     return (em, tokencandidate)

-- parseAll :: Parser [Either String String] -- Left is not token, Right is candidate token
-- parseAll = do
--     many $ do
--         between <- many (noneOf "`"))
--         token <- try codeToken

main :: IO ()
main = do
  -- input <- getContents
  let input = "`tk` `tk2` ` __`TT`__ `T.D` `T dd` "
  let ss = split (oneOf "`") input
  -- case (parse codeToken "" input) of
  --   Left err -> print err
  --   Right t -> print t
  print ss
  print =<< replace ss

replace :: [String] -> IO String
--- replace [s1] = [s1]
--- replace [s1,s2] = [s1,s2]
replace ("`":token:"`":ss) =
  if (all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ".")) token)
    then do
      -- https://github.com/ndmitchell/hoogle/blob/master/docs/API.md#json-api
      --
      r <- get $ "https://hoogle.haskell.org?mode=json&hoogle=" ++ token ++ "&start=1&count=1"
      case (r ^.. responseBody . nth 0 . key "url" . _String) of
        [hoogleurl] -> (("[`" ++ token ++ "`](" ++ unpack hoogleurl ++ ")") ++) <$> replace ss
        _ -> (("`" ++ token) ++) <$> replace ("`" : ss)
      --- (("[`" ++ token ++ "`](http:hackage.haskell.org)") ++) <$> replace ss
    else (("`" ++ token) ++) <$> replace ("`" : ss)
replace (x:xs) = (x ++) <$> replace xs
replace [] = return ""
