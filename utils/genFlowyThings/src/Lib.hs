{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Bool (bool)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Safe (atDef, atMay)
import Text.InterpolatedString.Perl6 (qq)

windowed :: Int -> [a] -> [[a]]
windowed _ [] = []
windowed size ls@(x : xs) =
  if length ls >= size
    then take size ls : windowed size xs
    else windowed size xs

mkArgsTypes :: Maybe Text -> Int -> [Text]
mkArgsTypes firstArg n = [1 .. n - 1] <&> (\x -> [qq|A$x|]) & (<> ["R"]) & (maybeToList firstArg <>)

--  <R>(f1: (_: Opt<T>) => R): R;
--  <A1, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => R): R;

joinNl :: [Text] -> Text
joinNl = T.intercalate "\n"

joinComma :: [Text] -> Text
joinComma = T.intercalate ", "

genInterface :: Text -> Bool -> [Text] -> Text
genInterface name hasT cases = [header, cases <&> ("  " <>) & joinNl, footer] & joinNl
  where
    tPart = bool "" "<T>" hasT :: Text
    header = [qq|interface $name$tPart \{|]
    footer = "}"

mkCaseFn :: (Int -> [Text]) -> Int -> Int -> Text
mkCaseFn mkArgsTypes' n j =
  let argNames = mkArgsTypes' n
      args = argNames & windowed 2 & (\x -> atDef ["?", "??"] x (j - 1))
   in [qq|f$j: (_: {args !! 0}) => {args !! 1}|]

genPipeFn :: Bool -> Int -> Text
genPipeFn inCls n = genInterface header inCls cases
  where
    header = "Pipe" <> bool "" "InClass" inCls <> "Fn"
    cases = [1 .. n] <&> mkCase
    mkArgsTypes' = mkArgsTypes (Just $ bool "I" "Opt<T>" inCls)
    mkCase i =
      let fParts = [1 .. i] <&> mkCaseFn mkArgsTypes' i & joinComma
          prefix = "<" <> joinComma (mkArgsTypes' i & bool id tail inCls) <> ">"
          firstArg = bool "x: I, " "" inCls :: Text
       in [qq|$prefix($firstArg$fParts): R;|]

genMapFlow :: Bool -> Int -> Text
genMapFlow inCls n = genInterface header inCls cases
  where
    header = "MapFlow" <> bool "" "InClass" inCls <> "Fn"
    cases = [1 .. n] <&> mkCase
    mkArgsTypes' = mkArgsTypes (Just $ bool "I" "T" inCls)
    mkCase i =
      let fParts = [1 .. i] <&> mkCaseFn mkArgsTypes' i & joinComma
          prefix = "<" <> joinComma (mkArgsTypes' i & bool id tail inCls) <> ">"
          firstArg = bool "x: Opt<I>, " "" inCls :: Text
       in [qq|$prefix($firstArg$fParts): Opt<R>;|]
