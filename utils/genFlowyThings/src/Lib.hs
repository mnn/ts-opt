{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Bool (bool)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
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

--  <R>(f1: (_: Opt<T>) => R): R;
--  <A1, R>(f1: (_: Opt<T>) => A1, f2: (_: A1) => R): R;

genPipeFn :: Bool -> Int -> Text
genPipeFn inCls n = [header, cases, footer] & T.intercalate "\n"
  where
    header =
      let innerPart :: Text = bool "" "InClass" inCls
          typeArg :: Text = bool "" "<T>" inCls
       in [qq|interface Pipe{innerPart}Fn$typeArg \{|]
    footer = "}"
    cases = [1 .. n] <&> mkCase <&> ("  " <>) & T.intercalate "\n"
    mkArgsTypes n = [1 .. n - 1] <&> (\x -> [qq|A$x|]) & (<> ["R"]) & (["Opt<T>" | inCls] <>) & (["I" | not inCls] <>) :: [Text]
    mkCase i =
      let fParts = [1 .. i] <&> mkCaseFn i & T.intercalate ", "
          prefix = "<" <> T.intercalate ", " (mkArgsTypes i & bool id tail inCls) <> ">"
          firstArg = bool "x: I, " "" inCls :: Text
       in [qq|$prefix($firstArg$fParts): R;|]
    mkCaseFn n j =
      let argNames = mkArgsTypes n
          args = argNames & windowed 2 & (\x -> atDef ["?", "??"] x (j - 1))
       in [qq|f$j: (_: {args !! 0}) => {args !! 1}|]
