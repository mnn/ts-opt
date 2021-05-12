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

nl :: Text
nl = "\n"

joinNl :: [Text] -> Text
joinNl = T.intercalate nl

joinComma :: [Text] -> Text
joinComma = T.intercalate ", "

mkInterfaceName :: Bool -> Text -> Text
mkInterfaceName inCls prefix = prefix <> bool "" "InClass" inCls <> "Fn"

data InterfaceHasTypeParam = InterfaceWithoutT | InterfaceWithT deriving (Eq, Show)

data InterfaceInClass = InterfaceIsInClass | InterfaceIsNotInClass deriving (Eq, Show)

genInterface :: Text -> InterfaceHasTypeParam -> InterfaceInClass -> Int -> (Int -> Text) -> Text
genInterface name hasTypeParam inCls casesCount caseMaker =
  [header, [1 .. casesCount] <&> caseMaker <&> ("  " <>) & joinNl, footer] & joinNl
  where
    tPart = bool "" "<T>" (hasTypeParam == InterfaceWithT) :: Text
    fullName = mkInterfaceName (inCls == InterfaceIsInClass) name
    header = [qq|export interface {fullName}$tPart \{|]
    footer = "}"

wrapInOpt :: Text -> Text
wrapInOpt x = [qq|Opt<$x>|]

wrapInNullable :: Text -> Text
wrapInNullable x = [qq|$x | undefined | null|]

data ResGenType = ResIsRaw | ResIsOpt | ResIsNullable

mkCaseFn :: (Int -> [Text]) -> ResGenType -> Int -> Int -> Text
mkCaseFn mkArgsTypes' resGenType n j =
  let argNames = mkArgsTypes' n
      args = argNames & windowed 2 & (\x -> atDef ["?", "??"] x (j - 1))
      processRes ResIsRaw = id
      processRes ResIsOpt = wrapInOpt
      processRes ResIsNullable = wrapInNullable
   in [qq|f$j: (_: {args !! 0}) => {processRes resGenType $ args !! 1}|]

renderFunction :: [Text] -> [Text] -> Text -> Text
renderFunction typeArgs args ret = [qq|$rTypeArgs($rArgs): $ret|]
  where
    rTypeArgs = bool ("<" <> joinComma typeArgs <> ">") "" (null typeArgs)
    rArgs = joinComma args

intWithTtoHasTypeParam :: InterfaceInClass -> InterfaceHasTypeParam
intWithTtoHasTypeParam InterfaceIsInClass = InterfaceWithT
intWithTtoHasTypeParam InterfaceIsNotInClass = InterfaceWithoutT

isInClass :: InterfaceInClass -> Bool
isInClass InterfaceIsInClass = True
isInClass InterfaceIsNotInClass = False

genPipe :: InterfaceInClass -> Int -> Text
genPipe inCls n = genInterface "Pipe" (intWithTtoHasTypeParam inCls) inCls n mkCase
  where
    isInClass' = isInClass inCls
    mkArgsTypes' = mkArgsTypes (Just $ bool "I" "Opt<T>" isInClass')
    mkCase i =
      let fParts = [1 .. i] <&> mkCaseFn mkArgsTypes' ResIsRaw i & joinComma
          prefix = "<" <> joinComma (mkArgsTypes' i & bool id tail isInClass') <> ">"
          firstArg = bool "x: I, " "" isInClass' :: Text
       in [qq|$prefix($firstArg$fParts): R;|]

genMapFlow :: InterfaceInClass -> Int -> Text
genMapFlow inCls n = genInterface "MapFlow" (intWithTtoHasTypeParam inCls) inCls n mkCase
  where
    isInClass' = isInClass inCls
    mkArgsTypes' = mkArgsTypes (Just $ bool "I" "T" isInClass')
    mkCase i =
      let fParts = [1 .. i] <&> mkCaseFn mkArgsTypes' ResIsRaw i & joinComma
          prefix = "<" <> joinComma (mkArgsTypes' i & bool id tail isInClass') <> ">"
          retType :: Text = bool "(x: Opt<I>) => " "" isInClass' <> "Opt<R>"
       in [qq|$prefix($fParts): $retType;|]

genAct :: InterfaceInClass -> Int -> Text
genAct inCls n = genInterface "Act" (intWithTtoHasTypeParam inCls) inCls n mkCase
  where
    isInClass' = isInClass inCls
    mkArgsTypes' = mkArgsTypes (Just $ bool "I" "T" isInClass')
    mkCase i =
      let fParts = [1 .. i] <&> mkCaseFn mkArgsTypes' ResIsOpt i & joinComma
          prefix = "<" <> joinComma (mkArgsTypes' i & bool id tail isInClass') <> ">"
          retType :: Text = bool "(x: Opt<I>) => " "" isInClass' <> "Opt<R>"
       in [qq|$prefix($fParts): $retType;|]

genActToOpt :: InterfaceInClass -> Int -> Text
genActToOpt inCls n = genInterface "ActToOpt" (intWithTtoHasTypeParam inCls) inCls n mkCase
  where
    isInClass' = isInClass inCls
    mkArgsTypes' = mkArgsTypes (Just $ bool "I" "T" isInClass')
    mkCase i =
      let fParts = [1 .. i] <&> mkCaseFn mkArgsTypes' ResIsNullable i & joinComma
          prefix = "<" <> joinComma (mkArgsTypes' i & bool id tail isInClass') <> ">"
          retType :: Text = bool "(x: Opt<I>) => " "" isInClass' <> "Opt<R>"
       in [qq|$prefix($fParts): $retType;|]
