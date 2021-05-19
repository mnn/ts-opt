module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Lib

genInCls :: (InterfaceInClass -> Int -> Text) -> Int -> IO ()
genInCls f n = do
  TIO.putStrLn $ f InterfaceIsInClass n
  putStrLn ""

genCurried :: (InterfaceInClass -> Int -> Text) -> Int -> IO ()
genCurried f n = do
  TIO.putStrLn $ f InterfaceIsNotInClass n
  putStrLn ""

gen :: (InterfaceInClass -> Int -> Text) -> Int -> IO ()
gen f n = do
  genInCls f n
  genCurried f n

main :: IO ()
main = do
  putStrLn "// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  putStrLn "// Don't modify manually, generated via utils/genFlowyThings  !!"
  putStrLn "// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  putStrLn ""
  putStrLn "import { Opt } from './Opt';"
  putStrLn ""
  gen genPipe 10
  gen genMapFlow 10
  gen genAct 10
  gen genActToOpt 10
  genCurried genFlow 10
  genCurried genCompose 10
