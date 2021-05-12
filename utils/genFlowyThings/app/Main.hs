module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Lib

gen :: (InterfaceInClass -> Int -> Text) -> Int -> IO ()
gen f n = do
  TIO.putStrLn $ f InterfaceIsInClass n
  putStrLn ""
  TIO.putStrLn $ f InterfaceIsNotInClass n
  putStrLn ""

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
