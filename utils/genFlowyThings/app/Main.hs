module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib

main :: IO ()
main = do
  TIO.putStrLn $ genPipeFn True 10
  TIO.putStrLn $ genPipeFn False 10
