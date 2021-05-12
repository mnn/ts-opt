{-# OPTIONS_GHC -F -pgmF htfpp -Wno-unused-top-binds #-}

import {-@ HTF_TESTS @-} LibSpec

import Test.Framework

main :: IO ()
main = htfMain htf_importedTests
