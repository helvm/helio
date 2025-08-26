module Main where

import qualified Spec

--import           Test.Hspec      (hspec)
--import           Test.Hspec.Slow

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  hspecTests <- testSpec "Hspec tests" Spec.spec
  defaultMain (testGroup "All tests" [hspecTests])

--main = (hspec . flip timeThese Spec.spec) =<< configure 1
