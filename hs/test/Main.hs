module Main where

import qualified Spec
import           Test.Hspec      (hspec)
import           Test.Hspec.Slow

main :: IO ()
main = (build <=< configure) 1 where
  build config = hspec $ timeThese config Spec.spec
