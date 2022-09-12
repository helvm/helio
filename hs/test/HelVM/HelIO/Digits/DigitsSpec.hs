module HelVM.HelIO.Digits.DigitsSpec (spec) where

import           HelVM.HelIO.Digit.Digits

import           Test.Hspec                     (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "naturalToDigits" $
    forM_ [ (0 , [] , [])
          , (1 , [1] , [1])
          , (2 , [2] , [1,0])
          , (4 , [4] , [1,0,0])
          , (7 , [1,0] , [1,1,1])
          , (8 , [1,1] , [1,0,0,0])
          , (16 , [2,2] , [1,0,0,0,0])
          ] $ \(input , output7, output2) ->
      describe (show input) $ do
        it "7" $ naturalToDigits7 (input :: Natural) `shouldBe` (output7 :: [Natural])
        it "2" $ naturalToDigits2 (input :: Natural) `shouldBe` (output2 :: [Natural])
