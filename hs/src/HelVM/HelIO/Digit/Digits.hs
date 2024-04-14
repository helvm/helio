module HelVM.HelIO.Digit.Digits (
  digitsToIntegral,
  naturalToDigits7,
  naturalToDigits2,
  naturalToDigits,
) where

import qualified Data.ListLike                 as LL
import           HelVM.HelIO.Collections.SList
import           HelVM.HelIO.Control.Safe

digitsToIntegral :: (MonadSafe m , Integral a) => a -> SList (m a) -> m a
digitsToIntegral base = foldr (liftedMulAndAdd base) (pure 0)

liftedMulAndAdd :: (MonadSafe m , Integral a)  => a -> m a -> m a -> m a
liftedMulAndAdd base = liftA2 (mulAndAdd base)

mulAndAdd :: Integral a => a -> a -> a -> a
mulAndAdd base digit acc = acc * base + digit

----

naturalToDigits7 :: Natural -> [Natural]
naturalToDigits7 = naturalToDigits 7

naturalToDigits2 :: Natural -> [Natural]
naturalToDigits2 = naturalToDigits 2

naturalToDigits :: Natural -> Natural -> [Natural]
naturalToDigits base = LL.reverse <$> unfoldr (modDivMaybe base)

modDivMaybe :: Natural -> Natural -> Maybe (Natural , Natural)
modDivMaybe _    0     = Nothing
modDivMaybe base value = Just (swap $ value `divMod` base)
