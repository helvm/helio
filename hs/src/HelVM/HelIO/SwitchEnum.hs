module HelVM.HelIO.SwitchEnum where

import qualified Relude.Extra as Extra

unsafeEnum :: (Bounded e , Enum e) => Int -> e
unsafeEnum i = (unsafe . Extra.safeToEnum) i where
  unsafe Nothing  = error $ show i
  unsafe (Just a) = a

enumFromBool :: (Bounded e , Enum e) => Bool -> e
enumFromBool = unsafeEnum . fromEnum

defaultEnum :: (Bounded e , Enum e) => e
defaultEnum = enumFromBool False

bothEnums :: (Bounded e , Enum e) => [e]
bothEnums = enumFromBool <$> [False , True]
