module HelVM.HelIO.SwitchEnum where

import           HelVM.HelIO.Extra

import qualified Relude.Extra      as Extra

unsafeEnum :: (Bounded e , Enum e) => Int -> e
unsafeEnum = tee fromJustWith Extra.safeToEnum

enumFromBool :: (Bounded e , Enum e) => Bool -> e
enumFromBool = unsafeEnum . fromEnum

defaultEnum :: (Bounded e , Enum e) => e
defaultEnum = enumFromBool False

bothEnums :: (Bounded e , Enum e) => [e]
bothEnums = enumFromBool <$> [False , True]
