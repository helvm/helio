module HelVM.HelIO.SwitchEnum where

import           HelVM.HelIO.Extra

import qualified Relude.Extra      as Extra

bothEnums :: (Bounded e , Enum e) => [e]
bothEnums = enumFromBool <$> [False , True]

enumFromBool :: (Bounded e , Enum e) => Bool -> e
enumFromBool = unsafeEnum . fromEnum

generateEnums :: (Bounded e , Enum e) => Int -> [e]
generateEnums i = unsafeEnum <$> [0 .. i]

defaultEnum :: (Bounded e , Enum e) => e
defaultEnum = unsafeEnum 0

unsafeEnum :: (Bounded e , Enum e) => Int -> e
unsafeEnum = tee fromJustWith Extra.safeToEnum
