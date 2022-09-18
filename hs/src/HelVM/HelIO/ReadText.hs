module HelVM.HelIO.ReadText (
  readTextUnsafe,
  readTextSafe,
  readTextMaybe,
  readUnsafe,
  readSafe,
) where

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra

readTextUnsafe :: Read a => Text -> a
readTextUnsafe = unsafe . readTextSafe

readTextSafe :: (MonadSafe m , Read a) => Text -> m a
readTextSafe = tee appendError (readSafeWithoutError . toString)

readTextMaybe :: Read a => Text -> Maybe a
readTextMaybe = readMaybe . toString

readUnsafe :: Read a => String -> a
readUnsafe = unsafe . readSafe

readSafe :: (MonadSafe m , Read a) => String -> m a
readSafe = tee (appendError . toText) readSafeWithoutError

readSafeWithoutError :: (MonadSafe m , Read a) => String -> m a
readSafeWithoutError = liftEitherError . readEither
