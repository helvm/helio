module HelVM.HelIO.Hel.Hel (
  helTToIO,
  helTToIOWithoutLogs,
  helTToIOWithLogs,
  helToIO,

  runHelT,
  runHel,

  safeWithMessagesToText,

  helT,
  hel,

  safeWithMessages,

  MonadHel,
  HelT,
  Hel,

  UnitSafeWithMessages,
  SafeWithMessages
) where

import           HelVM.HelIO.Hel.Logger
import           HelVM.HelIO.Hel.Message
import           HelVM.HelIO.Hel.Safe

import           Hel.Type.Operator

import qualified System.IO               as IO

-- | Deprecated, use Business

helTToIO :: Bool -> HelT IO a -> IO a
helTToIO False = helTToIOWithoutLogs
helTToIO True  = helTToIOWithLogs

helTToIOWithoutLogs :: HelT IO a -> IO a
helTToIOWithoutLogs = safeWithMessagesToIOWithoutLogs <=< runHelT

helTToIOWithLogs :: HelT IO a -> IO a
helTToIOWithLogs = safeWithMessagesToIOWithLogs <=< runHelT

helToIO :: Hel a -> IO a
helToIO = safeToIO <$> removeLogger

runHelT :: HelT m a -> m $ SafeWithMessages a
runHelT = runLoggerT <$> runSafeT

runHel :: Hel a -> SafeWithMessages a
runHel a = runLogger $ runSafe <$> a

safeWithMessagesToIOWithoutLogs :: SafeWithMessages a -> IO a
safeWithMessagesToIOWithoutLogs (safe , _) = safeToIO safe

safeWithMessagesToIOWithLogs :: SafeWithMessages a -> IO a
safeWithMessagesToIOWithLogs (safe , logs) = safeToIO safe <* IO.hPutStr stderr (errorsToString logs)

safeWithMessagesToText :: SafeWithMessages a -> Text
safeWithMessagesToText (safe , messages) = errorsToText messages <> safeToText safe

-- | Constructors
helT :: Monad m => m a -> HelT m a
helT = safeT <$> loggerT

hel :: a -> Hel a
hel = logger <$> pure

safeWithMessages :: a -> SafeWithMessages a
safeWithMessages = withMessages <$> pure

-- | Types
type MonadHel m = (MonadLogger m, MonadSafe m)

type HelT m = SafeT (LoggerT m)

type Hel a = Logger $ Safe a

type UnitSafeWithMessages = SafeWithMessages ()

type SafeWithMessages a = WithMessages (Safe a)
