module HelVM.HelIO.Control.Business (
  businessTToIO,
  businessTToIOWithoutLogs,
  businessTToIOWithLogs,
  businessToIO,

  runBusinessT,
  runBusiness,

  safeWithMessagesToText,

  businessT,
  business,

  safeWithMessages,

  MonadBusiness,
  BusinessT,
  Business,

  UnitSafeWithMessages,
  SafeWithMessages
) where

import           HelVM.HelIO.Control.Logger
import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import qualified System.IO                   as IO

businessTToIO :: Bool -> BusinessT IO a -> IO a
businessTToIO False = businessTToIOWithoutLogs
businessTToIO True  = businessTToIOWithLogs

businessTToIOWithoutLogs :: BusinessT IO a -> IO a
businessTToIOWithoutLogs = safeWithMessagesToIOWithoutLogs <=< runBusinessT

businessTToIOWithLogs :: BusinessT IO a -> IO a
businessTToIOWithLogs = safeWithMessagesToIOWithLogs <=< runBusinessT

businessToIO :: Business a -> IO a
businessToIO = safeToIO . removeLogger

runBusinessT :: BusinessT m a -> m $ SafeWithMessages a
runBusinessT = runLoggerT . runSafeT

runBusiness :: Business a -> SafeWithMessages a
runBusiness a = runLogger $ runSafe <$> a

safeWithMessagesToIOWithoutLogs :: SafeWithMessages a -> IO a
safeWithMessagesToIOWithoutLogs (safe , _) = safeToIO safe

safeWithMessagesToIOWithLogs :: SafeWithMessages a -> IO a
safeWithMessagesToIOWithLogs (safe , logs) = safeToIO safe <* IO.hPutStr stderr (errorsToString logs)

safeWithMessagesToText :: SafeWithMessages a -> Text
safeWithMessagesToText (safe , messages) = errorsToText messages <> safeToText safe

-- | Constructors
businessT :: Monad m => m a -> BusinessT m a
businessT = safeT . loggerT

business :: a -> Business a
business = logger . pure

safeWithMessages :: a -> SafeWithMessages a
safeWithMessages = withMessages . pure

-- | Types
type MonadBusiness m = (MonadLogger m , MonadSafe m)

type BusinessT m = SafeT (LoggerT m)

type Business a = Logger $ Safe a

type UnitSafeWithMessages = SafeWithMessages ()

type SafeWithMessages a = WithMessages (Safe a)
