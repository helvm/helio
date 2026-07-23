module HelVM.HelIO.Control.Logging (
  runNoLoggingT,

  logMessageTupleList,
  logMessageTuple,
  logData,
  logMessage,
  logMessages,

  MonadLogger,
  LoggingT,
) where

import           HelVM.HelIO.Control.Message

import           Control.Monad.Logger

logMessageTupleList :: MonadLogger m => [MessageTuple] -> m ()
logMessageTupleList = mapM_ logMessageTuple

logMessageTuple :: MonadLogger m => MessageTuple -> m ()
logMessageTuple = logMessage . logTupleToMessage

logTupleToMessage :: MessageTuple -> Text
logTupleToMessage (k, v) = k <> ": " <> v

logData :: (MonadLogger m, Show a) => a -> m ()
logData = logMessage . show

logMessage :: MonadLogger m => Text -> m ()
logMessage = logInfoN

logMessages :: (MonadLogger m, Foldable f) => f Text -> m ()
logMessages = mapM_ logMessage
