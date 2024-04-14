module HelVM.HelIO.IO.FileReader (
  FileReaderIO,
  wReadFile,
) where

import           HelVM.HelIO.Control.Business
import           HelVM.HelIO.Control.Logger
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

class Monad m => FileReaderIO m where
  wReadFile :: FilePath -> m Text

instance FileReaderIO (BusinessT IO) where
  wReadFile = businessT <$> readFileTextUtf8

instance FileReaderIO (LoggerT IO) where
  wReadFile = loggerT <$> readFileTextUtf8

instance FileReaderIO (SafeT IO) where
  wReadFile = safeT <$> readFileTextUtf8

instance FileReaderIO IO where
  wReadFile = readFileTextUtf8



