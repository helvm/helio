module HelVM.HelIO.IO.BIO where

import           HelVM.HelIO.Control.Business
import           HelVM.HelIO.IO.Console
import           HelVM.HelIO.IO.FileReader

type BIO m = (MonadBusiness m , ConsoleIO m , FileReaderIO m)
