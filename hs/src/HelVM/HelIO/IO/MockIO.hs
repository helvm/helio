module HelVM.HelIO.IO.MockIO (
  ioExecMockIOBatch,
  ioExecMockIOWithInput,

  safeExecMockIOBatch,
  safeExecMockIOWithInput,

  execMockIOBatch,
  execMockIOWithInput,

  runMockIO,
  createMockIO,
  calculateOutput,
  calculateLogged,

  MockIO,
  MockIOData,
) where

import           HelVM.HelIO.Control.Business
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.IO.Console
import           HelVM.HelIO.IO.FileReader

import           HelVM.HelIO.ListLikeExtra

import qualified Data.ByteString.Lazy         as LBS

import           Data.Text                    as Text
import qualified Data.Text.Lazy               as LT

ioExecMockIOBatch :: BusinessT MockIO () -> IO MockIOData
ioExecMockIOBatch = ioExecMockIOWithInput ""

ioExecMockIOWithInput :: Text -> BusinessT MockIO () -> IO MockIOData
ioExecMockIOWithInput i = safeToIO . safeExecMockIOWithInput i

safeExecMockIOBatch :: BusinessT MockIO () -> Safe MockIOData
safeExecMockIOBatch = safeExecMockIOWithInput ""

safeExecMockIOWithInput :: Text -> BusinessT MockIO () -> Safe MockIOData
safeExecMockIOWithInput i = pure . runMockIO i . runBusinessT

execMockIOBatch :: MockIO () -> MockIOData
execMockIOBatch = execMockIOWithInput ""

execMockIOWithInput :: Text -> MockIO () -> MockIOData
execMockIOWithInput i a = runMockIO i $ safeWithMessages <$> a

----

runMockIO :: Text -> MockIO UnitSafeWithMessages -> MockIOData
runMockIO i mockIO = flip mockDataLogStr mockIOData $ safeWithMessagesToText s
  where (s , mockIOData) = runState mockIO $ createMockIO i

createMockIO :: Text -> MockIOData
createMockIO i = MockIOData (toString i) "" ""

calculateOutput :: MockIOData -> Text
calculateOutput = calculateText . output

calculateLogged :: MockIOData -> Text
calculateLogged = calculateText . logged

----

instance ConsoleIO (BusinessT MockIO) where
  wGetContentsBS   = businessT   mockGetContentsBS
  wGetContentsText = businessT   mockGetContentsText
  wGetContents     = businessT   mockGetContents
  wGetChar         =             mockGetCharSafe
  wGetLine         =             mockGetLineSafe
  wPutChar         = businessT . mockPutChar
  wPutStr          = businessT . mockPutStr
  wLogStr          = businessT . mockLogStr

instance ConsoleIO (SafeT MockIO) where
  wGetContentsBS   = safeT   mockGetContentsBS
  wGetContentsText = safeT   mockGetContentsText
  wGetContents     = safeT   mockGetContents
  wGetChar         = safeT   mockGetChar
  wGetLine         = safeT   mockGetLine
  wPutChar         = safeT . mockPutChar
  wPutStr          = safeT . mockPutStr
  wLogStr          = safeT . mockLogStr

instance ConsoleIO MockIO where
  wGetContentsBS   = mockGetContentsBS
  wGetContentsText = mockGetContentsText
  wGetContents     = mockGetContents
  wGetChar         = mockGetChar
  wGetLine         = mockGetLine
  wPutChar         = mockPutChar
  wPutStr          = mockPutStr
  wLogStr          = mockLogStr

----

instance FileReaderIO (BusinessT MockIO) where
  wReadFile = pure . toText --FIXME

----

mockGetContentsBS :: MonadMockIO m => m LBS.ByteString
mockGetContentsBS =  fromStrict . encodeUtf8 <$> mockGetContentsText

mockGetContentsText :: MonadMockIO m => m LT.Text
mockGetContentsText = fromStrict . toText <$> mockGetContents

mockGetContents :: MonadMockIO m => m String
mockGetContents = mockGetContents' =<< get where
  mockGetContents' :: MonadMockIO m => MockIOData -> m String
  mockGetContents' mockIO = content <$ put mockIO { input = "" } where content = input mockIO

mockGetChar :: MonadMockIO m => m Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadMockIO m => MockIOData -> m Char
  mockGetChar' mockIO = orErrorTuple ("mockGetChar" , show mockIO) (top (input mockIO)) <$ put mockIO { input = orErrorTuple ("mockGetChar" , show mockIO) $ discard $ input mockIO }

mockGetLine :: MonadMockIO m => m Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadMockIO m => MockIOData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockGetCharSafe :: MonadBusinessMockIO m => m Char
mockGetCharSafe = mockGetChar' =<< get where
  mockGetChar' :: MonadBusinessMockIO m => MockIOData -> m Char
  mockGetChar' mockIO = appendErrorTuple ("mockGetCharSafe" , show mockIO) $ mockGetChar'' =<< unconsSafe (input mockIO) where
    mockGetChar'' (c, input') = put mockIO { input = input' } $> c

mockGetLineSafe :: MonadBusinessMockIO m => m Text
mockGetLineSafe = mockGetLine' =<< get where
  mockGetLine' :: MonadBusinessMockIO m => MockIOData -> m Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO


mockPutChar :: Char -> MockIO ()
mockPutChar = modify . mockDataPutChar

mockPutStr :: Text -> MockIO ()
mockPutStr = modify . mockDataPutStr

mockLogStr :: Text -> MockIO ()
mockLogStr = modify . mockDataLogStr

----

mockDataPutChar :: Char -> MockIOData -> MockIOData
mockDataPutChar char mockIO = mockIO { output = char : output mockIO }

mockDataPutStr :: Text -> MockIOData -> MockIOData
mockDataPutStr text mockIO = mockIO { output = calculateString text <> output mockIO }

mockDataLogStr :: Text -> MockIOData -> MockIOData
mockDataLogStr text mockIO = mockIO { logged = calculateString text <> logged mockIO }

----

type MonadBusinessMockIO m = (MonadMockIO m , MonadBusiness m) --FIXME

--type MonadSafeMockIO m = (MonadMockIO m , MonadSafe m) --FIXME

type MonadMockIO m = MonadState MockIOData m

type MockIO = State MockIOData

calculateText :: String -> Text
calculateText = Text.reverse . toText

calculateString :: Text -> String
calculateString =  toString . Text.reverse

data MockIOData = MockIOData
  { input  :: !String
  , output :: !String
  , logged :: !String
  }
  deriving stock (Eq , Read , Show)

----

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
