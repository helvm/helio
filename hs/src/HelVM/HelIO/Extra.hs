module HelVM.HelIO.Extra where

import           Data.Char          hiding (chr)
import           Data.Default
import           Data.Typeable
import           Text.Pretty.Simple

import qualified Data.Text          as Text

-- | FilesExtra

readFileTextUtf8 :: MonadIO m => FilePath -> m Text
readFileTextUtf8 = (pure . decodeUtf8) <=< readFileBS

-- | TextExtra

toUppers :: Text -> Text
toUppers = Text.map toUpper

splitOneOf :: String -> Text -> [Text]
splitOneOf s = Text.split contains where contains c = c `elem` s

-- | ShowExtra

showP :: Show a => a -> Text
showP = toText . pShowNoColor

showToText :: (Typeable a , Show a) => a -> Text
showToText a = show a `fromMaybe` (cast a :: Maybe Text)

-- | CharExtra

genericChr :: Integral a => a -> Char
genericChr = chr . fromIntegral

-- | MaybeExtra

infixr 0 ???
(???) :: Maybe a -> a -> a
(???) = flip fromMaybe

fromMaybeOrDef :: Default a => Maybe a -> a
fromMaybeOrDef = fromMaybe def

headMaybe :: [a] -> Maybe a
headMaybe = viaNonEmpty head

fromJustWith :: Show e => e -> Maybe a -> a
fromJustWith e = fromJustWithText (show e)

fromJustWithText :: Text -> Maybe a -> a
fromJustWithText t Nothing  = error t
fromJustWithText _ (Just a) = a

-- | ListExtra

unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f = go <=< f where
  go  Nothing       = pure []
  go (Just (b, a')) = (b : ) <$> (go <=< f) a'

--unfoldr :: (a ->  Maybe (b, a)) -> a -> [b]
--unfoldr f = runIdentity . unfoldrM (Identity . f)

runParser :: Monad m => Parser a b m -> [a] -> m [b]
runParser f = go where
  go [] = pure []
  go a  = (build <=< f) a where build (b, a') = (b : ) <$> go a'

repeatedlyM :: Monad m => Parser a b m -> [a] -> m [b]
repeatedlyM = runParser

repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly f = runIdentity . repeatedlyM (Identity . f)

-- | Extra

tee :: (a -> b -> c) -> (a -> b) -> a -> c
tee f1 f2 a = (f1 a . f2) a

type Act s a = s -> Either s a
type ActM m s a = s -> m (Either s a)
type Parser a b m = [a] -> m (b, [a])
