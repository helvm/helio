module HelVM.HelIO.Extra (
  toUppers,
  splitOneOf,
  showP,
  showToText,
  genericChr,
  (???),
  fromMaybeOrDef,
  headMaybe,
  fromJustWith,
  tee,
) where

import           Data.Char          hiding (chr)
import           Data.Default
import           Data.Typeable
import           Text.Pretty.Simple

import qualified Data.Text          as Text

-- | TextExtra

toUppers :: Text -> Text
toUppers = Text.map toUpper

splitOneOf :: String -> Text -> [Text]
splitOneOf s = Text.split contains where contains c = c `elem` s

----

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
fromJustWith e Nothing  = error $ show e
fromJustWith _ (Just a) = a

-- | Extra
tee :: (a -> b -> c) -> (a -> b) -> a -> c
tee f1 f2 a = f1 a $ f2 a
