module HelVM.HelIO.ListLike.ListLikeSafe where

import           HelVM.HelIO.ListLike.ListLikeMay

import           HelVM.HelIO.Control.Safe

import           Data.ListLike

import           Prelude                          hiding (break, divMod, drop, fromList, init, last, length, null, splitAt, swap, toList, uncons)

-- | Construction
convert :: (ListLike full1 item , ListLike full2 item) => full1 -> full2
convert = fromList <$> toList

maybeToFromList :: ListLike full item => Maybe item -> full
maybeToFromList (Just e) = singleton e
maybeToFromList Nothing  = mempty

-- | Split
splitBy :: (Eq item , ListLike full item) => item -> full -> (full , full)
splitBy separator l =  (acc , drop 1 l') where (acc , l') = break (== separator) l

-- | Pop
discard :: (MonadSafe m , ListLike full item) => full -> m full
discard l = appendError "Error for discard" $ snd <$> unconsSafe l

top :: (MonadSafe m , ListLike full item) => full -> m item
top s = appendError "Error for top" $ fst <$> unconsSafe s

unconsSafe :: (MonadSafe m , ListLike full item) => full -> m (item , full)
unconsSafe = liftMaybeOrError "Empty ListLike for unconsSafe" <$> uncons

uncons2Safe :: (MonadSafe m , ListLike full item) => full -> m (item , item , full)
uncons2Safe = liftMaybeOrError "Empty ListLike for uncons2Safe" <$> uncons2

unsnocSafe :: (MonadSafe m , ListLike full item) => full -> m (full , item)
unsnocSafe = liftMaybeOrError "Empty ListLike for unsnocSafe" <$> unsnoc
