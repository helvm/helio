module HelVM.HelIO.ListLike.ListLikeMay where

import           HelVM.HelIO.Extra

import           Data.ListLike

import           Prelude           hiding (break, divMod, drop, fromList, init, last, length, null, splitAt, swap, toList, uncons)

uncons2 :: ListLike full item => full -> Maybe (item, item, full)
uncons2 = uncons2' <=< uncons where
  uncons2' (e , l') = uncons2'' <$> uncons l' where
    uncons2'' (e' , l'') = (e , e' , l'')

unsnoc :: ListLike full item => full -> Maybe (full, item)
unsnoc l = toMaybe (null l) (init l , last l)
