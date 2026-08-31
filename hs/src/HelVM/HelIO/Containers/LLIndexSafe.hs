{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelIO.Containers.LLIndexSafe where

import           HelVM.HelIO.Control.Safe

import           Data.ListLike            hiding (show)

import           Prelude                  hiding (break, divMod, drop, fromList, length, splitAt, swap, uncons)

-- | Index
naturalIndexSafe :: (MonadSafe m , IndexSafe full item) => full -> Natural -> m item
naturalIndexSafe l =  indexSafe l <$> fromIntegral

-- | Type Class
class IndexSafe full item | full -> item where
  indexSafe :: MonadSafe m => full -> Int -> m item
  indexSafe l = liftMaybeOrError "LLTIndexSafe.indexSafe:" . indexMaybe l

  indexMaybe :: full -> Int -> Maybe item
  indexMaybe l = rightToMaybe <$> indexSafe l

  atSafe :: MonadSafe m => Int -> full -> m item
  atSafe = flip indexSafe

  atMaybe :: Int -> full -> Maybe item
  atMaybe = flip indexMaybe

  atWithDefault :: item -> Int -> full -> item
  atWithDefault e i = fromMaybe e <$> findMaybe i

  findSafe :: MonadSafe m => Int -> full -> m item
  findSafe = atSafe

  findMaybe :: Int -> full -> Maybe item
  findMaybe = atMaybe

  findWithDefault :: item -> Int -> full -> item
  findWithDefault = atWithDefault

{-# DEPRECATED findSafe "Use `atSafe`" #-}
{-# DEPRECATED findMaybe "Use `atMaybe`" #-}
{-# DEPRECATED findWithDefault "Use `atWithDefault`" #-}

instance ListLike full item => IndexSafe full item where
  indexSafe = indexSafeLL

-- | Internal functions
indexSafeLL :: (MonadSafe m , ListLike full item) => full -> Int -> m item
indexSafeLL l i
  | i < 0     = liftErrorWithTupleList "LLIndexSafe.indexSafeLL: index must be >= 0" [("i" , show i)]
  | ll <= i   = liftErrorWithTupleList "LLIndexSafe.indexSafeLL: index must not found" [("i" , show i) , ("length l" , show ll)]
  | otherwise = (pure <$> index l) i
    where ll = length l
