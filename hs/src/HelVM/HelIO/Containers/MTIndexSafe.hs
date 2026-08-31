{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelIO.Containers.MTIndexSafe where

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator
import           Data.MonoTraversable
import           Data.Sequences

import           Prelude                  hiding (break, divMod, drop, fromList, length, splitAt, swap, uncons)

-- | Index
naturalIndexSafe :: (MonadSafe m , IndexSafe seq , Num $ Index seq) => seq -> Natural -> m $ Element seq
naturalIndexSafe l = indexSafe l . fromIntegral

-- | Type Class
class IndexSafe seq where
  indexSafe :: MonadSafe m => seq -> Index seq -> m $ Element seq
  indexSafe l = liftMaybeOrError "MTIndexSafe.indexSafe:" . indexMaybe l

  indexMaybe :: seq -> Index seq -> Maybe $ Element seq
  indexMaybe l = rightToMaybe <$> indexSafe l

  atSafe :: MonadSafe m => Index seq -> seq -> m $ Element seq
  atSafe = flip indexSafe

  atMaybe :: Index seq -> seq -> Maybe $ Element seq
  atMaybe = flip indexMaybe

  atWithDefault :: Element seq -> Index seq -> seq -> Element seq
  atWithDefault e i = fromMaybe e . atMaybe i

  findSafe :: MonadSafe m => Index seq -> seq -> m $ Element seq
  findSafe = atSafe

  findMaybe :: Index seq -> seq -> Maybe $ Element seq
  findMaybe = atMaybe

  findWithDefault :: Element seq -> Index seq -> seq -> Element seq
  findWithDefault = atWithDefault

{-# DEPRECATED findSafe "Use `atSafe`" #-}
{-# DEPRECATED findMaybe "Use `atMaybe`" #-}
{-# DEPRECATED findWithDefault "Use `atWithDefault`" #-}

instance IsSequence seq => IndexSafe seq where
  indexMaybe = index
