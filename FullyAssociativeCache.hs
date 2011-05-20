-------------------------------------------------------------------------
-- | 
-- Module: FullyAssociativeCache
-- Copyright   :  (c) Andy Georges 2011,
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of a fully associative cache
-----------------------------------------------------------------------------
module FullyAssociativeCache (
    FullyAssociativeCache
  , mkFullyAssociativeCache
  , Cache (..)
  ) where

import Control.Arrow (second)

import Cache
import qualified NWayAssociativeCache as NWC

data FullyAssociativeCache = FAC { facCache :: NWC.NWayAssociativeCache } deriving Show

mkFullyAssociativeCache :: Int -> Int -> FullyAssociativeCache
mkFullyAssociativeCache s l = FAC { facCache = NWC.mkNWayAssociativeCache 1 l s }


instance Cache FullyAssociativeCache where
  request cache address = second FAC $ request (facCache cache) address
