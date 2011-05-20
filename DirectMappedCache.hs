-------------------------------------------------------------------------
-- | 
-- Module: DirectMappedCache
-- Copyright   :  (c) Andy Georges 2011,
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of a direct mapped cache.
-----------------------------------------------------------------------------
module DirectMappedCache ( 
      DirectMappedCache
    , mkDirectMappedCache
    ) where

import Control.Arrow (second)

import Cache
import qualified NWayAssociativeCache as NWC

data DirectMappedCache = DMC { dmCache :: NWC.NWayAssociativeCache } deriving Show


mkDirectMappedCache :: Int -> Int -> DirectMappedCache
mkDirectMappedCache s l = DMC { dmCache = NWC.mkNWayAssociativeCache s l 1 }


instance Cache DirectMappedCache where
  request cache address = second DMC $ request (dmCache cache) address

