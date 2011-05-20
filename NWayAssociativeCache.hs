-------------------------------------------------------------------------
-- | 
-- Module: NWayAssociativeCache
-- Copyright   :  (c) Andy Georges 2011,
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of an N-way set associative cache, using the LRU
-- replacement strategy
-----------------------------------------------------------------------------

module NWayAssociativeCache 
    ( NWayAssociativeCache
    , mkNWayAssociativeCache
    , Cache (..)
    , tag
    , set
    ) where

import qualified Data.IntMap as M
import Data.Maybe

import qualified LRUQueue as LQ
import Cache
 
-- | Representation of the N-way set associative cache
-- using a LRU replacement strategy
data NWayAssociativeCache = NWAC 
  { nwSets :: Int -- ^ Number of sets or blocks
  , nwLinesize :: Int -- ^ Number of bytes per block
  , nwAssociativity :: Int -- ^ Associativity of the cache
  , nwCache :: M.IntMap (LQ.LRUQueue Int)-- ^ LRU storage of the cache
  } deriving Show


mkNWayAssociativeCache :: Int -> Int -> Int -> NWayAssociativeCache
mkNWayAssociativeCache s l a = NWAC
  { nwSets = s
  , nwLinesize = l
  , nwAssociativity = a
  , nwCache = M.fromList $ zip [0..s-1] $ replicate s (LQ.mkLRUQueue [] a)
  }


instance Cache NWayAssociativeCache where
  request c address 
      | t `LQ.elem` q = (True, c { nwCache = M.adjust (\_ -> LQ.add q t ) s nwc})
      | otherwise = (False, c { nwCache = M.adjust (\_ -> LQ.add q t) s nwc} )
    where t = tag c address :: Int
          s = set c address
          nwc = nwCache c
          q = fromJust $ M.lookup s nwc :: LQ.LRUQueue Int


tag :: NWayAssociativeCache -> Int -> Int
tag (NWAC ss lz ass c) address = 
    let d = lz * ss in address `div` d


set :: NWayAssociativeCache -> Int -> Int
set (NWAC ss lz ass c) address =
    let d = lz * ss in address `mod` d `div` lz



