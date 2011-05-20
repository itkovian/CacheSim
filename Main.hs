{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------
-- | 
-- Module: Main
-- Copyright   :  (c) Andy Georges 2011,
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Cache simulator for the course on computer architecture taught at UGent.
-----------------------------------------------------------------------------
module Main 
    (
    ) where

import Data.Bits
import Data.List
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit
import System.Environment (getArgs)

import DirectMappedCache
import NWayAssociativeCache 
import FullyAssociativeCache


data CacheSimArguments = CSA 
    { cacheType :: CacheType
    , cacheBlocks :: Int
    , cacheLineSize :: Int
    , cacheAssociativity :: Int
    , pattern :: Pattern
    } deriving (Show, Data, Typeable, Eq)
  
data CacheType = DirectMapped | FullyAssociative | NWaySetAssociative deriving (Data, Typeable, Show, Eq, Read)
data Pattern = Pattern1 | Pattern2 | Pattern3 deriving (Data, Typeable, Show, Eq, Read)

cacheSimArgs = CSA
    { cacheType          = DirectMapped &= explicit &= name "cache" &= help "Type of cache. One of DirectMapped, FullyAssociative, NWaySetAssociative"
    , cacheBlocks        = 8 &= explicit &= name "blocks" &= help "Total number of blocks in the cache."
    , cacheLineSize      = 32 &= explicit &= name "linesize" &= help "Size of each cache block."
    , cacheAssociativity = def &= explicit &= name "assoc" &= help "Associativity of the cache. Only used in a n-way set associative cache."
    , pattern            = Pattern1 &= explicit &= name "pattern" &= help "Address stream pattern to simulate."
    } 
    &= help "Cache simulator for the Computer Architecture course at UGent"
    &= summary "CacheSim v0.1, (C) Andy Georges 2010-2011"

isPowerOf2 :: Int -> Bool
isPowerOf2 1 = True
isPowerOf2 n
  | n `mod` 2 == 0 = isPowerOf2 (n `div` 2)
  | otherwise = False

isPowerOf2' :: Int -> Maybe Bool
isPowerOf2' n = if isPowerOf2 n then Just True else Nothing

main = do
    args <- cmdArgs cacheSimArgs

    let addresses = case pattern args of
                        Pattern1 -> pattern1 32
                        Pattern2 -> pattern2 32
                        Pattern3 -> pattern3 32

        blocks = cacheBlocks args
        linesize = cacheLineSize args

    if (not $ isPowerOf2 blocks) || (not $ isPowerOf2 linesize) 
      then fail "Whoops. Need powers of 2 for block and linesize."
      else do let result = case cacheType args of
                    DirectMapped       -> flip simulate addresses $ mkDirectMappedCache blocks linesize
                    FullyAssociative   -> flip simulate addresses $ mkFullyAssociativeCache blocks linesize
                    NWaySetAssociative -> let assoc = cacheAssociativity args 
                                          in if blocks `mod` assoc /= 0 
                                                then fail "Ouch. Blocks should be a multiple of the associativity."
                                                else flip simulate addresses $ mkNWayAssociativeCache (blocks `div` assoc) linesize assoc
                    
              putStrLn $ show result

  

simulate :: Cache a => a -> [Int] -> (Int, Int, Double)
simulate cache as = do
    let (hits, cache') = foldl (\(hits, cache) address -> let (b, cache') = request cache address in (b:hits, cache')) ([], cache) as
        requests = length as
        hitCount = length $ filter id hits
    --putStrLn $ unlines . map (\(m, address, h) -> m ++ ": " ++ (show address) ++ (if h then "" else " *")) $ zip3 (concat $ repeat ["A","B"]) as (reverse hits) 
    --putStrLn $ show cache'
    (requests, hitCount, (fromIntegral hitCount) / (fromIntegral requests)) 


pattern1 :: Int -> [Int]
pattern1 size = 
    let aBase = 0
        bBase = 4 * size*size + 64
        as = do x <- [0..size-1]
                y <- [0..size-1]
                return $ (aBase,y,x)
        bs = do x <- [0..size-1]
                y <- [0..size-1]
                return $ (bBase,x,y)
    in map computeAddress $ interleave as bs
  where computeAddress (base, i, j) = base + 4 * (i * size + j)
        interleave [] [] = []
        interleave (a:as) (b:bs) = a:b:interleave as bs


pattern2 :: Int -> [Int]
pattern2 size = 
    let aBase = 0
        bBase = 4 * size * size + 64
        ps = concat $ [ [(aBase, k, j) | k <- [0..size-1] ] ++ [ (aBase, i, k) | k <- [0..size-1]] ++ [(bBase,i,j)] | i <- [0..size-1], j <- [0..size-1]]
    in map computeAddress ps
  where computeAddress (base, i, j) = base + 4 * (i * size + j)

                
pattern3 = undefined
