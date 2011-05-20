{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-------------------------------------------------------------------------
-- | 
-- Module: LRUQueue
-- Copyright   :  (c) Andy Georges 2011,
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of an LRU queue, with a fixed maximal size 
-----------------------------------------------------------------------------

module LRUQueue ( 
    LRUQueue
  , mkLRUQueue
  , ReplacementQueue (..)
  ) where

import qualified Data.List as DL
import ReplacementQueue

data LRUQueue a = LRUQ ![a] !Int deriving Show

mkLRUQueue :: [a] -> Int -> LRUQueue a
mkLRUQueue xs maxSize = LRUQ xs maxSize

instance (Num a, Eq a) => ReplacementQueue (LRUQueue a) a where
  add (LRUQ xs maxSize) x 
      | x `DL.elem` xs = LRUQ ((xs DL.\\ [x]) ++ [x]) maxSize
      | length xs < maxSize = LRUQ (xs ++ [x]) maxSize
      | otherwise = LRUQ (tail xs ++ [x]) maxSize

  front (LRUQ (x:xs) maxSize) = x

  remove (LRUQ xs maxSize) x = LRUQ (xs DL.\\ [x]) maxSize

  elem x (LRUQ xs _) = x `DL.elem` xs


