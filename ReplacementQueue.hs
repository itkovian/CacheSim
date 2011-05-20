{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------
-- | 
-- Module: ReplacementQueue
-- Copyright   :  (c) Andy Georges 2011,
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A class for queues that offer a replacement strategy
-----------------------------------------------------------------------------

module ReplacementQueue (
    -- * Replacement Queue typeclass
    ReplacementQueue(..)
  ) where

import Prelude hiding (elem)
-- ----------------------------------------------------------------------
-- | The class of replacement queues. These are data structures that 
-- adhere to several rules, such as having a maximal size, replace items
-- upon addition through some well defined (note that random is also 
-- well-defined in this sense) algorithm.
--
-- * @add adds an items, possibly removing one item if the size has grown 
-- too large
--
-- * @remove takes one item out of the set, making free space
--
-- * @front returns the front element of the queue
--
-- * @elem checks to see if an item is present in the replacement queue

class ReplacementQueue a b where 
  add    :: a -> b -> a 
  -- ^ Add an item to the queue, potentially ejecting an item 
  remove :: a -> b -> a 
  -- ^ Search and take out
  front  :: a -> b 
  -- ^ Returns the first item in the queue
  elem   :: b -> a -> Bool
  -- ^ Checks if an item is present in the queue, does not change anything

