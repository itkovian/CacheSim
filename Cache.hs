-------------------------------------------------------------------------
-- | 
-- Module :  Cache
-- Copyright   :  (c) Andy Georges 2011,
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  itkovian@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Typeclass to represent a cache and the operations it should support.
-----------------------------------------------------------------------------

module Cache ( 
    -- * Cache typeclass
    Cache(..)
  ) where


class Show a => Cache a where
  request :: a -> Int -> (Bool, a)


  
