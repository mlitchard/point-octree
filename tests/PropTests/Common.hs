{- |
   Module      : PropTests.Common
   Copyright   : Copyright (c) 2012 Michal J. Gajda
   License     : BSD3
            
   Maintainer  : Michael Litchard
   Stability   : experimental
   Portability : not portable
                                           
   This module provides some helpers for the tests.

-}

module PropTests.Common where

import Data.Vector.Class (vunpack)
import Data.Vector.V3 (Vector3 (Vector3))
import Test.QuickCheck.Arbitrary

-- | For testing purposes
instance Ord Vector3 where
  a `compare` b = pointwiseOrd $ zipWith compare (vunpack a) (vunpack b)

pointwiseOrd []      = EQ
pointwiseOrd (LT:cs) = LT
pointwiseOrd (GT:cs) = GT
pointwiseOrd (EQ:cs)= pointwiseOrd cs

instance Arbitrary Vector3 where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Vector3 x y z

