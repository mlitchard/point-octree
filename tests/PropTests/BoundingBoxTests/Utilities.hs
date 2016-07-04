{- |
  Module : PropTests.BoundingBoxTests.Utilities
  Copyright  : Copyright (c) 2016 Michael Litchard
  License    : BSD3

  Maintainer : Michael Litchard
  Stability  : experimental
  Portability: not portable
  
  This module provides utility functions for
  PropTests.BoundingBoxTests.External.hs

-}

module PropTests.BoundingBoxTests.Utilities
  ( bbis
  , isBBox
  , testConfig1
  , testConfig2
  , AltInput
  ) where


import Data.BoundingBox.B3 (BBox3 (..),bound_corners)

import Data.Octree.Internal hiding (lookup)
import Data.Octree.BoundingBox.BoundingBox (BBoxConfig (..))
import Data.Octree.BoundingBox.Internal ( DefInput
                                        , DefOutput
                                        , LeafValue
                                        , DefNodeValue
                                        , inclusive)
-- | Bounding Box of Infinite Space
--   The root Bounding Box
bbis :: BBox3
bbis =
  bound_corners swdCorner neuCorner
  where
    infinity = read "Infinity" :: Double
    swdCorner = Vector3 (-infinity) (-infinity) (-infinity)
    neuCorner = Vector3 infinity infinity infinity

isBBox :: BBox3 -> Bool
isBBox (BBox3 minX minY minZ maxX maxY maxZ) =
  (minX < maxX) && (minY < maxY) && (minZ < maxZ)


testConfig1 :: BBoxConfig DefInput [DefOutput] DefNodeValue
testConfig1 = BBoxConfig {
  select   = allPoints,
  procLeaf = allBoxesAndKeys,
  combine  = assemble
}
     
allBoxesAndKeys :: BBox3                     -> 
                   DefInput                  -> 
                   [LeafValue DefNodeValue ] -> 
                   [DefOutput]
allBoxesAndKeys bbx x leaf = [(bbx, leaf)]

allPoints :: BBox3 -> DefInput -> Maybe DefInput
allPoints bbx = Just

assemble :: DefInput -> [[DefOutput]] -> [DefOutput]
assemble _ = concat

type AltInput = BBox3

testConfig2 :: BBoxConfig AltInput DefOutput DefNodeValue
testConfig2 = BBoxConfig {
  select   = filterBoxes,
  procLeaf = checkbox,
  combine  = boxResult
}

filterBoxes :: BBox3 -> AltInput -> Maybe AltInput
filterBoxes genBox origBox = 
  if inclusive origBox genBox then Just origBox else Nothing

checkbox :: BBox3 -> AltInput -> [LeafValue DefNodeValue ] -> DefOutput
checkbox genBox _ leaf = (genBox, leaf)

boxResult :: AltInput -> [DefOutput] -> DefOutput
boxResult origBox allOuts =
  maybe bogusDummy ((,) origBox) $ lookup origBox allOuts
  where
    bogusDummy :: DefOutput
    bogusDummy = (bbis,[])         
