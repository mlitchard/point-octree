point-octree
======
This is a fork of Michał J Gajda's [Octree](https://github.com/BioHaskell/octree) library, with bounding boxes.

[![Build Status](https://travis-ci.org/mlitchard/point-octree.svg?branch=master)](https://www.travis-ci.org/mlitchard/point-octree)

(From Michal's README.md) 
To use simply:

~~~ {.haskell}
module Main where

import Data.Octree as O

import Data.Vector.V3

main = do let oct = fromList [(Vector3 1 2 3, "a"),
                              (Vector3 3 4 5, "b"),
                              (Vector3 8 8 8, "c")]
              report msg elt = putStrLn $ msg ++ show elt
          report "Nearest     :" $ O.nearest     oct     $ Vector3 2 2 3
          report "Within range:" $ O.withinRange oct 5.0 $ Vector3 2 2 3
          return ()
~~~


