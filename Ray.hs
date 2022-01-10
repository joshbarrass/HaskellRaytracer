module Ray
  ( Ray
  , pos
  , direction
  , pointFromTo
  ) where

import Vector

data Ray = Ray { pos :: Vector.Vec3, direction :: Vector.Vec3}

pointFromTo :: Vec3 -> Vec3 -> Ray
pointFromTo from to = let
  direction = normalise $ vsub to from
  in Ray from direction
