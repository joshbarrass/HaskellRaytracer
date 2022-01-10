module Ray
  ( Ray(..)
  -- , pos
  -- , direction
  , pointFromTo
  , show
  ) where

import Vector

data Ray = Ray { pos :: Vector.Vec3, direction :: Vector.Vec3}

pointFromTo :: Vec3 -> Vec3 -> Ray
pointFromTo from to = let
  direction = normalise $ vsub to from
  in Ray from direction

instance Show Ray where
  show (Ray pos dir) = "Ray(A: " ++ show pos ++ ", B: " ++ show dir ++ ")"
