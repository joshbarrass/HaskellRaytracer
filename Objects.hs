module Objects
  ( Object(..)
  , hit
  ) where

import Vector
import Ray

data Object = Sphere { radius :: Float, centre :: Vec3 }

hit :: Ray -> Object -> Bool
hit (Ray a b) (Sphere r c) = let
  ac = a `vsub` c
  -- Construct quadratic equation in t
  -- t^2 B.B + 2t B.(A-C) + (A-C).(A-C) - R*R
  -- where A is pos ray
  --       B is direction ray
  --       C is centre sphere
  qa = b `dot` b
  qb = 2 * b `dot` ac
  qc = ac `dot` ac - r*r
  discriminant a b c = b*b - 4*a*c
  in discriminant qa qb qc > 0
