module Objects
  ( Object(..)
  , hit
  ) where

import Vector
import Ray
import Hit

data Object = Sphere { radius :: Float, centre :: Vec3 }

solveQuadratic :: Float -> Float -> Float -> Maybe (Float, Float)
solveQuadratic a b c
  | discriminant < 0 = Nothing
  | discriminant == 0 = let t = (-b)/(2*a) in Just (t, t)
  | otherwise = Just (((-b) + sqrt discriminant) / (2*a), ((-b) - sqrt discriminant) / (2*a))
  where discriminant = b*b - 4*a*c
  

hit :: Ray -> Object -> Maybe Hit
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
  solutions = solveQuadratic qa qb qc
  in case solutions of Nothing -> Nothing
                       Just (t1, t2) -> let t = min t1 t2
                                            hitPos = a `vadd` (t `scale` b)
                                            normal = normalise $ hitPos `vsub` c
                                        in Just $ Hit hitPos normal t
