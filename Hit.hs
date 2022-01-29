module Hit (
  Hit(..)
  ) where

import Vector

data Hit = Hit { point :: Vec3, normal :: Vec3, t :: Float } deriving Show
