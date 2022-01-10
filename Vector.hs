module Vector
  ( Vec3(..)
  , dot
  , cross
  , vreverse
  , vadd
  , vsub
  , scale
  , vmul
  , vdiv
  , magnitude
  , normalise
  , vx
  , vy
  , vz
  ) where

type Vec3 = (Float, Float, Float)

dot :: Vec3 -> Vec3 -> Float
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vec3 -> Vec3 -> Vec3
cross (x1, y1, z1) (x2, y2, z2) = (y1*z2 - z1*y2, x2*z1 - x1*z2, x1*y2 - x2*y1)

vreverse :: Vec3 -> Vec3
vreverse (x, y, z) = (-x, -y, -z)

vadd :: Vec3 -> Vec3 -> Vec3
(x1, y1, z1) `vadd` (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

vsub :: Vec3 -> Vec3 -> Vec3
v1 `vsub` v2 = v1 `vadd` vreverse v2

scale :: Float -> Vec3 -> Vec3
scale a (x, y, z) = (a*x, a*y, a*z)
vmul = scale
vdiv a = scale (1.0/a)

magnitude :: Vec3 -> Float
magnitude (x, y, z) = sqrt $ x**2 + y**2 + z**2

normalise :: Vec3 -> Vec3
normalise v = magnitude v `vdiv` v

vx :: Vec3 -> Float
vx (x, _, _) = x

vy :: Vec3 -> Float
vy (_, y, _) = y

vz :: Vec3 -> Float
vz (_, _, z) = z
