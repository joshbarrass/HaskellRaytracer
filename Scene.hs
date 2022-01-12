module Scene
  ( defaultBackground
  , Scene(..)
  , defaultBGScene
  , render
  ) where

import Vector
import Image
import Ray
import Objects
import Hit

import Data.Maybe
import Data.List

defaultBackground :: Ray -> Pixel
defaultBackground ray = let
  unit = normalise $ direction ray
  y = vy unit
  t = (y + 1.0) * 0.5
  in ((1-t) + t*0.5, (1-t) + t*0.7, 1)

data Scene = Scene { objects :: [Object], background :: Ray -> Pixel,  bottomLeft :: Vec3, screenSize :: (Vec3, Vec3), seed :: Int}

defaultBGScene objects = Scene objects defaultBackground

render :: Scene -> (Int, Int) -> Image
render scene (width, height) = let
  w = fromIntegral width :: Float
  h = fromIntegral height :: Float
  u i = scale (fromIntegral i / w) (fst $ screenSize scene)
  v j = scale (fromIntegral j / h) (snd $ screenSize scene)

  targets = [[bottomLeft scene `vadd` u i `vadd` v j | i <- [0..width-1]] | j <- [height-1, height-2..0]]
  -- rays = [[pointFromTo (0,0,0) t | t <- targetRow] | targetRow <- targets]
  rays = map (map (pointFromTo (0,0,0))) targets

  -- in [[color scene ray | ray <- rowOfRays] | rowOfRays <- rays]
  in map (map (color scene)) rays

color :: Scene -> Ray -> Pixel
color scene ray = let
  haveHit = Objects.hit ray 0 (1/0)
  hits = catMaybes [haveHit object | object <- objects scene]
  in case hits of [] -> background scene ray
                  xs -> let closest = head (sortOn t xs)
                            n = normal closest
                            in 0.5 `scale ` (n `vadd` (1, 1, 1))
