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

import Data.Maybe
import Debug.Trace

defaultBackground :: Ray -> Pixel
defaultBackground ray = let
  unit = normalise $ direction ray
  y = vy unit
  t = (y + 1.0) * 0.5
  in ((1-t) + t*0.5, (1-t) + t*0.7, 1)

data Scene = Scene { objects :: [Object], background :: Ray -> Pixel,  bottomLeft :: Vec3, screenSize :: (Vec3, Vec3)}

defaultBGScene objects = Scene objects defaultBackground

render :: Scene -> (Int, Int) -> Image
render scene (width, height) = let
  w = fromIntegral width :: Float
  h = fromIntegral height :: Float
  u i = scale (fromIntegral i / w) (fst $ screenSize scene)
  v j = scale (fromIntegral j / h) (snd $ screenSize scene)

  targets = [[bottomLeft scene `vadd` u i `vadd` v j | i <- [0..width-1]] | j <- [height-1, height-2..0]]
  rays = [[pointFromTo (0,0,0) t | t <- targetRow] | targetRow <- targets]

  in [[color scene ray | ray <- rowOfRays] | rowOfRays <- rays]

color :: Scene -> Ray -> Pixel
color scene ray = let
  haveHit = Objects.hit ray
  hits = [haveHit object | object <- objects scene]
  in if any isJust hits then Debug.Trace.trace (show hits) (1, 0, 0) else background scene ray
