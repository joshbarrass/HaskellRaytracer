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
import System.Random
-- import System.Random.PCG.Fast.Pure (create, uniformF)

-- traceShowId :: a -> a
-- traceShowId x = x

defaultBackground :: Ray -> Pixel
defaultBackground ray = let
  unit = normalise $ direction ray
  y = vy unit
  t = (y + 1.0) * 0.5
  in ((1-t) + t*0.5, (1-t) + t*0.7, 1)

data Scene = Scene { objects :: [Object], background :: Ray -> Pixel,  bottomLeft :: Vec3, screenSize :: (Vec3, Vec3), aa :: Int, seed :: Int}

defaultBGScene objects = Scene objects defaultBackground

render :: Scene -> (Int, Int) -> Image
render scene (width, height) = let
  w = fromIntegral width :: Float
  h = fromIntegral height :: Float
  u i = scale (i / w) (fst $ screenSize scene)
  v j = scale (j / h) (snd $ screenSize scene)
  ns = aa scene

  randomList = if ns > 1
    then getRandomList $ seed scene
    else [0.5 | _ <- [1..]]

  totalIndex i j = round w * j + i :: Int
  randomNums i j = take (ns * 2) $ drop (totalIndex i j * ns * 2) randomList

  targets = [[[bottomLeft scene `vadd` u (fromIntegral i + rx) `vadd` v (fromIntegral j + ry)
              | (rx, ry) <- splitZip $ randomNums i j]
             | i <- [0..width-1]]
            | j <- [height-1, height-2..0]]
  -- rays = [[pointFromTo (0,0,0) t | t <- targetRow] | targetRow <- targets]
  rays = map (map (map (pointFromTo (0,0,0)))) targets 

  in [[fromIntegral ns `vdiv` foldl1 vadd [color scene ray | ray <- aaRays]
      | aaRays <- rowOfRays]
     | rowOfRays <- rays]
  -- in map (map (map (color scene)) rays

color :: Scene -> Ray -> Pixel
color scene ray = let
  haveHit = Objects.hit ray 0 (1/0)
  hits = catMaybes [haveHit object | object <- objects scene]
  in case hits of [] -> background scene ray
                  xs -> let closest = head (sortOn t xs)
                            n = normal closest
                            in 0.5 `scale ` (n `vadd` (1, 1, 1))

getRandomList :: Int -> [Float]
getRandomList seed = randoms (mkStdGen seed) :: [Float]
-- getRandomList seed = let
  -- g = create seed
  -- in [uniformF g | _ <- [1..]]

splitZip :: [a] -> [(a, a)]
splitZip xs = let
  l = length xs
  midpoint = (l `div` 2)
  in zip (take midpoint xs) (drop midpoint xs)
