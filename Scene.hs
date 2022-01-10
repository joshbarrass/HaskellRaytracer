import Vector
import Image
import Ray

defaultBackground :: Ray -> Pixel
defaultBackground ray = let
  unit = normalise $ direction ray
  y = vy unit
  t = (y + 1.0) * 0.5
  in ((1-t) + t*0.5, (1-t) + t*0.7, 1)

data Object = Sphere { radius :: Float, centre :: Vec3 }
data Scene = Scene { objects :: [Object], background :: Ray -> Pixel,  bottomLeft :: Vec3, screenSize :: (Vec3, Vec3)}

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
color = background

main :: IO ()
main = do
  let
    scene = Scene [] defaultBackground (-2, -1, -1) ((4, 0, 0), (0, 2, 0))
    dims = (200, 100)
    in Image.savePPM (render scene dims) "test.ppm"
