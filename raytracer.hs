import Image
import Scene
import Objects

main :: IO ()
main = do
  let
    scene = defaultBGScene [Sphere 0.5 (0, 0, -1)] (-2, -1, -1) ((4, 0, 0), (0, 2, 0))
    dims = (200, 100)
    in Image.savePPM (render scene dims) "test.ppm"
