module Image
  ( Pixel(..)
  , Image(..)
  , newImage
  , savePPM
  , setPixel
  ) where

import Vector

type Pixel = Vector.Vec3
output :: Pixel -> String
output (r, g, b) = let
  ir = r*255.0 :: Float
  ig = g*255.0 :: Float
  ib = b*255.0 :: Float
  in show (min (round ir) 255) ++ " " ++ show (min (round ig) 255) ++ " " ++ show (min (round ib) 255)
-- output (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b

type Image = [[Pixel]]

fill :: Int -> a -> [a]
fill n x = [x | _ <- [1..n]]

newImage :: (Int, Int) -> Image
newImage (width, height) = let
  oneRow = fill width (0, 0, 0)
  in fill height oneRow

setPixel :: Image -> Int -> Int -> Pixel -> Image
setPixel im x y v = let
  oldRow = im !! y
  newRow = take x oldRow ++ v : drop (x+1) oldRow
  in take y im ++ newRow : drop (y+1) im

serialiseImage :: Image -> String
serialiseImage im = concat [output pix ++ "\n" | pix <- concat im]

savePPM :: Image -> FilePath -> IO ()
savePPM im path = do writeFile path fileContent
                       where fileContent = "P3\n" ++ show (length $ head im) ++ " " ++ show (length im) ++ "\n255\n" ++ serialiseImage im
