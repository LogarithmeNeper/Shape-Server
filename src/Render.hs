-- Same exact file from the example given in class except from the last part (see below)

module Render(Window,defaultWindow,samples,render) where
import Codec.Picture ( writePng, generateImage, Pixel8, PixelRGB8 (PixelRGB8))
import Shapes

data Window = Window Point Point (Int,Int)

defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (500,500)

samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [c0, c0 + (c1-c0) / fromIntegral (n-1) ..]

pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (projectX p0) (projectX p1) w ]
                | y <- reverse $ samples (projectY p0) (projectY p1) h
  ]

coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]

scaleValue :: Fractional a => (a,a) -> (a,a) -> a -> a
scaleValue (a1,a2) (b1,b2) v = b1 + (v - a1) * (b2-b1) / (a2-a1)

mapPoint :: Window -> (Int,Int) -> Point
mapPoint (Window p0 p1 (w,h)) (x,y) = point scaledX scaledY
  where
    scaledX = scaleValue (0,fromIntegral w) (projectX p0, projectX p1) (fromIntegral x)
    scaledY = scaleValue (0,fromIntegral h) (projectY p0, projectY p1) (fromIntegral y)

render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win
      -- We actually use a PixelRGB8 instead of a pixel.
      pixRenderer :: Int -> Int -> PixelRGB8
      pixRenderer x y = colorForImage $ mapPoint win (x,y)

      -- Color for image uses the workflow defined in Shapes (see that file for more explanations)
      colorForImage :: Point -> PixelRGB8
      colorForImage p = getPixelColour p sh