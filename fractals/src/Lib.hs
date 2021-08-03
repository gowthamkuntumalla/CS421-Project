{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

  -- needed to display the picture in the playground
  -- Rasterific
import Graphics.Rasterific hiding (Point, Vector, Line, Path, polygon)
import Graphics.Rasterific.Texture
import Data.Complex
import System.IO
import Codec.Picture -- (generateImage, writePng, PixelRGBA8, Image)

type Point   = (Float, Float)
type Vector  = (Float, Float)
type Line    = (Point, Point)
type Path    = [Point]
type Picture = [(Colour, Path)]
type Colour  = (Int, Int, Int, Int) -- red, green, blue, opacity

-- Predefined colours
--
white, black, blue, red, green, orange, magenta, lightgreen, darkblue :: Colour
white      = (200,  200, 255, 255)
black      = (  0,    0,   0, 255)
blue       = (  0,  110, 255, 255)
red        = (255,    0,   0, 255)
green      = (10,  255,  10,  235)
orange     = (255,  255,  0,  200)
magenta    = (153,    0, 153, 220)
lightgreen = ( 27,  230,  34, 255)
darkblue   = ( 24,   50, 194, 255)

-- Render a picture composed of coloured line paths with the specified line width.
--
drawPicture :: Float -> Picture -> Image PixelRGBA8
drawPicture linewidth picture
  = renderDrawing  800 800 (toColour black) $
      mapM_ (\(col, path) -> withTexture (uniformTexture $ toColour col) (drawPath path)) picture
  where
    drawPath points    = stroke linewidth  JoinRound (CapRound, CapStraight 0) $
                           polyline (map (\(x, y) -> V2 x y) points)
    toColour (a,b,c,d) = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)



next :: Point -> Point -> Point
next (u,v) (x,y) = (x*x-y*y+u,2*x*y +v)

mandelbrot :: Point -> [Point] 
mandelbrot p = iterate (next p) (0, 0)

fairlyClose :: Point -> Bool
fairlyClose (u,v) = (u*u+v*v)<100


inMandelbrotSet :: Point -> Bool 
inMandelbrotSet p = all fairlyClose (mandelbrot p)

approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))


chooseColor :: [color] -> [Point] -> color
chooseColor palette = (palette !!) . length . take n . takeWhile fairlyClose where n = length palette - 1

someFunc :: IO ()
someFunc = putStrLn "someFuncstring"



house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]

