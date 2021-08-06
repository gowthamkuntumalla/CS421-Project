-- Exploring Generation of Fractals Using Haskell 

-- gowtham kuntumalla

-- main ref:- Mark P. Jones, Journal of Functional Programming, 14(6), November 2004.

import Data.Complex   
import Data.List 
import Control.Monad 
import Text.Read 

-- Type declarations
type Point = (Float, Float)
type Image color = Point -> color -- From Points to Pictures
type Grid a = [[a]]


-- this iterator is a key generator for mandelbrot set (set of all z that satisfy the following equation).
-- say z = x + i y, p = u + i v (complex number). z_(n+1) = z_n^2 + p (2way symmetry)
next :: Point -> Point -> Point
next (u,v) (x,y) = (x*x-y*y+u,2*x*y+v)


-- Another possible iterator. z_(n+1) = z_n^3 + p (3way symmetry)
next2 :: Point -> Point -> Point
next2 (u,v) (x,y) = (x*x*x-3*x*y*y+u,3*x*x*y - y*y*y + v) 

-- this function takes one argument, point p and starts the image about origin (0,0)
mandelbrot :: Point -> [Point] 
mandelbrot p = iterate (next p) (0, 0)
-- mandelbrot p = iterate (next2 p) (0, 0) -- beauty goes away. not very interesting as the 2 way symmetry.

fairlyClose :: Point -> Bool
fairlyClose (u,v) = (u*u+v*v)<100 
-- in fairlyClose, we can change this the upper limit number to vary the size of the image. 
-- smaller number indicates a set of images closer to origin and vice versa. 

inMandelbrotSet :: Point -> Bool 
inMandelbrotSet p = all fairlyClose (mandelbrot p)

-- approxTest :: Int -> Point -> Bool
-- approxTest n p = all fairlyClose (take n (mandelbrot p))


-- functions to help in IO drawing - polymorphic function. palette can refer to string of character, RGB pixel etc.
-- points with more number of faily close points are 'thicker' in character pallete / 'brighter' in color pallete
-- and correspondingly have higher index in the pallete
chooseColor :: [color] -> [Point] -> color
chooseColor palette = (palette !!) . length . take n . takeWhile fairlyClose where n = length palette - 1

fracImage :: (Point -> [Point]) -> [color] -> Image color 
fracImage fractal palette = chooseColor palette . fractal

for :: Int -> Float -> Float -> [Float] 
for n min1 max1 = take n [min1, min1 + delta ..]
    where delta = (max1 - min1)/fromIntegral (n - 1)

-- limit the fractal image to a certain rectangle with grid points as defined by the user
grid :: Int -> Int -> Point  -> Point -> Grid Point 
grid c r (xmin, ymin) (xmax, ymax) = [ [(x,y) | x <- for c xmin xmax] | y <- for r ymin ymax]

sample :: Grid Point -> Image color -> Grid color 
sample points image = map (map image) points

-- converts numberrs to chars/pixels using mentioned geomteric limits
draw :: Grid Point -> (Point -> [Point]) -> [color] -> (Grid color -> image) -> image
draw points fractal palette render = render (sample points (fracImage fractal palette))

-- # character pallete for showing on sreen
charPalette :: [Char]
charPalette = "  `,.‘~:;o-!|?/<>X+={#%&@8*$" 

charRender :: Grid Char -> IO () 
charRender = putStr . unlines

--  FIGURES

-- Mandelbrot Sets
figure10 = draw points mandelbrot charPalette charRender
    where points = grid 75 35 (-2.5, -1.5) (0.75, 1.5)

-- figure11 = draw points mandelbrot charPalette charRender
--     where points = grid 75 35 (-2.25, -1) (0.5, 2)

-- Julia Sets (simiar to mandelbrot but )
julia :: Point -> Point -> [Point] 
julia c = iterate (next c)
-- julia c = iterate (next2 c) -- beauty goes away. not very interesting as the 2 way symmetry.

figure20 = draw points (julia (0.32, 0.043)) charPalette charRender
    where points = grid 45 37 (-1.5, -1.5) (1.5, 1.5)

figure21 = draw points (julia (0.5, 0.01)) charPalette charRender
    where points = grid 45 37 (-1.5, -1.5) (1.5, 1.5)
