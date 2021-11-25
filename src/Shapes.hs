module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, projectX, projectY, norm, dot,
  empty, circle, square, rectangle, ellipse, polygon,
  identity, translate, rotate, scale, (<+>),
  inside, shapeColour, getPixelColour)  where

import Codec.Picture (PixelRGB8 (PixelRGB8))
import Data.List (sortOn)
import Data.Ord (Down(Down))

-- Basic Class of vectors
data Vector = Vector Double Double
    deriving Show

vector :: Double -> Double -> Vector
vector = Vector

-- Using vectors
projectX :: Vector -> Double
projectX (Vector x y) = x

projectY :: Vector -> Double
projectY (Vector x y) = y

norm :: Vector -> Double
norm (Vector a b) = sqrt (a*a + b*b)

dot :: Vector -> Vector -> Double
dot (Vector a b) (Vector c d) = a*c + b*d

-- Basic Class of 2*2 Matrix
data Matrix = Matrix Vector Vector
    deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

-- Using 2*2 matrices
rotationMatrix :: Double -> Matrix
rotationMatrix theta = matrix (cos theta) (-sin theta) (sin theta) (cos theta)

transpose :: Matrix -> Matrix
transpose (Matrix (Vector a b) (Vector c d)) = Matrix (Vector a c) (Vector b d)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = Matrix (Vector (d/det) (-b/det)) (Vector (-c/det) (a/det))
    where det = a*d-b*c

-- Matrix / Vector operations
prod :: Matrix -> Vector -> Vector
prod (Matrix r1 r2) v = Vector (dot r1 v) (dot r2 v)

-- Basic data type for Point
type Point = Vector

point :: Double -> Double -> Point
point = vector

-- Shapes
data Shape = Empty
    | Circle
    | Square
    | Rectangle Double Double
    | Ellipse Double Double
    | Polygon [Point]
    deriving Show

empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

rectangle :: Double -> Double -> Shape
rectangle = Rectangle

ellipse :: Double -> Double -> Shape
ellipse = Ellipse

polygon :: [Point] -> Shape
polygon = Polygon

-- Hierarchy shape to apply a hierarchy mask : from different images we gonna choose the color of the one with the highest index.
type HierarchyShape = (Shape, PixelRGB8, Int)

-- Working on shapes
data Transform = Identity
    | Translate Vector
    | Scale Vector
    | Compose Transform Transform
    | Rotate Matrix
    deriving Show

identity :: Transform
identity = Identity

translate :: Vector -> Transform
translate = Translate

scale :: Vector -> Transform
scale = Scale

rotate :: Double -> Transform
rotate angle = Rotate $ rotationMatrix angle

(<+>) :: Transform -> Transform -> Transform
t0 <+> t1 = Compose t0 t1

-- Explicit definitions
transform :: Transform -> Point -> Point
transform Identity p = p
transform (Translate (Vector transx transy)) (Vector x y) = Vector (x-transx) (y-transy)
transform (Scale (Vector scalex scaley)) (Vector x y)  = Vector (x/scalex)  (y/scaley)
transform (Rotate matrix) point = invert matrix `prod` point
transform (Compose transform1 transform2) point = transform transform2 $ transform transform1 point

-- Drawings
type Drawing = [(Transform, HierarchyShape)]

-- interpretation function for drawings
insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm p <= 1
p `insides` Rectangle w h = insideRectangle p w h
p `insides` Ellipse rHorizontal rVertical = insideEllipse p rHorizontal rVertical
p `insides` Polygon lstOfPoints = insidePolygon p lstOfPoints

insideRectangle :: Point -> Double -> Double -> Bool
insideRectangle (Vector x y) w h = abs x <= w && abs y <= h

insideEllipse :: Point -> Double -> Double -> Bool
insideEllipse (Vector x y) rHorizontal rVertical = distance (Vector scaleX scaleY) <= 1
    where
        scaleX = x/rHorizontal
        scaleY = y/rVertical

insidePolygon :: Point -> [Point] -> Bool
-- sanity check
insidePolygon p [] = False
insidePolygon p [t] = False
insidePolygon p [t1,t2] = False
-- case of a real polygon
insidePolygon p (t1:t2:lst) = insidePolygonTransformedList p (t1:t2:lst++[t1,t2])

insidePolygonTransformedList :: Point -> [Point] -> Bool
-- sanity check
insidePolygonTransformedList p [] = False
insidePolygonTransformedList p [t] = False
insidePolygonTransformedList p [t1,t2] = False
-- real-life stop
insidePolygonTransformedList p [t1,t2,t3] = compareAngle p t1 t2 t3
insidePolygonTransformedList p (t1:t2:t3:lst) = compareAngle p t1 t2 t3 && insidePolygonTransformedList p (t2:t3:lst)

compareAngle :: Point -> Point -> Point -> Point -> Bool
compareAngle pointTBT side1 center side2 = anglePoints side1 center pointTBT <= anglePoints side1 center side2

anglePoints :: Point -> Point -> Point -> Double
anglePoints (Vector a b) (Vector c d) (Vector e f) = angleVectors (Vector (a-c) (b-d)) (Vector (e-c) (f-d))

angleVectors :: Vector -> Vector -> Double
angleVectors v1 v2 = acos (dot v1 v2/(norm v1 * norm v2))

distance :: Point -> Double
distance (Vector x y) = sqrt (x**2 + y**2)

maxnorm :: Point -> Double
maxnorm (Vector x y) = max (abs x) (abs y)

-- Colours
shapeColour :: Point -> HierarchyShape -> Maybe PixelRGB8
shapeColour point (shape, colour, _) = if insides point shape then Just colour else Nothing

-- TODO : actually implement a gradient

inside1 :: Point -> (Transform, HierarchyShape) -> (Maybe PixelRGB8, Int)
inside1 point (t, (s, c, h)) = (shapeColour (transform t point) (s, c, h), h)

inside :: Point -> Drawing -> [(Maybe PixelRGB8, Int)]
inside p [] = []
inside p lst = map (inside1 p) lst

filterList ::  [(Maybe PixelRGB8, Int)] -> [(PixelRGB8, Int)]
filterList [] = []
filterList ((p, h):q) = case p of
    Just x -> (x, h) : filterList q
    Nothing -> (PixelRGB8 0 0 0, -1):filterList q

getPixelColourFromList :: [(PixelRGB8, Int)] -> PixelRGB8
getPixelColourFromList lst = fst (head (sortOn (Down . snd) lst))

getPixelColour :: Point -> Drawing -> PixelRGB8
getPixelColour p lst = getPixelColourFromList (filterList (inside p lst))