module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, projectX, projectY, norm, dot,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>),
  inside)  where

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
type Drawing = [(Transform,Shape)]

-- interpretation function for drawings
inside :: Point -> Drawing -> Bool
inside point = any (inside1 point)

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 point (t, s) = insides (transform t point) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm p <= 1
p `insides` Rectangle w h = insideRectangle p w h
p `insides` Ellipse rHorizontal rVertical = insideEllipse p rHorizontal rVertical
p `insides` Polygon lst = insidePolygon p lst

insideRectangle :: Point -> Double -> Double -> Bool
insideRectangle (Vector x y) w h = abs x <= w && abs y <= h

insideEllipse :: Point -> Double -> Double -> Bool
insideEllipse (Vector x y) rHorizontal rVertical = distance (Vector scaleX scaleY) <= 1
    where
        scaleX = x/rHorizontal
        scaleY = y/rVertical

--TODO
insidePolygon :: Point -> [Point] -> Bool
insidePolygon p lst = True

-- <ABC
anglePoints :: Point -> Point -> Point -> Double
anglePoints (Vector a b) (Vector c d) (Vector e f) = angleVectors (Vector (a-c) (b-d)) (Vector (e-c) (f-d))

angleVectors :: Vector -> Vector -> Double
angleVectors v1 v2 = acos (dot v1 v2/(norm v1 * norm v2))

distance :: Point -> Double
distance (Vector x y) = sqrt (x**2 + y**2)

maxnorm :: Point -> Double
maxnorm (Vector x y) = max (abs x) (abs y)
