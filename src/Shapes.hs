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

-- Constructor of a vector
vector :: Double -> Double -> Vector
vector = Vector

-- Using vectors
-- Project on the horizontal axis p_x
projectX :: Vector -> Double
projectX (Vector x y) = x

-- Project on the vertical axis p_y
projectY :: Vector -> Double
projectY (Vector x y) = y

-- Getting the Euclidian norm of a vector v -> ||v||_2
norm :: Vector -> Double
norm (Vector a b) = sqrt (a*a + b*b)

-- Implementing the dot product between two vectors : (v1, v2) -> v1 . v2
dot :: Vector -> Vector -> Double
dot (Vector a b) (Vector c d) = a*c + b*d

-- Basic Class of 2*2 Matrix
data Matrix = Matrix Vector Vector
    deriving Show

-- Constructor of a matrix
matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

-- Defining a rotation matrix by its angle : \theta -> R_\theta
rotationMatrix :: Double -> Matrix
rotationMatrix theta = matrix (cos theta) (-sin theta) (sin theta) (cos theta)

-- Transpose a matrix : M -> M^T
transpose :: Matrix -> Matrix
transpose (Matrix (Vector a b) (Vector c d)) = Matrix (Vector a c) (Vector b d)

-- Invert a matrix : M -> M^{-1} (if M\in GL_2(R))
invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = Matrix (Vector (d/det) (-b/det)) (Vector (-c/det) (a/det))
    where det = a*d-b*c

-- Multiply two matrices : (M1, M2) -> M1M2 (not commutative)
multiply :: Matrix -> Matrix -> Matrix
multiply (Matrix (Vector a b) (Vector c d)) (Matrix (Vector e f) (Vector g h)) = (Matrix (Vector i j) (Vector k l))
    where 
        i = a*e + b*g
        j = a*f + b*h
        k = c*e + d*g
        l = c*f + d*h

-- Matrix / Vector operations
-- Product between a matrix and a vector (M, v) -> Mv
prod :: Matrix -> Vector -> Vector
prod (Matrix r1 r2) v = Vector (dot r1 v) (dot r2 v)

-- Basic data type for Point
type Point = Vector

-- Constructor of the point
point :: Double -> Double -> Point
point = vector

-- Shapes
-- Enumeration of the shapes as defined in the assignment. Extending the Shapes datatype given in weekly assignment.
data Shape = Empty
    | Circle
    | Square
    | Rectangle Double Double
    | Ellipse Double Double
    | Polygon [Point]
    deriving Show

-- constructor for simple shapes (norm : 0 or 1)
empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

-- Constructor for a rectangle with two parameters (width, heigth)
rectangle :: Double -> Double -> Shape
rectangle = Rectangle

-- Constructor for a ellipse with two parameters (radius horiz, radius vert)
ellipse :: Double -> Double -> Shape
ellipse = Ellipse

-- Constructor for a polygon with a list f points.
polygon :: [Point] -> Shape
polygon = Polygon

-- Almost everything is eta-reduced here, not sure if this is really clear...

-- Hierarchy shape to apply a hierarchy mask : from different images we gonna choose the color of the one with the highest index.
-- Used to describe a shape with it's color and the index (the lower, the more background the shape is going to be)
type HierarchyShape = (Shape, PixelRGB8, Int)

-- Transform shapes using a set of Transform. We used what was given in the weekly exercise, and nothing changed except optimization.
-- Not commenting the constructors, except the <+> operator, see below.
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


-- I tried to optimize this and to concatenate successive rotations, translations and scales.
-- When two rotations are applied, the resulting rotation is the sum of the two angles, or the product of the two matrices.
-- When two translations are applied, the resulting translation is the translation of the sum-vector.
-- When two scaling are applied, the resulting scaling is the scale of the coordinate-product of the two vectors.
-- In any other case, we could work with a list of transform, I would do it if I had more time (see report)
(<+>) :: Transform -> Transform -> Transform
(Rotate matrix1) <+> (Rotate matrix2) = Rotate (matrix1 `multiply` matrix2)
(Translate (Vector x y)) <+> (Translate (Vector x' y')) = translate (Vector (x+x') (y+y'))
(Scale (Vector x y)) <+> (Scale (Vector x' y')) = Scale (Vector (x*x') (y*y'))
t0 <+> t1 = Compose t0 t1

-- Explicit definitions of transformations.
transform :: Transform -> Point -> Point
transform Identity p = p
transform (Translate (Vector transx transy)) (Vector x y) = Vector (x-transx) (y-transy)
transform (Scale (Vector scalex scaley)) (Vector x y)  = Vector (x/scalex)  (y/scaley)
transform (Rotate matrix) point = invert matrix `prod` point
transform (Compose transform1 transform2) point = transform transform2 $ transform transform1 point

-- Drawings type as a list of transformation and shapes with color and index.
type Drawing = [(Transform, HierarchyShape)]

-- Inside function for drawings : depending on the shape.
insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm p <= 1
p `insides` Rectangle w h = insideRectangle p w h
p `insides` Ellipse rHorizontal rVertical = insideEllipse p rHorizontal rVertical
p `insides` Polygon lstOfPoints = insidePolygon p lstOfPoints

-- A rectangle is a scaled square, let's use this as we use the absolute value of x/w <= 1 and y/h <= 1 (we use large inequalities as we want to include the boundary.)
insideRectangle :: Point -> Double -> Double -> Bool
insideRectangle (Vector x y) w h = abs x <= w && abs y <= h

-- Similarly for an ellipse seen as a scaled circle.
insideEllipse :: Point -> Double -> Double -> Bool
insideEllipse (Vector x y) rHorizontal rVertical = distance (Vector scaleX scaleY) <= 1
    where
        scaleX = x/rHorizontal
        scaleY = y/rVertical

-- Function to check if a point is inside a polygon. 
-- We want to check the angles between all three consecutive points (see below) so we need to actually use the first points at the end of the list.
insidePolygon :: Point -> [Point] -> Bool
-- sanity check
insidePolygon p [] = False
insidePolygon p [t] = False
insidePolygon p [t1,t2] = False
-- case of a real polygon
insidePolygon p (t1:t2:lst) = insidePolygonTransformedList p (t1:t2:lst++[t1,t2])

-- For each triplet of points we need that the angle of the point with the first two points (the second one being the center) 
-- is <= the angle between the three consecutive points (the second one being the center).
-- However, this function has a quite unexpected outcome when working with examples. I need to check.
insidePolygonTransformedList :: Point -> [Point] -> Bool
-- sanity check
insidePolygonTransformedList p [] = False
insidePolygonTransformedList p [t] = False
insidePolygonTransformedList p [t1,t2] = False
-- real-life stop
insidePolygonTransformedList p [t1,t2,t3] = compareAngle p t1 t2 t3
insidePolygonTransformedList p (t1:t2:t3:lst) = compareAngle p t1 t2 t3 && insidePolygonTransformedList p (t2:t3:lst)

-- Functions used to compare angles between four points (basic comparison in the function above : P, (ABC) -> checking if <ABP <= <ABC)
compareAngle :: Point -> Point -> Point -> Point -> Bool
compareAngle pointTBT side1 center side2 = anglePoints side1 center pointTBT <= anglePoints side1 center side2

-- Function used to compute angles between three points : (A, B, C) -> <ABC.
anglePoints :: Point -> Point -> Point -> Double
anglePoints (Vector a b) (Vector c d) (Vector e f) = angleVectors (Vector (a-c) (b-d)) (Vector (e-c) (f-d))

--  Function used to compute angles between two vectors : (v1, v2) -> <(v1, v2) = acos((v1.v2)/(||v1||.||V2||)).
angleVectors :: Vector -> Vector -> Double
angleVectors v1 v2 = acos (dot v1 v2/(norm v1 * norm v2))

-- Euclidian distance for a point : p -> ||p||_2
distance :: Point -> Double
distance (Vector x y) = sqrt (x**2 + y**2)

-- Manahattan distance for a point : p -> ||p||_\infty
maxnorm :: Point -> Double
maxnorm (Vector x y) = max (abs x) (abs y)

-- Colours
-- Using the Maybe Monad to implement the color : if the point is inside, we go with Just color (param), else we go with Nothing.
shapeColour :: Point -> HierarchyShape -> Maybe PixelRGB8
shapeColour point (shape, colour, _) = if insides point shape then Just colour else Nothing

-- TODO : actually implement a gradient [clearly not done at this point and I still have to figure out how to do it]

-- Function to get the eventual pixel and the hierarchy for ***one*** individual transformed shape so that we either get :
-- (point, (transformation, (shape, colour, hierarchy))) -> (Just colour, hierarchy) if inside
--                                                       -> (Nothing, hierarchy) is not
-- This will be used to produce a list of such Maybe pixels and hierarchy, from which we will filter on Just values to get the actual color (see below)
inside1 :: Point -> (Transform, HierarchyShape) -> (Maybe PixelRGB8, Int)
inside1 point (t, (s, c, h)) = (shapeColour (transform t point) (s, c, h), h)

-- This function is used to map the previous function on every element of the drawing (composed of a list of transforms associated with shapes with colours and hierarchy)
-- so that we get a list of possible colors, and we will sort on Just values, and treat Nothing as a black pixel of the lowest hierarchy possible (0, given that all values of hierarchy are >0)
inside :: Point -> Drawing -> [(Maybe PixelRGB8, Int)]
inside p [] = []
inside p lst = map (inside1 p) lst

-- Filtering the list to do the previous action :
-- If the current element is a Just value, we get the value with the real hierarchy
-- If the current element is a Nothing value, we add a black pixel with the lowest possible hierarchy (0, given that all values of hierarchy are >0)
-- We use that function recursively on every possible pixel.
-- Also, Nothing -> Black pixel lowest hierarchy is used so that we do not actually have an empty list (from which we will get the first element when sorted...)
inside :: Point -> Drawing -> [(Maybe PixelRGB8, Int)]
filterList ::  [(Maybe PixelRGB8, Int)] -> [(PixelRGB8, Int)]
filterList [] = []
filterList ((p, h):q) = case p of
    Just x -> (x, h) : filterList q
    Nothing -> (PixelRGB8 0 0 0, -1):filterList q

-- Let's explain how I went to this function. We get a list of Pixels with hierarchy (possible black pixels in it)
-- We need to first get the second part of each element in the list [map snd lst]
-- Then we need to sort the list downwards on that part (upwards would work but it gives the opposite orders of shapes) [sortOn (Down . snd) lst]
-- Then we pop the highest value using head.
-- This head a tuple, we need to get the pixel part only, it's the first part so we use fst.
getPixelColourFromList :: [(PixelRGB8, Int)] -> PixelRGB8
getPixelColourFromList lst = fst (head (sortOn (Down . snd) lst))

-- Now we combine everything :
-- We need to get the colour from the filtered list of inside points of the drawing.
getPixelColour :: Point -> Drawing -> PixelRGB8
getPixelColour p lst = getPixelColourFromList (filterList (inside p lst))