-- Basics for utils that will be used for shapes.
module Utils(
    Vector, Matrix, Point,
    projectX, projectY,
    point,
) where
-- Basic Class of vectors
data Vector = Vector Double Double
    deriving Show

vector :: Double -> Double -> Vector
vector x y = Vector x y

projectX :: Vector -> Double
projectX (Vector x y) = x

projectY :: Vector -> Double 
projectY (Vector x y) = y

-- Basic Class of Matrix
data Matrix = Matrix Vector Vector
    deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

-- Basic data type for Point
type Point = Vector

point :: Double -> Double -> Point
point = vector