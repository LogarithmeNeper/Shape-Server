-- Basics for utils that will be used for shapes.
module Utils(
    Vector, Matrix, Point,
    projectX, projectY, norm, dot,
    rotationMatrix, invert, transpose,
    prod,
    point,
) where
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