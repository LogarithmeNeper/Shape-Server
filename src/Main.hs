{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Necessary imports. 
import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

import Shapes
import Render (render,defaultWindow)
import Codec.Picture (PixelRGB8(PixelRGB8))

{-
Main function is at the bottom of the code.
-}

-- Shapes
-- Simple drawings (first part)
-- Circle
simpleCircleDrawing :: Drawing 
simpleCircleDrawing = [ (identity, (circle, PixelRGB8 255 0 0, 1)) ]
simpleCircleDrawingString :: H.Html
simpleCircleDrawingString = "[ (identity, (circle, PixelRGB8 255 0 0, 1)) ]"
-- Square
simpleSquareDrawing :: Drawing
simpleSquareDrawing = [ (identity, (square, PixelRGB8 0 255 0, 1)) ]
simpleSquareDrawingString :: H.Html
simpleSquareDrawingString = "[ (identity, (square, PixelRGB8 255 0 0, 1)) ]"
-- Rectangle
simpleRectangleDrawing :: Drawing
simpleRectangleDrawing = [ (identity, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleRectangleDrawingString :: H.Html
simpleRectangleDrawingString = "[ (identity, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"
-- Ellipse
simpleEllipseDrawing :: Drawing
simpleEllipseDrawing = [ (identity, (ellipse 1 0.25, PixelRGB8 0 255 0, 1)) ]
simpleEllipseDrawingString :: H.Html
simpleEllipseDrawingString = "[ (identity, (ellipse 1 0.25, PixelRGB8 0 255 0, 1)) ]"
-- Polygon (not really working)
simplePolygonDrawing :: Drawing
simplePolygonDrawing = [ (identity, (polygon [point 0 1, point 0.75 1, point 0.75 (-1), point (-0.75) (-1), point 0.75 (-1)], PixelRGB8 0 255 0, 1)) ]
simplePolygonDrawingString :: H.Html
simplePolygonDrawingString = "[ (identity, (polygon [point 0 1, point 0.75 1, point 0.75 (-1), point (-0.75) (-1), point 0.75 (-1)], PixelRGB8 0 255 0, 1)) ]"

-- Transformations
-- Scale
simpleScaleDrawing :: Drawing
simpleScaleDrawing = [ (scale (point 2 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleScaleDrawingString :: H.Html
simpleScaleDrawingString = "[ (scale (point 2 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"
-- Translate
simpleTranslateDrawing :: Drawing
simpleTranslateDrawing = [ (translate (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleTranslateDrawingString :: H.Html
simpleTranslateDrawingString = "[ (translate (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"
-- Rotate
simpleRotateDrawing :: Drawing
simpleRotateDrawing = [ (rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleRotateDrawingString :: H.Html
simpleRotateDrawingString = "[ (rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

-- Composition of transformations
-- Using every transformation possible of (Rotate, Translate, Scale) (*) (Rotate, Translate, Scale)
-- Not commenting every single drawing.
rotateRotateDrawing :: Drawing
rotateRotateDrawing = [ (rotate 45 <+> rotate 10, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
rotateRotateDrawingString :: H.Html
rotateRotateDrawingString = "[ (rotate 45 <+> rotate 10, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

translateTranslateDrawing :: Drawing
translateTranslateDrawing = [ (translate (point 0.5 0.5) <+> translate (point (-0.5) 0), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
translateTranslateDrawingString :: H.Html
translateTranslateDrawingString = "[ (translate (point 0.5 0.5) <+> translate (point (-0.5) 0), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

scaleScaleDrawing :: Drawing
scaleScaleDrawing = [ (scale (point 2 0.5) <+> scale (point 0.5 1), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
scaleScaleDrawingString :: H.Html
scaleScaleDrawingString = "[ (scale (point 2 0.5) <+> scale (point 0.5 1), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

rotateTranslateDrawing :: Drawing
rotateTranslateDrawing = [ (rotate 45 <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
rotateTranslateDrawingString :: H.Html
rotateTranslateDrawingString = "[ (rotate 45 <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

rotateScaleDrawing :: Drawing
rotateScaleDrawing = [ (rotate 45 <+> scale (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
rotateScaleDrawingString :: H.Html
rotateScaleDrawingString = "[ (rotate 45 <+> scale (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

translateRotateDrawing :: Drawing
translateRotateDrawing = [ (translate (point 0.5 (-0.5)) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
translateRotateDrawingString :: H.Html
translateRotateDrawingString = "[ (translate (point 0.5 (-0.5)) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

translateScaleDrawing :: Drawing
translateScaleDrawing = [ (translate (point 0.5 (-0.5)) <+> scale (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
translateScaleDrawingString :: H.Html
translateScaleDrawingString = "[ (translate (point 0.5 (-0.5)) <+> scale (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

scaleRotateDrawing :: Drawing
scaleRotateDrawing = [ (scale (point 2 0.5) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
scaleRotateDrawingString :: H.Html
scaleRotateDrawingString = "[ (scale (point 2 0.5) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

scaleTranslateDrawing :: Drawing
scaleTranslateDrawing = [ (scale (point 2 0.5) <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
scaleTranslateDrawingString :: H.Html
scaleTranslateDrawingString = "[ (scale (point 2 0.5) <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

-- Hierarchy for shapes
rectangleEllipseDrawing :: Drawing
rectangleEllipseDrawing = [(identity, (rectangle 2 0.5, PixelRGB8 255 0 0, 1)), (identity, (ellipse 1 2, PixelRGB8 0 255 0, 2))]
rectangleEllipseDrawingString :: H.Html
rectangleEllipseDrawingString = "[(identity, (rectangle 2 0.5, PixelRGB8 255 0 0, 1)), (identity, (ellipse 1 2, PixelRGB8 0 255 0, 2))]"

ellipseRectangleDrawing :: Drawing
ellipseRectangleDrawing = [(identity, (ellipse 1 2, PixelRGB8 0 255 0, 1)), (identity, (rectangle 2 0.5, PixelRGB8 255 0 0, 2))]
ellipseRectangleDrawingString :: H.Html
ellipseRectangleDrawingString = "[(identity, (ellipse 1 2, PixelRGB8 0 255 0, 1)), (identity, (rectangle 2 0.5, PixelRGB8 255 0 0, 2))]"

ellipseRectangleSquareDrawing :: Drawing
ellipseRectangleSquareDrawing = [(identity, (ellipse 1 2, PixelRGB8 0 255 0, 1)), (identity, (rectangle 2 0.5, PixelRGB8 255 0 0, 2)), (identity, (square, PixelRGB8 0 0 255, 3))]
ellipseRectangleSquareDrawingString :: H.Html
ellipseRectangleSquareDrawingString = "[(identity, (ellipse 1 2, PixelRGB8 0 255 0, 1)), (identity, (rectangle 2 0.5, PixelRGB8 255 0 0, 2)), (identity, (square, PixelRGB8 0 0 255, 3))]"

-- Everything mixed up : we tried to do a beautiful drawing. Not sure if it is, though.
firstDrawing :: Drawing
firstDrawing = [(scale (point 0.5 0.5) <+> translate (point (-2) 0), (rectangle 4 1, PixelRGB8 128 128 128, 1)), (scale (point 0.5 0.5) <+> translate (point 2 0), (rectangle 4 1, PixelRGB8 128 128 128, 2)), (identity, (square, PixelRGB8 36 70 142, 3)), (scale (point 0.66 0.66) <+> rotate 45, (square, PixelRGB8 140 87 156, 4)), (scale (point 0.33 0.33), (square, PixelRGB8 216 9 126, 5)), (scale (point 0.1 0.1),  (circle, PixelRGB8 128 128 128, 6))]
firstDrawingString :: H.Html
firstDrawingString = "[(scale (point 0.5 0.5) <+> translate (point (-2) 0), (rectangle 4 1, PixelRGB8 128 128 128, 1)), (scale (point 0.5 0.5) <+> translate (point 2 0), (rectangle 4 1, PixelRGB8 128 128 128, 2)), (identity, (square, PixelRGB8 36 70 142, 3)), (scale (point 0.66 0.66) <+> rotate 45, (square, PixelRGB8 140 87 156, 4)), (scale (point 0.33 0.33), (square, PixelRGB8 216 9 126, 5)), (scale (point 0.1 0.1),  (circle, PixelRGB8 128 128 128, 6))]"

-- This function is used to render every drawing defined before and put it in the img/ folder.
generate :: IO ()
generate = do
  -- Shapes
  render "img/simpleCircle.png" defaultWindow simpleCircleDrawing
  render "img/simpleSquare.png" defaultWindow simpleSquareDrawing
  render "img/simpleRectangle.png" defaultWindow simpleRectangleDrawing
  render "img/simpleEllipse.png" defaultWindow simpleEllipseDrawing
  render "img/simplePolygon.png" defaultWindow simplePolygonDrawing
  -- Transfromations
  render "img/simpleScale.png" defaultWindow simpleScaleDrawing
  render "img/simpleTranslate.png" defaultWindow simpleTranslateDrawing
  render "img/simpleRotate.png" defaultWindow simpleRotateDrawing
  -- Composition of transformations
  render "img/rotateRotate.png" defaultWindow rotateRotateDrawing
  render "img/rotateTranslate.png" defaultWindow rotateTranslateDrawing
  render "img/rotateScale.png" defaultWindow rotateScaleDrawing
  render "img/translateRotate.png" defaultWindow translateRotateDrawing
  render "img/translateTranslate.png" defaultWindow translateTranslateDrawing
  render "img/translateScale.png" defaultWindow translateScaleDrawing
  render "img/scaleRotate.png" defaultWindow scaleRotateDrawing
  render "img/scaleTranslate.png" defaultWindow scaleTranslateDrawing
  render "img/scaleScale.png" defaultWindow scaleScaleDrawing
  -- Hierarchy for shapes
  render "img/rectangleEllipse.png" defaultWindow rectangleEllipseDrawing
  render "img/ellipseRectangle.png" defaultWindow ellipseRectangleDrawing
  render "img/ellipseRectangleSquare.png" defaultWindow ellipseRectangleSquareDrawing
  -- Everything mixed up
  render "img/firstDrawing.png" defaultWindow firstDrawing

-- Starting Scotty server.
startServer :: IO ()
-- 3000 will be the port : use localhost:3000/
startServer = scotty 3000 $ do
  -- Defining pages with functions used to create HTML using Blaze library. See below for the most interesting parts.
  get "/" $ do
    html $ do R.renderHtml $ home
  
  get "/basic-shapes" $ do
    html $ do R.renderHtml $ basicShapes
  
  get "/basic-transformations" $ do
    html $ do R.renderHtml $ basicTransformations

  get "/compose-transformations" $ do
    html $ do R.renderHtml $ composeTransformations

  get "/hierarchy-shapes" $ do
    html $ do R.renderHtml $ hierarchyShapes

  get "/drawing" $ do
    html $ do R.renderHtml $ drawing

  -- Simple GET on img folder (populated by generate function, see above.)
  -- There are 21 of them.
  get "/img/simpleCircle.png" $ do
    file "img/simpleCircle.png"

  get "/img/simpleSquare.png" $ do
    file "img/simpleSquare.png"

  get "/img/simpleRectangle.png" $ do
    file "img/simpleRectangle.png"

  get "/img/simpleEllipse.png" $ do
    file "img/simpleEllipse.png"

  get "/img/simplePolygon.png" $ do
    file "img/simplePolygon.png"

  get "/img/simpleScale.png" $ do
    file "img/simpleScale.png"

  get "/img/simpleTranslate.png" $ do
    file "img/simpleTranslate.png"

  get "/img/simpleRotate.png" $ do
    file "img/simpleRotate.png"

  get "/img/rotateRotate.png" $ do
    file "img/rotateRotate.png"

  get "/img/rotateTranslate.png" $ do
    file "img/rotateTranslate.png"

  get "/img/rotateScale.png" $ do
    file "img/rotateScale.png"

  get "/img/translateRotate.png" $ do
    file "img/translateRotate.png"

  get "/img/translateTranslate.png" $ do
    file "img/translateTranslate.png"

  get "/img/translateScale.png" $ do
    file "img/translateScale.png"

  get "/img/scaleRotate.png" $ do
    file "img/scaleRotate.png"

  get "/img/scaleTranslate.png" $ do
    file "img/scaleTranslate.png"

  get "/img/scaleScale.png" $ do
    file "img/scaleScale.png"

  get "/img/rectangleEllipse.png" $ do
    file "img/rectangleEllipse.png"

  get "/img/ellipseRectangle.png" $ do
    file "img/ellipseRectangle.png"  

  get "/img/ellipseRectangleSquare.png" $ do
    file "img/ellipseRectangleSquare.png"

  get "/img/firstDrawing.png" $ do
    file "img/firstDrawing.png"

  get "/style/style.css" $ do
    file "style/style.css"

-- Structure taken from lectures
-- CSS loading from https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation (better saying where I found it, as I found it quite fun)
home :: H.Html
home = do
  -- Header
  H.head $ do
    -- Defining title of the page.
    H.title "Shape server"
    -- Importing CSS.
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  -- Body
  H.body $ do
    -- Welcome page.
    H.h1 "Welcome to our Haskell-written site!"
    H.p "This is a project written (almost) completely in Haskell (except for a CSS file) : it uses an eDSL for Shapes extended from what we saw during lectures."
    H.br 
    H.p "If you found this page, you are at the right place. However, shall you want to look at the code, you can either follow"
    -- Link to GitHub repo (external node).
    H.a H.! A.href "https://github.com/LogarithmeNeper/shape-server" $ H.span "this link"
    H.p "or look at the code in your favourite IDE (given that you downloaded it, which is the case). Below are links you can follow to see images, and code used to produce them"
    -- Navigation menu (links to internal nodes).
    H.ul $ do
      H.li $ do H.a H.! A.href "/basic-shapes" $ H.span "Basic Shapes"
      H.li $ do H.a H.! A.href "/basic-transformations" $ H.span "Basic Transformations"
      H.li $ do H.a H.! A.href "/compose-transformations" $ H.span "Composition of Transformations (minimal optimization)"
      H.li $ do H.a H.! A.href "/hierarchy-shapes" $ H.span "Hierarchy of Shapes"
      H.li $ do H.a H.! A.href "/drawing" $ H.span "All together"

-- The same exact structure applies to other pages, please refer to the above function if needed. Just commenting once on how we get to display an image.
basicShapes :: H.Html
basicShapes = do
  H.head $ do
    H.title "Basic Shapes"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Basic Shapes"
    H.p "This page is used to present the basic shapes of our project : Circle, Square, Ellipse, Rectangle, Polygon. We display the image and then the code used to generate it."
    H.br 
    H.a H.! A.href "/" $ H.span "Go back to main page"
    H.p "Circle"
    -- Define an img node and use the src attribute with a path, and an attribute alt if the image is not found (normally, it will work as the img folder exists.)
    H.img H.! A.src "../img/simpleCircle.png" H.! A.alt "Simple Circle."
    H.p simpleCircleDrawingString
    H.br
    H.p "Square"
    H.img H.! A.src "../img/simpleSquare.png" H.! A.alt "Simple Square."
    H.p simpleSquareDrawingString
    H.br
    H.p "Ellipse"
    H.img H.! A.src "../img/simpleEllipse.png" H.! A.alt "Simple Ellipse."
    H.p simpleEllipseDrawingString
    H.br
    H.p "Rectangle"
    H.img H.! A.src "../img/simpleRectangle.png" H.! A.alt "Simple Rectangle."
    H.p simpleRectangleDrawingString
    H.br
    H.p "Polygon"
    H.img H.! A.src "../img/simplePolygon.png" H.! A.alt "Simple Polygon."
    H.p simplePolygonDrawingString
    H.br

basicTransformations :: H.Html
basicTransformations = do
  H.head $ do
    H.title "Basic Transformations"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Basic Transformations"
    H.p "This page is used to present the basic transformations of our project : Scale, Translate, Rotate. We display the image and then the code used to generate it."
    H.br 
    H.a H.! A.href "/" $ H.span "Go back to main page"
    H.p "Reference Image"
    H.img H.! A.src "../img/simpleRectangle.png" H.! A.alt "Reference image."
    H.p simpleRectangleDrawingString
    H.br
    H.p "Scale"
    H.img H.! A.src "../img/simpleScale.png" H.! A.alt "Simple Scale."
    H.p simpleScaleDrawingString
    H.br
    H.p "Translate"
    H.img H.! A.src "../img/simpleTranslate.png" H.! A.alt "Simple Translate."
    H.p simpleTranslateDrawingString
    H.br
    H.p "Rotate"
    H.img H.! A.src "../img/simpleRotate.png" H.! A.alt "Simple Rotate."
    H.p simpleRotateDrawingString

composeTransformations :: H.Html 
composeTransformations = do
  H.head $ do
    H.title "Compose Transformations"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Compose Transformations"
    H.p "This page is used to present the compositions transformations of our project. We display the image and then the code used to generate it."
    H.br 
    H.a H.! A.href "/" $ H.span "Go back to main page"
    H.p "Reference Image"
    H.img H.! A.src "../img/simpleRectangle.png" H.! A.alt "Reference image."
    H.p simpleRectangleDrawingString
    H.br
    -- Rotate
    H.p "Rotate -> Rotate (optimized)"
    H.img H.! A.src "../img/rotateRotate.png" H.! A.alt "Rotate -> Rotate."
    H.p rotateRotateDrawingString
    H.br
    H.p "Rotate -> Translate"
    H.img H.! A.src "../img/rotateTranslate.png" H.! A.alt "Rotate -> Translate."
    H.p rotateTranslateDrawingString
    H.br
    H.p "Rotate -> Scale"
    H.img H.! A.src "../img/rotateScale.png" H.! A.alt "Rotate -> Scale."
    H.p rotateScaleDrawingString
    H.br
    -- Translate
    H.p "Translate -> Rotate"
    H.img H.! A.src "../img/translateRotate.png" H.! A.alt "Translate -> Rotate."
    H.p translateRotateDrawingString
    H.br
    H.p "Translate -> Translate (optimized)"
    H.img H.! A.src "../img/translateTranslate.png" H.! A.alt "Translate -> Translate."
    H.p translateTranslateDrawingString
    H.br
    H.p "Translate -> Scale"
    H.img H.! A.src "../img/translateScale.png" H.! A.alt "Translate -> Scale."
    H.p translateScaleDrawingString
    H.br
    -- Scale
    H.p "Scale -> Rotate"
    H.img H.! A.src "../img/scaleRotate.png" H.! A.alt "Scale -> Rotate."
    H.p scaleRotateDrawingString
    H.br
    H.p "Scale -> Translate"
    H.img H.! A.src "../img/scaleTranslate.png" H.! A.alt "Scale -> Translate."
    H.p scaleTranslateDrawingString
    H.br
    H.p "Scale -> Scale (optimized)"
    H.img H.! A.src "../img/scaleScale.png" H.! A.alt "Scale -> Scale."
    H.p scaleScaleDrawingString

hierarchyShapes :: H.Html 
hierarchyShapes = do
  H.head $ do
    H.title "Shapes hierarchy"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Hierarchy of Shapes"
    H.p "This page is used to present the hierarchy for shapes in our project. We display the image and then the code used to generate it."
    H.br 
    H.a H.! A.href "/" $ H.span "Go back to main page"
    H.p "Rectangle < Ellipse"
    H.img H.! A.src "../img/rectangleEllipse.png" H.! A.alt "Rectangle < Ellipse"
    H.p rectangleEllipseDrawingString
    H.br
    H.p "Ellipse < Rectangle"
    H.img H.! A.src "../img/ellipseRectangle.png" H.! A.alt "Ellipse < Rectangle"
    H.p ellipseRectangleDrawingString
    H.br
    H.p "Ellipse < Rectangle < Square"
    H.img H.! A.src "../img/ellipseRectangleSquare.png" H.! A.alt "Ellipse < Rectangle < Square"
    H.p ellipseRectangleSquareDrawingString
    H.br

drawing :: H.Html 
drawing = do
  H.head $ do
    H.title "Simple Drawing"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Simple drawing"
    H.p "Just a simple page to display a drawing that uses almost everything in the project."
    H.br 
    H.a H.! A.href "/" $ H.span "Go back to main page"
    H.p "Drawing."
    H.img H.! A.src "../img/firstDrawing.png" H.! A.alt "Drawing"
    H.p firstDrawingString

-- Main function : 
-- Generates the shapes in the img folder, then starts server.
main :: IO ()
main = do generate
          startServer