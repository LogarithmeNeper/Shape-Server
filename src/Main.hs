{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

import Shapes
import Render (render,defaultWindow)
import Codec.Picture (PixelRGB8(PixelRGB8))
import Text.Blaze.Html4.FrameSet.Attributes (start)

-- Shapes
simpleCircleDrawing = [ (identity, (circle, PixelRGB8 255 0 0, 1)) ]
simpleCircleDrawingString = "[ (identity, (circle, PixelRGB8 255 0 0, 1)) ]"

simpleSquareDrawing = [ (identity, (square, PixelRGB8 0 255 0, 1)) ]
simpleSquareDrawingString = "[ (identity, (square, PixelRGB8 255 0 0, 1)) ]"

simpleRectangleDrawing = [ (identity, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleRectangleDrawingString = "[ (identity, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

simpleEllipseDrawing = [ (identity, (ellipse 1 0.25, PixelRGB8 0 255 0, 1)) ]
simpleEllipseDrawingString = "[ (identity, (ellipse 1 0.25, PixelRGB8 0 255 0, 1)) ]"

simplePolygonDrawing = [ (identity, (polygon [point 0 1, point 0.75 1, point 0.75 (-1), point (-0.75) (-1), point 0.75 (-1)], PixelRGB8 0 255 0, 1)) ]
simplePolygonDrawingString = "[ (identity, (polygon [point 0 1, point 0.75 1, point 0.75 (-1), point (-0.75) (-1), point 0.75 (-1)], PixelRGB8 0 255 0, 1)) ]"

-- Transformations
simpleScaleDrawing = [ (scale (point 2 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleScaleDrawingString = "[ (scale (point 2 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

simpleTranslateDrawing = [ (translate (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleTranslateDrawingString = "[ (translate (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

simpleRotateDrawing = [ (rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
simpleRotateDrawingString = "[ (rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

-- Composition of transformations
rotateRotateDrawing = [ (rotate 45 <+> rotate 10, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
rotateRotateDrawingString = "[ (rotate 45 <+> rotate 10, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

translateTranslateDrawing = [ (translate (point 0.5 0.5) <+> translate (point (-0.5) 0), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
translateTranslateDrawingString = "[ (translate (point 0.5 0.5) <+> translate (point (-0.5) 0), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

scaleScaleDrawing = [ (scale (point 2 0.5) <+> scale (point 0.5 1), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
scaleScaleDrawingString = "[ (scale (point 2 0.5) <+> scale (point 0.5 1), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

rotateTranslateDrawing = [ (rotate 45 <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
rotateTranslateDrawingString = "[ (rotate 45 <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

rotateScaleDrawing = [ (rotate 45 <+> scale (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
rotateScaleDrawingString = "[ (rotate 45 <+> scale (point 0.5 0.5), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

translateRotateDrawing = [ (translate (point 0.5 (-0.5)) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
translateRotateDrawingString = "[ (translate (point 0.5 (-0.5)) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

translateScaleDrawing = [ (rotate 45 <+> scale (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
translateScaleDrawingString = "[ (translate (point 0.5 (-0.5)) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

scaleRotateDrawing = [ (scale (point 2 0.5) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
scaleRotateDrawingString = "[ (scale (point 2 0.5) <+> rotate 45, (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

scaleTranslateDrawing = [ (scale (point 2 0.5) <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]
scaleTranslateDrawingString = "[ (scale (point 2 0.5) <+> translate (point 0.5 (-0.5)), (rectangle 1 0.25, PixelRGB8 255 0 0, 1)) ]"

-- Hierarchy for shapes
rectangleEllipseDrawing = [(identity, (rectangle 1 4, PixelRGB8 255 0 0, 1)), (identity, (ellipse 1 4, PixelRGB8 0 255 0, 2))]
rectangleEllipseDrawingString = "[(identity, (rectangle 1 4, PixelRGB8 255 0 0, 1)), (identity, (ellipse 1 4, PixelRGB8 0 255 0, 2))]"

ellipseRectangleDrawing = [(identity, (ellipse 1 4, PixelRGB8 0 255 0, 1)), (identity, (rectangle 1 4, PixelRGB8 255 0 0, 2))]
ellipseRectangleDrawingString = "[(identity, (ellipse 1 4, PixelRGB8 0 255 0, 1)), (identity, (rectangle 1 4, PixelRGB8 255 0 0, 2))]"

ellipseRectangleSquareDrawing = [(identity, (ellipse 1 4, PixelRGB8 0 255 0, 1)), (identity, (rectangle 1 4, PixelRGB8 255 0 0, 2)), (identity, (square, PixelRGB8 0 0 255, 3))]
ellipseRectangleSquareDrawingString = "[(identity, (ellipse 1 4, PixelRGB8 0 255 0, 1)), (identity, (rectangle 1 4, PixelRGB8 255 0 0, 2)), (identity, (square, PixelRGB8 0 0 255, 3))]"

-- Everything mixed up
firstDrawing = [(scale (point 0.5 0.5) <+> translate (point (-2) 0), (rectangle 4 1, PixelRGB8 128 128 128, 1)), (scale (point 0.5 0.5) <+> translate (point 2 0), (rectangle 4 1, PixelRGB8 128 128 128, 2)), (identity, (square, PixelRGB8 36 70 142, 3)), (scale (point 0.66 0.66) <+> rotate 45, (square, PixelRGB8 140 87 156, 4)), (scale (point 0.33 0.33), (square, PixelRGB8 216 9 126, 5)), (scale (point 0.1 0.1),  (circle, PixelRGB8 128 128 128, 6))]

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

startServer :: IO ()
startServer = scotty 3000 $ do
  get "/" $ do
    html $ do R.renderHtml $ home
  
  get "/basic-shapes" $ do
    html $ do R.renderHtml $ basicShapes
  
  get "/img" $ do
    html $ display

  -- getting images for web server
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

  get "/img/lambda.jpg" $ do
    file "img/lambda.jpg"

  get "/style/style.css" $ do
    file "style/style.css"

-- Structure taken from lectures
-- CSS loading from https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation (better saying where I found it, as I found it quite fun)
home :: H.Html
home = do
  H.head $ do
    H.title "Shape server"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Welcome to our Haskell-written site!"
    H.img H.! A.src "../img/lambda.jpg" H.! A.alt "Haskell logo"
    H.p "This is a project written (almost) completely in Haskell (except for a CSS file) : it uses an eDSL for Shapes extended from what we saw during lectures."
    H.br 
    H.p "If you found this page, you are at the right place. However, shall you want to look at the code, you can either follow" 
    H.a H.! A.href "https://github.com/LogarithmeNeper/shape-server" $ H.span "this link"
    H.p "or look at the code in your favourite IDE (given that you downloaded it, which is the case). Below are links you can follow to see images, and code used to produce them"
    H.ul $ do
      H.li $ do H.a H.! A.href "/basic-shapes" $ H.span "Basic Shapes"
      H.li "Second item"
      H.li "Third item"
    
basicShapes :: H.Html
basicShapes = do
  H.head $ do
    H.title "Basic Shapes"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Basic Shapes"
    H.p "This page is to present the basic shapes of our project : Circle, Square, Ellipse, Rectangle, Polygon. We display the image and then the code used to generate it."
    H.br 
    H.a H.! A.href "/" $ H.span "Go back to main page"
    H.p "Circle"
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

display :: Text
display = do R.renderHtml $ do myImage

myImage :: H.Html 
myImage = H.img H.! A.src "../img/output.png" H.! A.alt "Contemporary art."

main :: IO ()
main = do generate
          startServer