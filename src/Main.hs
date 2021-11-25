module Main where

import Shapes
import Render (render,defaultWindow)
import Codec.Picture (PixelRGB8 (PixelRGB8))

exampleDrawing =  [ (identity, (rectangle 1 0.25, PixelRGB8 45 10 80, 1)), ((scale (point 0.5 0.25) <+> translate (point 1.2 0.4)), (ellipse 1 1, PixelRGB8 90 10 0, 2)) ]

main = render "output.png" defaultWindow exampleDrawing

{-

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty 
main = scotty 3000 $ do 
  get "/" $ do 
    html "Hello World!"

-}