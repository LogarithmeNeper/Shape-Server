module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ (identity, rectangle 1 0.25) ]

main = render "output.png" defaultWindow exampleDrawing

{-

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty 
main = scotty 3000 $ do 
  get "/" $ do 
    html "Hello World!"

-}