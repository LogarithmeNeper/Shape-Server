{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

main = scotty 3000 $ do
  get "/" $ do
    html $ do R.renderHtml $ home
  
  get "/img" $ do
    html $ display

  get "/img/output.png" $ do
    file "img/output.png"

  get "/style/style.css" $ do
    file "style/style.css"

  get "/favicon.ico" $ do
    file "favicon.ico"

-- Taken from https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation 
home :: H.Html
home = do
  H.head $ do
    H.title "Our Page"
    H.link H.! A.rel "stylesheet" H.! A.href "style/style.css"
  H.body $ do
    H.h1 "Welcome to our Haskell-written site!"
    H.a H.! A.href "/img" $ H.span "Texte"

display :: Text
display = do R.renderHtml $ do myImage

myImage :: H.Html 
myImage = H.img H.! A.src "../img/output.png" H.! A.alt "Contemporary art."
