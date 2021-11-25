{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R

main = scotty 3000 $ do
  get "/" $ do
    html $ link
  
  get "/img" $ do
    html $ display

  get "/img/output.png" $ do
    file "img/output.png"

link :: Text
link = do R.renderHtml $ do myList

myList :: H.Html
myList = H.a H.! A.href "/img" $ H.span "Texte"

display :: Text
display = do R.renderHtml $ do myImage

myImage :: H.Html 
myImage = H.img H.! A.src "../img/output.png" H.! A.alt "Contemporary art."