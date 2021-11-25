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

  get "/img/lambda.jpg" $ do
    file "img/lambda.jpg"

  get "/style/style.css" $ do
    file "style/style.css"

-- Structure taken from lectures
-- CSS loading from https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation (better saying where I found it, as I found it quite fun)
home :: H.Html
home = do
  H.head $ do
    H.title "Our Page"
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
      H.li $ do H.a H.! A.href "/img" $ H.span "Texte"
      H.li "Second item"
      H.li "Third item"
    

display :: Text
display = do R.renderHtml $ do myImage

myImage :: H.Html 
myImage = H.img H.! A.src "../img/output.png" H.! A.alt "Contemporary art."
