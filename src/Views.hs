{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Views where

import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

home :: H.Html
home = H.docTypeHtml $ do
    H.head $ do
             H.title "Tiny Shop"
             H.link H.! rel "stylesheet" H.! type_ "text/css" H.! href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"
    H.body $ do
             H.h1 "Welcome!"
             H.p "A cozy place to buy stuff but there's no pictures."

catalog :: H.Html
catalog = H.docTypeHtml $ do
    H.head $ do
             H.title "Catalog"

item :: H.Html
item = H.docTypeHtml $ do
    H.head $ do
             H.title "Item"

cart :: H.Html
cart = H.docTypeHtml $ do
    H.head $ do
             H.title "Cart"

checkout :: H.Html
checkout = H.docTypeHtml $ do
    H.head $ do
             H.title "Checkout"

dashboard :: H.Html
dashboard = H.docTypeHtml $ do
    H.head $ do
             H.title "Dashboard"