{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Views where

import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

home :: H.Html
home = H.docTypeHtml $ do
    H.head $ do
             H.title "Tiny Shop"
    H.body $ do
             H.h1 "Welcome!"
             H.p "A cozy place to buy stuff but there's no pictures."