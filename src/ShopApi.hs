{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ShopApi where

import Data.Proxy
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import qualified Data.Text as T
import Data.Int
import Servant.API
import Servant.JS
import Database.Persist
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Language.Javascript.JQuery

import Models
import Views

type ShopApi = "add" :> ReqBody '[JSON] Item :> Put '[JSON] (Maybe (Key Item))
  :<|> "one" :> Capture "id" Int64 :> Get '[JSON] (Maybe Item)
  :<|> "modify" :> ReqBody '[JSON] Item :> Patch '[JSON] [Char]
  :<|> "remove" :> Capture "id" Int64 :> Delete '[JSON] [Char]
  :<|> "all" :> Get '[JSON] ([Item])
  :<|> "home" :> Get '[HTML] H.Html
  -- :<|> "catalog" :> Get '[HTML] H.Html
  -- :<|> "item" :> Get '[HTML] H.Html
  -- :<|> "cart" :> Get '[HTML] H.Html
  -- :<|> "checkout" :> Get '[HTML] H.Html
  -- :<|> "dashboard" :> Get '[HTML] H.Html

shopApi :: Proxy ShopApi
shopApi = Proxy
{-
apiToJs :: Text
apiToJs = jsForAPI shopApi jquery

writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiToJs
  jq <- T.readFile =<< Language.Javascript.JQuery.file
  T.writeFile "static/jq.js" jq-}