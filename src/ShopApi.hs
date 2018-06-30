{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ShopApi where

import Data.Proxy
import Data.Text
import Data.Int
import Servant.API
import Database.Persist
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

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