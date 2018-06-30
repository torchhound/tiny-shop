{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.Aeson
import Data.Text
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  username Text
  password Text
  UniqueUserName username
  deriving Eq Read Show
Item
  name Text
  description Text
  UniqueProductName name
  deriving Eq Read Show
|]

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "username"
             <*> v .: "password"

instance ToJSON User where
  toJSON (User username password) =
    object [ "username" .= username
           , "password" .= password ]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \ v ->
    Item <$> v .: "name"
             <*> v .: "description"

instance ToJSON Item where
  toJSON (Item name description) =
    object [ "name" .= name
           , "description" .= description ]      