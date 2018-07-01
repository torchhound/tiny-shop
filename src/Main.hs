{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Servant
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite ( ConnectionPool, createSqlitePool
                               , runSqlPool, runSqlPersistMPool
                               , runMigration, selectFirst, (==.)
                               , insert, entityVal, deleteWhere
                               , updateWhere, (*=.), selectList
                               , toSqlKey, Filter)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Int
import Network.Wai.Handler.Warp as Warp

import ShopApi
import Models
import qualified Views as V

server :: ConnectionPool -> Server ShopApi
server pool = _itemPost 
  :<|> _itemGet 
  :<|> _itemPatch 
  :<|> _itemDelete
  :<|> _itemAll

  where
    _itemPost newItem = liftIO $ itemPost newItem
    _itemGet id = liftIO $ itemGet id
    _itemPatch patchItem = liftIO $ itemPatch patchItem
    _itemDelete id = liftIO $ itemDelete id
    _itemAll = liftIO itemAll

    itemPost :: Item -> IO (Maybe (Key Item))
    itemPost newItem = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [ItemName ==. (itemName newItem)] []
      case exists of 
        Nothing -> Just <$> insert newItem
        Just _ -> return Nothing

    itemGet :: Int64 -> IO (Maybe Item)
    itemGet id = flip runSqlPersistMPool pool $ do
      _Item <- selectFirst [ItemId ==. (toSqlKey id)] []
      return $ entityVal <$> _Item

    itemPatch :: Item -> IO ([Char])
    itemPatch patchItem = flip runSqlPersistMPool pool $ do
      updateWhere [ItemName ==. (itemName patchItem)] [ItemName *=. (itemName patchItem), 
        ItemDescription *=. (itemDescription patchItem)]
      return "Item Patched"

    itemDelete :: Int64 -> IO ([Char])
    itemDelete id = flip runSqlPersistMPool pool $ do
      deleteWhere [ItemId ==. (toSqlKey id)]
      return "Item Deleted"

    itemAll :: IO [Item]
    itemAll = flip runSqlPersistMPool pool $ do
      _Items <- selectList ([] :: [Filter Item]) []
      return $ entityVal <$> _Items

server' :: ConnectionPool -> Server ViewApi
server' pool = server pool
  :<|> return V.home

app :: ConnectionPool -> Application
app pool = serve viewApi $ server' pool

mkApp :: FilePath -> IO Application
mkApp file = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs file) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

sprint :: FilePath -> IO ()
sprint file = Warp.run 8081 =<< mkApp file

main :: IO ()
main = do
  writeJsFiles
  sprint "sqlite.db"