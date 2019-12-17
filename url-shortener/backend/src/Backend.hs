{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.ByteString.Lazy
import Data.Text (Text)
import Data.Word
import Database.Id.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Snap.Core

maxUrlSize :: Word64
maxUrlSize = 2000

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS urls\
  \ (id SERIAL PRIMARY KEY, url VARCHAR(?) NOT NULL);"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDb "db" $ \pool -> do
        _ <- withResource pool $ \dbcon -> execute dbcon migration [maxUrlSize]
        serve $ \case

          BackendRoute_Get_Url :/ key -> do
            result <- liftIO $ withResource pool $ \dbcon ->
              query dbcon "SELECT (url) FROM urls WHERE id = ?;" [unId key]
            case result of
              [[url]] -> redirect url
              _ -> do
                modifyResponse $ setResponseStatus 404 "Not Found"
                modifyResponse $ setContentType "text/plain; charset=utf8"
                writeText "That shortlink wasn't found ¯\\_(ツ)_/¯"

          BackendRoute_Shorten :/ () -> do
            Just url <- A.decode <$> readRequestBody maxUrlSize
            [[key]] <- liftIO $ withResource pool $ \dbcon -> do
                _ <- execute dbcon "INSERT INTO urls (url) VALUES (?);" [url :: Text]
                query_ dbcon "SELECT last_value FROM urls_id_seq;"
            modifyResponse $ setResponseStatus 200 "OK"
            writeBS $ toStrict $ A.encode $ ("/s/" <>) $ show (key :: Int)

          _ -> redirect "/"
      return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
