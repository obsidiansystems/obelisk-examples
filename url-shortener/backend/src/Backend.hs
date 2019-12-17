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
import Prelude hiding (id)
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

          BackendRoute_Get_Url :/ id -> do
            result <- liftIO $ withResource pool $ \dbcon ->
              query dbcon "SELECT (url) FROM urls WHERE id = ?;" [unId id]
            case result of
              [[url]] -> redirect url
              _ -> do
                modifyResponse $ setResponseStatus 404 "Not Found"
                writeText "That shortlink wasn't found ¯\\_(ツ)_/¯"
                r <- getResponse
                finishWith $ setContentType "text/plain; charset=utf8" r

          BackendRoute_Shorten :/ () -> do
            Just url <- A.decode <$> readRequestBody maxUrlSize
            [[id]] <- liftIO $ withResource pool $ \dbcon -> do
                _ <- execute dbcon "INSERT INTO urls (url) values (?);" [url :: Text]
                query_ dbcon "SELECT last_value FROM urls_id_seq;"
            liftIO $ threadDelay 1000000
            modifyResponse $ setResponseStatus 200 "OK"
            writeBS $ toStrict $ A.encode $ ("/s/" <>) $ show (id :: Int)
            r <- getResponse
            finishWith r

          _ -> redirect "/"
      return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
