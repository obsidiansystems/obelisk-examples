{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Common.Route as R
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Word (Word64)
import Database.Id.Class (unId)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute_, Query, query)
import Gargoyle.PostgreSQL.Connect (withDb)
import Obelisk.Backend (Backend(..), _backend_run, _backend_routeEncoder)
import Obelisk.Route (pattern (:/))
import qualified Snap.Core as S

maxUrlSize :: Word64
maxUrlSize = 2000

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS urls\
  \ (id SERIAL PRIMARY KEY, url TEXT NOT NULL)"

backend :: Backend R.BackendRoute R.FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDb "db" $ \pool -> do
        _ <- withResource pool $ \dbcon -> execute_ dbcon migration
        serve $ \case

          R.BackendRoute_GetUrl :/ key -> do
            result <- liftIO $ withResource pool $ \dbcon ->
              query dbcon "SELECT (url) FROM urls WHERE id = ?" [unId key]
            case result of
              [[url]] -> S.redirect url
              _ -> do
                S.modifyResponse $ S.setResponseStatus 404 "Not Found"
                S.modifyResponse $ S.setContentType "text/plain; charset=utf8"
                S.writeText "That shortlink wasn't found ¯\\_(ツ)_/¯"

          R.BackendRoute_Shorten :/ () -> do
            Just url <- A.decode <$> S.readRequestBody maxUrlSize
            [[key]] <- liftIO $ withResource pool $ \dbcon ->
                query dbcon "INSERT INTO urls (url) VALUES (?) RETURNING id" [url :: Text]
            S.modifyResponse $ S.setResponseStatus 200 "OK"
            S.writeBS $ toStrict $ A.encode $ ("/s/" <>) $ show (key :: Int)

          _ -> S.redirect "/"
      return ()
  , _backend_routeEncoder = R.fullRouteEncoder
  }
