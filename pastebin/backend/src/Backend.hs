{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Common.Route as R
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Database.Id.Class (Id(..), unId)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute_, Query, query)
import Gargoyle.PostgreSQL.Connect (withDb)
import Obelisk.Backend (Backend(..), _backend_run, _backend_routeEncoder)
import Obelisk.Route (renderFrontendRoute, pattern (:/))
import qualified Snap.Core as S

maxPasteSize :: Word64
maxPasteSize = 10000

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS pastes\
  \ (id SERIAL PRIMARY KEY, paste TEXT NOT NULL)"

backend :: Backend R.BackendRoute R.FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDb "db" $ \pool -> do
        _ <- withResource pool $ \dbcon -> execute_ dbcon migration
        serve $ \case

          R.BackendRoute_GetPaste :/ key -> do
            result <- liftIO $ withResource pool $ \dbcon ->
              query dbcon "SELECT (paste) FROM pastes WHERE id = ?" [unId key]
            case (result :: [[Text]]) of
              [[paste]] -> S.writeBS $ toStrict $ A.encode paste
              _ -> S.modifyResponse $ S.setResponseStatus 404 "Not Found"

          R.BackendRoute_MkPaste :/ () -> do
            Just paste <- A.decode <$> S.readRequestBody maxPasteSize
            [[key]] <- liftIO $ withResource pool $ \dbcon ->
                query dbcon "INSERT INTO pastes (paste) VALUES (?) RETURNING id" [paste :: Text]
            S.modifyResponse $ S.setResponseStatus 200 "OK"
            S.writeBS $ toStrict $ A.encode (key :: Int)

          _ -> S.redirect $ encodeUtf8 $ renderFrontendRoute R.checkedFullRouteEncoder $
            R.FrontendRoute_Main :/ ()
      return ()
  , _backend_routeEncoder = R.fullRouteEncoder
  }
