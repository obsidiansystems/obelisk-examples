{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Map as M
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)

import Common.Api
import Common.Route
import Obelisk.Generated.Static

type AppWidget t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , Prerender t m
  )

type FrontendWidget t m =
  ( AppWidget t m
  , MonadJSM m
  , MonadJSM (Performable m)
  )

data State = NotStarted | Loading | Loaded (Maybe Text)
  deriving (Eq)

submitAttrs :: State -> M.Map Text Text
submitAttrs = \case
  Loading -> ("disabled" =: "true")
  _ -> mempty

urlInput :: AppWidget t m => m (Dynamic t State)
urlInput = do
  rec
    inputEl <- inputElement def
    (submitBtn,_) <- elAttr "span" ("id" =: "submit") $
      elDynAttr' "button" (submitAttrs <$> state) $ text "shorten"
    let click = domEvent Click submitBtn
    let url = tag (current $ _inputElement_value inputEl) click
    request <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (shortenRequest <$> url))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
  pure state

createShortLinkRoute :: Text
createShortLinkRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_Shorten :/ ()

shortenRequest :: Text -> XhrRequest Text
shortenRequest s = postJson createShortLinkRoute (ShortenRequest s)

createdLink :: AppWidget t m => State -> m ()
createdLink = \case
  NotStarted -> blank
  Loading -> text "shortening..."
  Loaded a -> case a of
    Nothing -> text "Error"
    Just url -> elAttr "a" ("href" =: url <> "target" =: "_blank") $ text url

app :: AppWidget t m => m ()
app = do
  state <- el "div" urlInput
  dyn_ $ createdLink <$> state

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Url Shortener"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css") blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: $(static "style.css")) blank
  , _frontend_body = app
  }
