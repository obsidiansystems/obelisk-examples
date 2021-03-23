{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import GHC.Int
import Database.Id.Class (Id(..))
import qualified Data.Map as M
import Data.Text (Text)
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Api
import Common.Route
import Common.Schema

type AppWidget js t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PerformEvent t m
  , PostBuild t m
  , Prerender js t m
  , TriggerEvent t m
  )

type WidgetWithJS js t m =
  ( AppWidget js t m
  , HasJSContext (Performable m)
  , MonadJSM (Performable m)
  , MonadJSM m
  )

data State = NotStarted | Loading | Loaded (Maybe Int64)
  deriving (Eq)

pasteAttrs :: State -> M.Map Text Text
pasteAttrs = \case
  Loading -> ("disabled" =: "true")
  _ -> mempty

pasteInput :: AppWidget js t m => m (Dynamic t State)
pasteInput = do
  rec
    inputEl <- textAreaElement def
    (pasteBtn, _) <- elAttr "span" ("id" =: "paste") $
      elDynAttr' "button" (pasteAttrs <$> state) $ text "paste"
    let click = domEvent Click pasteBtn
    let paste = tag (current $ _textAreaElement_value inputEl) click
    request <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (pasteRequest <$> paste))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
  pure state

createPasteRoute :: Text
createPasteRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_MkPaste :/ ()

pasteRequest :: Text -> XhrRequest Text
pasteRequest s = postJson createPasteRoute (PasteRequest s)

viewPlaceholder :: AppWidget js t m => m ()
viewPlaceholder = text "Loading paste..."

showPasteResult :: AppWidget js t m => Maybe Text -> m ()
showPasteResult = \case
  Nothing -> text "Paste not found ¯\\_(ツ)_/¯"
  Just t -> el "pre" $ text t

getPasteUrl :: Id Paste -> Text
getPasteUrl pasteId = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_GetPaste :/ pasteId

viewClientSide :: WidgetWithJS js t m => Dynamic t (Id Paste) -> m ()
viewClientSide pasteId = do
  onload <- getPostBuild
  initialPasteLoad <- getAndDecode $ getPasteUrl <$> tag (current pasteId) onload
  widgetHold_ viewPlaceholder (showPasteResult <$> initialPasteLoad)

stateToMaybeKey :: State -> Maybe Int64
stateToMaybeKey = \case
  Loaded key -> key
  _ -> Nothing

app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main -> do
      state <- pasteInput
      setRoute $ (FrontendRoute_ViewPaste :/) . Id <$> fmapMaybe id (stateToMaybeKey <$> updated state)
    FrontendRoute_ViewPaste -> do
      pasteId <- askRoute
      prerender_
        viewPlaceholder
        (viewClientSide pasteId)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Pastebin"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css") blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"style.css") blank
  , _frontend_body = app
  }
