{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Function
import Data.Functor.Identity
import Database.Id.Class (Id)
import Database.Id.Obelisk.Route (idPathSegmentEncoder)

import Obelisk.Route
import Obelisk.Route.TH

import Common.Schema

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_MkPaste :: BackendRoute ()
  BackendRoute_GetPaste :: BackendRoute (Id Paste)

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_ViewPaste :: FrontendRoute (Id Paste)

checkedFullRouteEncoder
  :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = checkEncoder fullRouteEncoder & \case
  Left err -> error $ T.unpack err
  Right encoder -> encoder

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_MkPaste -> PathSegment "create" $ unitEncoder mempty
      BackendRoute_GetPaste -> PathSegment "paste" idPathSegmentEncoder
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_ViewPaste -> PathSegment "p" idPathSegmentEncoder)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
