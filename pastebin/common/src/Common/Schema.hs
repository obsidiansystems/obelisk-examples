{-# LANGUAGE DeriveGeneric #-}
module Common.Schema where

import Data.Text (Text)
import Database.Id.Class
import GHC.Generics

newtype Paste = Paste { unPaste :: Text }
  deriving Generic

instance HasId Paste
