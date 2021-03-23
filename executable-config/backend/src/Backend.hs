{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
module Backend where

import Common.Route
import Control.Monad.IO.Class
import Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics
import Obelisk.Backend
import Obelisk.Configs
import qualified Obelisk.ExecutableConfig.Lookup as ExeCfg

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \_ -> do
    -- Retrieve all of the configuration files
    cfgs <- liftIO ExeCfg.getConfigs
    -- Run the configuration transformer. You don't necessarily
    -- have to do this, as you can just pull configs out of the
    -- 'Map' produced on the previous line manually.
    runConfigsT cfgs $ do
      -- There are several functions in Obelisk.Configs for looking up
      -- configuration values. 'getTextConfig' is for configuration files
      -- containing text values. 'getConfig' by contrast returns a bytestring
      -- representing the entire config file.
      string_config <- getTextConfig "backend/string_config"
      record_config :: (Maybe Record) <- (Aeson.decodeStrict =<<) <$> getConfig "backend/record"
      liftIO $ do
        putStr "String config: "
        putStrLn $ maybe "<not found>" T.unpack string_config
        putStr "Record config: "
        putStrLn $ maybe "<not found>" show record_config
  , _backend_routeEncoder = fullRouteEncoder
  }

data Record = Record
  { field1 :: Int
  , field2 :: Char
  }
  deriving (Generic, Show)

instance ToJSON Record
instance FromJSON Record

printRecord :: Record -> String
printRecord (Record f1 f2) = show f1 <> " " <> [f2]
