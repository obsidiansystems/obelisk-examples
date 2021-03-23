Backend run-time configuration
==============================

Configuration file locations
----------------------------

Configuration files that are specific to the backend should be placed in `config/backend`. This is especially important if those files contain sensitive information.

The backend has access to the files in `config/frontend` and `config/common` as well. The frontend, by contrast, only has access to `config/frontend` and `config/common`.

Using configuration files
-------------------------

We're going to build an obelisk backend that retrieves configuration files and prints them to the console. Of course, it could something more useful with them, but this should suffice.

In our example, we've created two configuration files:

```
config
└── backend
    ├── record
    └── string_config
```

The `record` file contains a json-encoded `Record` (defined below). The `string_config` file contains a string.

Let's set up our backend module:

```haskell

> {-# Language DeriveGeneric #-}
> {-# Language OverloadedStrings #-}
> {-# Language ScopedTypeVariables #-}
> module Backend where
> 
> import Common.Route
> import Control.Monad.IO.Class
> import Data.Aeson as Aeson
> import qualified Data.Text as T
> import GHC.Generics
> import Obelisk.Backend

```

In the backend module of your obelisk application, you'll need to import the `Obelisk.ExecutableConfig.Lookup`. This module knows where to look for configuration files and retrieves them.

You'll also want `Obelisk.Configs`, which provides a configuration file lookup monad and some other conveniences.

```haskell

> import Obelisk.Configs
> import qualified Obelisk.ExecutableConfig.Lookup as ExeCfg
> 
> backend :: Backend BackendRoute FrontendRoute
> backend = Backend
>   { _backend_run = \serve -> do

```
Inside of the backend runner function, we'll retrieve our configuration files:

```haskell

>     cfgs <- liftIO ExeCfg.getConfigs
>     serve $ \_ -> do

```

Next we'll run the configuration transformer. You don't necessarily have to do this, as you can just pull configs out of the
map produced by `ExeCfg.getConfigs`.

`ConfigsT` does give us access to some useful functions for looking up particular configs. We'll use some functions from `Obelisk.Configs` to look up configuration values. 'getTextConfig' is for configuration files containing text values. 'getConfig' by contrast returns a bytestring representing the entire config file.

```haskell

>     runConfigsT cfgs $ do
>       string_config <- getTextConfig "backend/string_config"

```
Our `record` configuration file is json-encoded, so we'll read it as a bytestring and try to decode it.

```haskell

>       record_config :: (Maybe Record) <- (Aeson.decodeStrict =<<) <$> getConfig "backend/record"

```
Next we'll print information about the configuration files. Since this is running inside of the backend handler, refreshing the browser ought to cause these values to be printed to the `ob run` console.

```haskell

>       liftIO $ do
>         putStr "String config: "
>         putStrLn $ maybe "<not found>" T.unpack string_config
>         putStr "Record config: "
>         putStrLn $ maybe "<not found>" show record_config
>   , _backend_routeEncoder = fullRouteEncoder
>   }

```

Here's the record type we used in our config file.

```haskell

> 
> data Record = Record
>   { field1 :: Int
>   , field2 :: Char
>   }
>   deriving (Generic, Show)
> 
> instance ToJSON Record
> instance FromJSON Record

```
