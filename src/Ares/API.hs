{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ares.API
    ( API
    , api
    )
  where

import Servant
import Ares.App

type API =
    "factory-reset" :>
        Post '[] () :<|>
    "logs" :>
        Get '[JSON] [FilePath] :<|>
    "reload" :>
        Post '[] () :<|>
    "stop" :>
        Post '[] () :<|>
    "apps" :> (
        Get '[JSON] [App] :<|>
        Capture "name" AppName :> (
            Get '[JSON] (Maybe App) :<|>
            ReqBody '[FormUrlEncoded] AppPath :>
                Put '[JSON] (Maybe App) :<|>
            Delete '[JSON] Bool ))

api :: Proxy API
api = Proxy
