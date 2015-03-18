{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Consul
    ( withConsul )
import Network.Wai.Middleware.Consul.GitHub ( gitHubPullOnWebhook )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Application.Static
    ( staticApp, defaultWebAppSettings )

main :: IO (Either () ())
main =
  withConsul
    gitHubPullOnWebhook
    (\middleware ->
       run 8080
           (logStdoutDev $
            middleware $
            staticApp $
            defaultWebAppSettings "."))
