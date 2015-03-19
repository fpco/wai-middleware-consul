{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import BasePrelude
import Control.Logging ( withStdoutLogging )
import Network.Wai.Application.Static
    ( staticApp, defaultWebAppSettings )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Consul ( withConsul )
import Network.Wai.Middleware.Consul.GitHub ( gitHubPullOnWebhook )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )

main :: IO ()
main =
  withStdoutLogging
    (withConsul
       gitHubPullOnWebhook
       (\middleware ->
          run 8080
              (logStdoutDev $
               middleware $
               staticApp $
               defaultWebAppSettings ".")))
