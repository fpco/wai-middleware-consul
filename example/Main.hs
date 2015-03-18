{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasePrelude
import Control.Concurrent.Async ( race_ )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Consul
    ( mkConsulProxy, mkConsulWatch )
import Network.Wai.Middleware.Consul.GitHub ( gitHubPullOnWebhook )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Application.Static
    ( staticApp, defaultWebAppSettings )

main :: IO ()
main =
  race_ (mkConsulWatch gitHubPullOnWebhook)
        (do middleware <- mkConsulProxy gitHubPullOnWebhook
            run 8080 $
              logStdoutDev $ middleware $ staticApp $ defaultWebAppSettings ".")
