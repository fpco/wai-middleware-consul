{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasePrelude
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Logger ( runStdoutLoggingT )
import Network.Wai.Application.Static
    ( staticApp, defaultWebAppSettings )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Consul ( withConsul )
import Network.Wai.Middleware.Consul.GitHub ( gitHubPullOnWebhook )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )

main :: IO ()
main =
  runStdoutLoggingT
    (void (withConsul
             gitHubPullOnWebhook
             (\middleware ->
                liftIO (run 8080
                            (logStdoutDev $
                             middleware $
                             staticApp $
                             defaultWebAppSettings ".")))))
