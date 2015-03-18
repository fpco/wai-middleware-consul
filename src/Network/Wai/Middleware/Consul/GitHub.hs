{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Middleware.Consul.GitHub
       (gitHubPullOnWebhook)
       where

import           BasePrelude
import qualified Data.ByteString as B
import           Network.HTTP.Types
import           Network.Socket
import           Network.Wai
import           Network.Wai.Middleware.Consul (ConsulSettings(..))
import           System.Process

gitHubPullOnWebhook :: ConsulSettings
gitHubPullOnWebhook =
  ConsulSettings {csHost = "0.0.0.0"
                 ,csPort = PortNum 8500
                 ,csKey = "github"
                 ,csFilter = isGitHubWebhook
                 ,csCallback = \_ -> callProcess "git" ["pull"] }

isGitHubWebhook :: Request -> Bool
isGitHubWebhook req =
  (requestMethod req == methodPost) &&
  -- https://developer.github.com/webhooks/#delivery-headers "When
  -- one of those events is triggered, we’ll send a HTTP POST
  -- payload to the webhook’s configured URL."
  (pathInfo req ==
   ["github"]) &&
  -- "X-Github-Event: Name of the event that triggered this
  -- delivery."
  (lookup' "X-Github-Event" (requestHeaders req) ==
   ["push"]) &&
  -- "X-Hub-Signature: HMAC hex digest of the payload, using the
  -- hook’s secret as the key (if configured)."
  (lookup' "X-Hub-Signature" (requestHeaders req) /=
   []) && -- FIXME: Validate HMAC
  -- "X-Github-Delivery: Unique ID for this delivery."
  (lookup' "X-Github-Delivery" (requestHeaders req) /=
   []) &&
  -- "Also, the User-Agent for the requests will have the prefix
  -- GitHub-Hookshot/"
  (all (B.isPrefixOf "GitHub-Hookshot/")
       (lookup' hUserAgent $ requestHeaders req))
  where
    -- Stolen from Yesod.Core
    lookup' a = map snd . filter (\x -> a == fst x)
