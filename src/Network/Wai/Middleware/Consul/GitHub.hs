{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Network.Wai.Middleware.Consul
Description : WAI Middleware for Consul
Copyright   : (c) FPComplete, 2015
License     : MIT
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX

This module contains specific configuration for using WAI Middleware
for Consul with Github. We can configure a GitHub Webhook to POST to
consul middleware.  Consul middleware will push the GitHub webhook
payload to Consul. Consul will let every WAI application know of the
data update & each application will execute `git pull` to update their
git repository contents.

@
      ┌─────────┐      ┌─────────┐
      │ Github  │      │         │
      │  Repo   │─────▶│ AWS ELB │
      │ Webhook │      │         │
      └─────────┘      └─────────┘
                            │
        ┌────────────┬──────┘─ ─ ─
        │                         │
        ▼            ▼            ▼
   ┌─────────┐  ┌─────────┐  ┌─────────┐
   │         │  │         │  │         │
┌──│ WAI App │  │ WAI App │  │ WAI App │
│  │         │  │         │  │         │
│  └─────────┘  └─────────┘  └─────────┘
│                    ▲            ▲
│                    │            │
│       ┌────────────┴────────────┘
│       │
│       │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐
│  │         │  │         │  │         │
└─▶│ Consul  │──│ Consul  │──│ Consul  │
   │         │  │         │  │         │
   └─────────┘  └─────────┘  └─────────┘
@
-}

module Network.Wai.Middleware.Consul.GitHub
       (gitHubPullOnWebhook)
       where

import BasePrelude
import qualified Data.ByteString as B ( isPrefixOf )
import Network.HTTP.Types ( methodPost, hUserAgent )
import Network.Socket ( PortNumber(PortNum) )
import Network.Wai
    ( Request(pathInfo, requestHeaders, requestMethod) )
import Network.Wai.Middleware.Consul ( ConsulSettings(..) )
import System.Process ( callProcess )

-- | GitHub Webhook handler with Consul callback that does a `git pull`
-- when fired.
gitHubPullOnWebhook :: ConsulSettings
gitHubPullOnWebhook =
  ConsulSettings {csHost = "0.0.0.0"
                 ,csPort = PortNum 8500
                 ,csKey = "github"
                 ,csFilter = isGitHubWebhook
                 ,csCallback =
                    \_ ->
                      callProcess "git"
                                  ["pull"]}

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
  case lookup' hUserAgent (requestHeaders req) of
    [agent] -> "GitHub-Hookshot/" `B.isPrefixOf` agent
    _ -> False
  where lookup' a =
          map snd .
          filter (\x -> a == fst x)
