{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Network.Wai.Middleware.Consul
Description : WAI Middleware for Consul
Copyright   : (c) FPComplete, 2015
License     : MIT
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX

This module helps you proxy information to Consul from the internet &
also react to changes to K/V data coming from Consul.

@
      ┌─────────┐      ┌─────────┐
      │  JSON   │      │  LOAD   │
      │  HTTP   │─────▶│ BALANCR │
      │  POST   │      │         │
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

module Network.Wai.Middleware.Consul
       ( ConsulSettings
       , defaultConsulSettings
       , getConsulCallback
       , getConsulFilter
       , getConsulHost
       , getConsulKey
       , getConsulLimit
       , getConsulPort
       , mkConsulProxy
       , mkConsulWatch
       , setConsulCallback
       , setConsulFilter
       , setConsulHost
       , setConsulKey
       , setConsulLimit
       , setConsulPort
       , withConsul
       ) where

import BasePrelude
import Control.Concurrent.Async ( race )
import Control.Exception.Enclosed ( catchAny )
import Control.Monad.IO.Class ( MonadIO(..), liftIO )
import Control.Monad.Logger ( MonadLoggerIO, logWarn )
import Control.Monad.Trans.Control
    ( MonadBaseControl(liftBaseWith, restoreM) )
import Control.Monad.Trans.Resource ( runResourceT )
import qualified Data.ByteString.Lazy as LB ( toStrict )
import Data.Conduit ( ($$) )
import qualified Data.Conduit.Binary as C ( take )
import Data.Text ( Text )
import qualified Data.Text as T ( pack )
import Network.Consul
    ( KeyValue(kvModifyIndex),
      KeyValuePut(KeyValuePut, kvpCasIndex, kvpFlags, kvpKey, kvpValue),
      putKey,
      initializeConsulClient,
      getKey )
import Network.HTTP.Client
    ( defaultManagerSettings, managerResponseTimeout )
import Network.HTTP.Types ( status201, methodPost )
import Network.Socket ( PortNumber(PortNum) )
import Network.Wai
    ( Middleware, Request, responseBuilder, pathInfo, requestMethod )
import Network.Wai.Conduit ( sourceRequestBody )

-- | Consul Settings for watching & proxying Consul data
data ConsulSettings =
  ConsulSettings {csHost :: Text -- ^ Consul host address
                 ,csPort :: PortNumber -- ^ Consul host port
                 ,csKey :: Text -- ^ Consul key
                 ,csFilter :: Request -> Bool -- ^ Filter for proxy put
                 ,csLimit :: Maybe Int -- ^ Optional request body size limit
                 ,csCallback :: ConsulCallback -- ^ Callback when data changes
                 }

type ConsulCallback = forall (m :: * -> *).
  (MonadBaseControl IO m,MonadLoggerIO m) => KeyValue -> m ()

defaultConsulSettings :: ConsulSettings
defaultConsulSettings =
  ConsulSettings {csHost = "0.0.0.0"
                 ,csPort = PortNum 8500
                 ,csKey = "wai"
                 ,csFilter =
                    (\req ->
                       (requestMethod req == methodPost) &&
                       (pathInfo req ==
                        ["wai"]))
                 ,csLimit = Nothing
                 ,csCallback = liftIO . print}

setConsulHost :: Text -> ConsulSettings -> ConsulSettings
setConsulHost a b = b { csHost = a }

getConsulHost :: ConsulSettings -> Text
getConsulHost = csHost

setConsulPort :: PortNumber -> ConsulSettings -> ConsulSettings
setConsulPort a b = b { csPort = a }

getConsulPort :: ConsulSettings -> PortNumber
getConsulPort = csPort

setConsulKey :: Text -> ConsulSettings -> ConsulSettings
setConsulKey a b = b { csKey = a }

getConsulKey :: ConsulSettings -> Text
getConsulKey = csKey

setConsulFilter :: (Request -> Bool) -> ConsulSettings -> ConsulSettings
setConsulFilter a b = b { csFilter = a }

getConsulFilter :: ConsulSettings -> Request -> Bool
getConsulFilter = csFilter

setConsulLimit :: Maybe Int -> ConsulSettings -> ConsulSettings
setConsulLimit a b = b { csLimit = a }

getConsulLimit :: ConsulSettings -> Maybe Int
getConsulLimit = csLimit

setConsulCallback :: ConsulCallback -> ConsulSettings -> ConsulSettings
setConsulCallback a b = b { csCallback = a }

getConsulCallback :: ConsulSettings -> ConsulCallback
getConsulCallback = csCallback

-- | Creates a complete Consul middleware for the cluster.
-- Combines mkConsulWatch async function (watches Consul data for
-- updates) & mkConsulProxy (proxys data from the internet to Consul)
-- into one common-use function. This will probably be the function
-- you want.  See the example/ application for more insight.
withConsul :: (Monad m,MonadBaseControl IO m,MonadLoggerIO m)
           => ConsulSettings -> (Middleware -> m a) -> m (Either () a)
withConsul cs f =
  liftRace (mkConsulWatch cs)
           (mkConsulProxy cs >>= f)

liftRace :: MonadBaseControl IO m
     => m a -> m b -> m (Either a b)
liftRace x y =
  do res <-
       liftBaseWith
         (\run ->
            race (run x)
                 (run y))
     case res of
       Left x' -> Left <$> restoreM x'
       Right y' -> Right <$> restoreM y'

-- | Creates a background process to receive notifications.
-- Notifications happen via blocking HTTP request. (The HTTP client
-- manager used has been configured to wait forever for a response.)
-- The ConsulSettings (csHost, csPort & csKey) are used to connect to
-- Consul and watch for key-value changes.  When Consul's value
-- changes, it will respond to the HTTP request.  Upon receiving a
-- good changed-value response, we fire the csCallback function to
-- allow for a reaction to the data change.  If there there is a
-- problem with the request/response cycle or an exception in the
-- supplied callback function, we just re-make the rquest & wait
-- patiently for changes again.
mkConsulWatch :: (MonadBaseControl IO m,MonadLoggerIO m)
              => ConsulSettings -> m ()
mkConsulWatch cs =
  go 0 =<<
  initializeConsulClient
    (csHost cs)
    (csPort cs)
    (Just $
     defaultManagerSettings {managerResponseTimeout = Nothing})
  where go idx' cc =
          catchAny (do kv <-
                         getKey cc
                                (csKey cs <> "?index=" <>
                                 T.pack (show idx'))
                                Nothing
                                Nothing
                       case kv of
                         Nothing ->
                           do liftIO (threadDelay $ 1000 * 1000)
                              go idx' cc
                         (Just kv') ->
                           do (csCallback cs $ kv')
                              go (kvModifyIndex kv') cc)
                   (\ex ->
                      $(logWarn) (T.pack $ show ex))

-- | Create WAI middleware that can be used to proxy incoming data
-- into Consul (one-way). This function initiates our consul client
-- and returns the middleware for WAI to use.  The middleware will
-- filter incoming requests that match ConsulSettings csFilter.  If
-- there is a match it will create a make the key value put call for
-- Consul using the incoming request body as the data for the Consul
-- K/V.
mkConsulProxy :: (MonadIO m,Functor m)
              => ConsulSettings -> m Middleware
mkConsulProxy cs =
  proxyToConsul <$>
  initializeConsulClient (csHost cs)
                         (csPort cs)
                         Nothing
  where proxyToConsul cc app' req respond
          | csFilter cs req =
            do bs <-
                 liftIO (runResourceT $
                         sourceRequestBody req $$
                         C.take (fromMaybe 5242880 (csLimit cs)))
               let keyValuePut =
                     KeyValuePut {kvpKey = csKey cs
                                 ,kvpValue = LB.toStrict bs
                                 ,kvpCasIndex = Nothing
                                 ,kvpFlags = Nothing}
               _workedOK <-
                 putKey cc keyValuePut Nothing
               -- TODO respond negatively if Consul 'put' didn't work
               respond (responseBuilder status201 [] "")
          | otherwise = app' req respond
