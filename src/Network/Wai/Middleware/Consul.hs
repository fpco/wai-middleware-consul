{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Middleware.Consul
       (ConsulSettings(..),
        mkConsulWatch,
        mkConsulProxy)
       where

import           BasePrelude
import           Control.Concurrent.Async (withAsync, waitCatch)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import           Network.Consul (KeyValue(..),
                                 KeyValuePut(..),
                                 getKey,
                                 initializeConsulClient,
                                 putKey)
import           Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout)
import           Network.HTTP.Types (status201)
import           Network.Socket (PortNumber)
import           Network.Wai (Middleware, Request, responseLBS, strictRequestBody)
import           System.IO (hPutStr, stderr)

-- | Consul Settings for watching & proxying Consul data
data ConsulSettings =
  ConsulSettings {csHost :: T.Text                -- ^ Consul host address
                 ,csPort :: PortNumber            -- ^ Consul host port
                 ,csKey :: T.Text                 -- ^ Consul key
                 ,csFilter :: Request -> Bool     -- ^ Filter for proxy put
                 ,csCallback :: KeyValue -> IO () -- ^ Callback when data changes
                 }

-- | Creates a background process to receive notifications
-- of data changes from Consul.  This is accopmlished via a blocking
-- HTTP request. (The HTTP client manager used has been configured to
-- wait forever for a response.)  The ConsulSettings (csHost, csPort &
-- csKey) are used to connect to Consul and watch for key-value
-- changes.  When Consul's value changes, it will respond to the HTTP
-- request.  Upon receiving a good changed-value response, we fire the
-- csCallback function to allow for a reaction to the data change.  If
-- there there is a problem with the request/response cycle or an
-- exception in the supplied callback function, we just re-make the
-- rquest & wait patiently for changes again.
mkConsulWatch :: forall a. ConsulSettings -> IO a
mkConsulWatch cs =
  do cc <-
       initializeConsulClient
         (csHost cs)
         (csPort cs)
         (Just $
          defaultManagerSettings {managerResponseTimeout = Nothing})
     go cc 0
  where go cc idx' =
          withAsync (getKey cc
                            (csKey cs <> "?index=" <>
                             T.pack (show idx'))
                            Nothing
                            Nothing)
                    (\a ->
                       do kv <- waitCatch a
                          case kv of
                            Left e ->
                              do print ("CONSUL: " <> show e)
                                 -- TODO exponential backoff
                                 threadDelay $ 1000 * 1000
                                 go cc idx'
                            Right Nothing ->
                              do putStrLn "CONSUL: no data"
                                 threadDelay $ 1000 * 1000
                                 go cc idx'
                            Right (Just kv') ->
                              do putStrLn ("CONSUL: update #" <>
                                           show (kvModifyIndex kv') <>
                                           ")")
                                 (csCallback cs $ kv') `catch`
                                   (\e -> hPutStr stderr ("CONSUL: " <> show (e :: IOException)))
                                 go cc (kvModifyIndex kv'))

-- | Create WAI middleware that can be used to proxy incoming data
-- into Consul (one-way). This function initiates our consul client
-- and returns the middleware for WAI to use.  The middleware will
-- filter incoming requests that match ConsulSettings csFilter.  If
-- there is a match it will create a make the key value put call for
-- Consul using the incoming request body as the data for the Consul
-- K/V.
mkConsulProxy :: ConsulSettings -> IO Middleware
mkConsulProxy cs =
  do defaultCC <-
       initializeConsulClient (csHost cs)
                              (csPort cs)
                              Nothing
     return (proxyToConsul defaultCC)
  where proxyToConsul cc app' req respond
          | (csFilter cs $ req) =
            do bs <- strictRequestBody req
               let keyValuePut =
                     KeyValuePut {kvpKey = csKey cs
                                 ,kvpValue = LB.toStrict bs
                                 ,kvpCasIndex = Nothing
                                 ,kvpFlags = Nothing}
               _workedOK <-
                 putKey cc keyValuePut Nothing
               -- TODO respond negatively if Consul 'put' didn't work
               respond $
                 responseLBS status201 [] ""
          | otherwise = app' req respond
