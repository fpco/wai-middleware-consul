{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import           BasePrelude
import           Control.Concurrent.Async (race_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.PackageDescription.TH
import           Distribution.Verbosity
import           Git.Embed
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Consul (mkConsulProxy, mkConsulWatch)
import           Network.Wai.Middleware.Consul.GitHub (gitHubPullOnWebhook)
import           Network.Wai.Middleware.RequestLogger
import           System.Process
import           System.Posix
import           Text.Blaze (ToMarkup)
import           Text.Blaze.Html (Markup)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Hamlet (hamlet)

main :: IO ()
main =
  race_ (mkConsulWatch gitHubPullOnWebhook)
        (do middleware <- mkConsulProxy gitHubPullOnWebhook
            run 8080 $
              logStdoutDev $ middleware $ dashboard)

dashboard :: Application
dashboard _req respond =
  do cabalRev <- onDiskCabalVer
     gitRev <- onDiskGitRev
     processId <- getProcessID
     respond (responseLBS status200
                          [(hContentType,"text/html")] $
              (renderHtml $
               status (show processId)
                      cabalRev
                      gitRev
                      (\_ _ -> T.empty)))

status :: forall t a a1 a2.
          (ToMarkup a2,ToMarkup a1,ToMarkup a)
       => a -> a1 -> a2 -> t -> Markup
status processId cabalVer gitRev = [hamlet|
$doctype 5
<html>
  <head>
    <link href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css" rel="stylesheet">
    <link href="//getbootstrap.com/examples/jumbotron-narrow/jumbotron-narrow.css" rel="stylesheet">
    <script src="//maxcdn.bootstrapcdn.com/bootstrap/3.3.2/js/bootstrap.min.js">
  <body>
    <div class="container">
      <div class="header">
        <nav>
          <ul class="nav nav-pills pull-right">
            <li role="presentation"><a href="https://github.com/fpco/wai-middleware-consul">GitHub</a>
            <li role="presentation"><a href="https://hackage.haskell.org/package/wai-middleware-consul">Hackage</a>
        <h3 class="text-muted">FPComplete
      <div class="jumbotron">
        <h1>WAI Middleware
        <h2>for <a href="https://www.consul.io">Consul</a>
      <p class="lead">WAI Middleware Consul helps your web application interaction with Consul.  It can help you proxy data to Consul and it can help you watch for data changes in the Consul cluster.
      <table class="table table-striped table-bordered table-hover table-condensed">
        <thead>
          <tr><th>Item</th><th>Value</th>
        <tbody>
          <tr><td>Web-App System Process ID</td><td>#{processId}</td>
          <tr><td>Cabal Version (Compiled)</td><td>#{compiledCabalVer}</td>
          <tr><td>Git Revision (Compiled)</td><td>#{compiledGitRev}</td>
          <tr><td>Cabal Version (On Disk)</td><td>#{cabalVer}</td>
          <tr><td>Git Revision (On Disk)</td><td>#{gitRev}</td>
      <footer class="footer">
        <p>&copy; FPComplete 2015
|]

compiledGitRev :: Text
compiledGitRev = $(embedGitShortRevision)

compiledCabalVer :: Text
compiledCabalVer = $(packageVariable (pkgVersion . package))

onDiskGitRev :: IO Text
onDiskGitRev = return . T.pack =<< readProcess "git" ["rev-parse", "--short", "HEAD"] []

onDiskCabalVer :: IO Text
onDiskCabalVer =
  return . T.pack . showVersion . pkgVersion . package . packageDescription =<<
  readPackageDescription silent "wai-middleware-consul.cabal"
