-- servant example
-- ####################################
-- module Main where
--
-- import Network.Wai.Handler.Warp
--
-- import Handler
--
-- main :: IO ()
-- main = run 8081 app3
-- ####################################

-- wai example
-- ####################################
-- module Main where
--
-- import Network.Wai.Handler.Warp (run)
--
-- import YWai
--
-- main :: IO ()
-- main = do
--     putStrLn $ "http://localhost:8080/"
--     run 8080 app
-- ####################################

-- miso sample
-- -- | Haskell language pragma
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
--
-- -- | Haskell module declaration
-- module Main where
--
-- -- | Miso framework import
-- import Miso
-- import Miso.String
--
-- import Sample
--
-- -- | Entry point for a miso application
-- main :: IO ()
-- main = startApp App {..}
--   where
--     initialAction = SayHelloWorld -- initial action to be executed on application load
--     model  = 0                    -- initial model
--     update = updateModel          -- update function
--     view   = viewModel            -- view function
--     events = defaultEvents        -- default delegated events
--     subs   = []                   -- empty subscription list
--     mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Main where

import Data.Proxy
import Servant.API
import Servant.Links
import Servant.Server.Internal.Handler

import Miso

import Router

-- | Main entry point
main :: IO ()
main = do
  IO.hPutStrLn IO.stderr ("Running on port " ++ show port ++ "...")
  run port $ logStdout (compress app)
 where
  port     = 3001 :: Port
  compress = gzip def { gzipFiles = GzipCompress }

app :: Application
app = serve
  (Proxy @ServerAPI)
  (    static
  :<|> serverHandlers
  :<|> Tagged page404
  )
  where
    static :: Server StaticAPI
    static = serveDirectoryFileServer "static"

    serverHandlers :: Server ServerRoutes
    serverHandlers = homeServer :<|> aboutServer

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
    -- Handles the route for the home page, rendering Common.homeView.
    homeServer :: Handler (HtmlPage (View Router.Action))
    homeServer =
        pure $ HtmlPage $
          Common.viewModel $
          Common.initialModel Common.homeLink

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
    -- Renders the /flipped page.
    flippedServer :: Servant.Handler (HtmlPage (View Common.Action))
    flippedServer =
        pure $ HtmlPage $
          Common.viewModel $
          Common.initialModel Common.flippedLink

    -- The 404 page is a Wai application because the endpoint is Raw.
    -- It just renders the page404View and sends it to the client.
    page404 :: Wai.Application
    page404 _ respond = respond $ Wai.responseLBS
        HTTP.status404 [("Content-Type", "text/html")] $
        L.renderBS $ L.toHtml Common.page404View

type ServerAPI =
       StaticAPI
  :<|> (ServerRoutes
  :<|> Raw) -- This will show the 404 page for any unknown route

type StaticAPI = "static" :> Servant.Raw

type ServerRoutes
   = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action


homeHandlers :: Handler (Wrapper (View Action))
homeHandlers = goHome

aboutHandler :: Handler (Wrapper (View Action))
aboutHandler = goAbout