{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import qualified Common
import           Data.Proxy
import qualified Lucid                                as L
import qualified Lucid.Base                           as L
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.Gzip          as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Servant
import           Servant ( (:>), (:<|>)(..) )
import qualified System.IO                            as IO

import qualified Miso
import Miso ( View )
import Miso.String

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr "Running on port 3003..."

    Wai.run 3003 $ Wai.logStdout $ compress app
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }

app :: Wai.Application
app =
    Servant.serve (Proxy @ServerAPI)
        (    static
        :<|> serverHandlers
        :<|> Servant.Tagged page404
        )
  where
    static :: Servant.Server StaticAPI
    static = Servant.serveDirectoryFileServer "static"

    serverHandlers :: Servant.Server ServerRoutes
    serverHandlers = homeServer :<|> flippedServer :<|> loginServer

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
    -- Handles the route for the home page, rendering Common.homeView.
    homeServer :: Servant.Handler (HtmlPage (View Common.Action))
    homeServer =
        pure $ HtmlPage $
          Common.homeView $
          Common.initialModel Common.homeLink

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
    -- Renders the /flipped page.
    flippedServer :: Servant.Handler (HtmlPage (View Common.Action))
    flippedServer =
        pure $ HtmlPage $
          Common.flippedView $
          Common.initialModel Common.flippedLink

    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
    -- Renders the /flipped page.
    loginServer :: Servant.Handler (HtmlPage (View Common.Action))
    loginServer =
        pure $ HtmlPage $
          Common.loginView $
          Common.initialModel Common.loginLink

    -- The 404 page is a Wai application because the endpoint is Raw.
    -- It just renders the page404View and sends it to the client.
    page404 :: Wai.Application
    page404 _ respond = respond $ Wai.responseLBS
        HTTP.status404 [("Content-Type", "text/html")] $
        L.renderBS $ L.toHtml Common.page404View

-- | Represents the top level Html code. Its value represents the body of the
-- page.
newtype HtmlPage a = HtmlPage a
  deriving (Show, Eq)

cdnEdge :: MisoString
cdnEdge = "https://herocdn.sfo2.cdn.digitaloceanspaces.com"

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
  toHtmlRaw = L.toHtml
  toHtml (HtmlPage x) = do
    L.doctype_
    L.html_
        [L.class_ "has-navbar-fixed-top  has-navbar-fixed-bottom", L.lang_ "en"]
      $ do
          L.head_ $ do
            L.title_ "Hero [alpha]"
            L.link_ [L.rel_ "manifest", L.href_ "/manifest.json"]
            L.link_ [L.rel_ "icon", L.type_ ""]

            -- icons
            L.link_
              [ L.rel_ "apple-touch-icon"
              , L.sizes_ "180x180"
              , L.href_
              $  cdnEdge
              <> "/assets/images/favicons/apple-touch-icon.png"
              ]
            L.link_
              [ L.rel_ "icon"
              , L.type_ "image/png"
              , L.sizes_ "32x32"
              , L.href_ $ cdnEdge <> "/assets/images/favicons/favicon-32x32.png"
              ]
            L.link_
              [ L.rel_ "icon"
              , L.type_ "image/png"
              , L.sizes_ "16x16"
              , L.href_ $ cdnEdge <> "/assets/images/favicons/favicon-16x16.png"
              ]
            L.link_
              [ L.rel_ "manifest"
              , L.href_ $ cdnEdge <> "/assets/images/favicons/manifest.json"
              ]
            L.link_
              [ L.rel_ "mask-icon"
              , L.href_
              $  cdnEdge
              <> "/assets/images/favicons/safari-pinned-tab.svg"
              ]

            L.meta_ [L.charset_ "utf-8"]
            L.meta_ [L.name_ "theme-color", L.content_ "#000"]
            L.meta_ [L.httpEquiv_ "X-UA-Compatible", L.content_ "IE=edge"]
            L.meta_
              [ L.name_ "viewport"
              , L.content_ "width=device-width, initial-scale=1"
              ]
            cssRef animateRef
            cssRef bulmaRef
            cssRef fontAwesomeRef
            cssRef "/static/all.css"
            jsRef "/static/all.js"
          L.body_ (L.toHtml x)
   where
    jsRef href = L.with
      (L.script_ mempty)
      [ L.makeAttribute "src"   href
      , L.makeAttribute "async" mempty
      , L.makeAttribute "defer" mempty
      ]
    cssRef href = L.with
      (L.link_ mempty)
      [L.rel_ "stylesheet", L.type_ "text/css", L.href_ href]

fontAwesomeRef :: MisoString
fontAwesomeRef =
  "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"

animateRef :: MisoString
animateRef =
  "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.7.0/animate.min.css"

bulmaRef :: MisoString
bulmaRef =
  "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.2/css/bulma.min.css"

-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View Common.Action))`
type ServerRoutes
   = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI =
       StaticAPI
  :<|> (ServerRoutes
  :<|> Servant.Raw) -- This will show the 404 page for any unknown route

type StaticAPI = "static" :> Servant.Raw

