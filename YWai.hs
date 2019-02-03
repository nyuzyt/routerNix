{-# LANGUAGE OverloadedStrings #-}

module YWai where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app request respond = respond $ case rawPathInfo request of
    "/"     -> index
    "/raw/" -> plainIndex
    _       -> notFound

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing

plainIndex :: Response
plainIndex = responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"