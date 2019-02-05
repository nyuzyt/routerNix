# routerNix

### Some notes for haskell
* =>
```
(==) :: (Eq a) => a -> a -> Bool

=>符号: 它左边的部分叫做类型约束。我们可以这样阅读这段类型声明：

    “相等函数取两个相同类型的值作为参数并返回一个布尔值，而这两个参数的类型同在Eq类之中（即类型约束）”
```

* ::
```
removeNonUppercase :: [Char] -> [Char]   

::读作“它的类型为”。凡是明确的类型，其首字母必为大写。
```

* <>
```
ghci> [1,2,3] <> [4,5,6]
[1,2,3,4,5,6]

It's an alias for mappend, from the Data.Monoid module.
```
* <*>
```
GHCi> (+) (Just 3) <*> Just 4
Just 7
```

* . and $
```
putStrLn (show (1 + 1))

putStrLn $ show $ 1 + 1

putStrLn . show $ 1 + 1

f ( g x ) = (f . g) x
```

### Some notes for Miso and Servant
#### Steps:
1. main: run port app (Servant module)
    ```haskell
    main = do
        IO.hPutStrLn IO.stderr ("Running on port " ++ show port ++ "...")
        run port $ logStdout (compress app)
      where
        port     = 3001 :: Port
        compress = gzip def { gzipFiles = GzipCompress }
    ```
2.  app: composed of routes and handlers
    ```haskell
    app :: Application
    app =
        Servant.serve (Proxy @ServerAPI)
            (    static
            :<|> serverHandlers
            :<|> Tagged page404
            )
    ```
    1. routes: static routes and serverRoutes

        ```haskell
        -- The server serves static files besides the ServerRoutes, among which is the
        -- javascript file of the client.
        type ServerAPI =
               StaticAPI
          :<|> (ServerRoutes
          :<|> Raw) -- This will show the 404 page for any unknown route
        
        type StaticAPI = "static" :> Raw
        -- Raw is another app::application

        type ServerRoutes
        = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

        ``` 

    2. static handler
        ```haskell
        static :: Servant.Server StaticAPI
        static = Servant.serveDirectoryFileServer "static"
        ```

    3. server Handler (import qualified Common)
        ```haskell
        serverHandlers :: Servant.Server ServerRoutes
        serverHandlers = homeServer :<|> flippedServer
        
        -- Alternative type:
        -- Servant.Server (ToServerRoutes Common.Home HtmlPage Common.Action)
        -- Handles the route for the home page, rendering Common.homeView.
        homeServer :: Servant.Handler (HtmlPage (View Common.Action))
        homeServer =
            pure $ HtmlPage $
              Common.viewModel $
              Common.initialModel Common.homeLink
        ```

        1. viewModel 
            ```haskell
            -- Checks which URI is open and shows the appropriate view
            viewModel :: Model -> View Action
            viewModel m =
                case Miso.runRoute (Proxy @ViewRoutes) viewTree _uri m of
                  Left _routingError -> page404View
                  Right v -> v
                  
            -- Servant tree of view functions
            -- Should follow the structure of ViewRoutes
            viewTree
                ::      (Model -> View Action)
                   :<|> (Model -> View Action)
            viewTree = homeView :<|> flippedView
            ```

        2. initialModel
            ```haskell
            initialModel :: Network.URI -> Model
            initialModel uri =
                Model
                { _uri = uri
                , _counterValue = 0
                }
            ```

        3. homeLink (import qualified Network.URI as Network)
            ```haskell
            homeLink :: Network.URI
            homeLink =
            #if MIN_VERSION_servant(0,10,0)
                Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
            #else
                safeLink (Proxy @ViewRoutes) (Proxy @Home)
            #endif
            ```
        
        4. ViewRoutes
            ```haskell
            -- Holds a servant route tree of `View action`
            type ViewRoutes = Home :<|> Flipped
            
            -- Home route, contains two buttons and a field
            type Home = View Action
            
            -- Flipped route, same as Home, but with the buttons flipped
            type Flipped = "flipped" :> View Action
            ```
            
    4. 404 Handler
        ```haskell
        -- The 404 page is a Wai application because the endpoint is Raw.
        -- It just renders the page404View and sends it to the client.
        page404 :: Wai.Application
        page404 _ respond = respond $ Wai.responseLBS
            HTTP.status404 [("Content-Type", "text/html")] $
            L.renderBS $ L.toHtml Common.page404View
     
        page404View :: View Action
        page404View =
            text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"
        ```
