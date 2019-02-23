{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

module Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View )
import Miso.String
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network


data Model
   = Model
     { _uri          :: !Network.URI
     , _counterValue :: !Int
     }
     deriving (Eq, Show)

initialModel :: Network.URI -> Model
initialModel uri =
    Model
    { _uri = uri
    , _counterValue = 0
    }

data Action
  = NoOp
  | AddOne
  | SubtractOne
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped :<|> Login

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

type Login = "login" :> View Action

makeLenses ''Model

-- Checks which URI is open and shows the appropriate view
viewModel :: Model -> View Action
viewModel m =
    case Miso.runRoute (Proxy @ViewRoutes) viewTree _uri m of
      Left _routingError -> page404View
      Right v -> v

-- -- Servant tree of view functions
-- -- Should follow the structure of ViewRoutes
viewTree = homeView :<|> flippedView :<|> loginView

-- View function of the Home route
homeView :: Model -> View Action
homeView m =
    div_ []
      [ div_
        []
        [ button_ [ onClick SubtractOne ] [ text "-" ]
        , text $ Miso.ms $ show $ _counterValue m
        , button_ [ onClick AddOne ] [ text "+" ]
        ]
      , button_ [ onClick $ ChangeURI flippedLink ] [ text "Go to /flipped" ]
      ]

-- View function of the Flipped route
flippedView :: Model -> View Action
flippedView m =
    div_ []
      [ div_
        []
        [ button_ [ onClick AddOne ] [ text "+" ]
        , text $ Miso.ms $ show $ _counterValue m
        , button_ [ onClick SubtractOne ] [ text "-" ]
        ]
      , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to /" ]
      ]
cdnEdge :: MisoString
cdnEdge = "https://herocdn.sfo2.cdn.digitaloceanspaces.com"

-- View function of the Login route
loginView :: Model -> View Action
loginView m =
    body_ [class_ "layout-default"]
      [ section_ [class_ "hero is-fullheight is-medium is-black is-bold"]
        [ div_ [class_ "hero-body"]
          [ div_ [class_ "container"]
            [ div_ [class_ "column is-6 is-offset-3"]
              [ div_ [class_ "card-content"]
                [ div_ [class_ "column is-4 is-offset-4"]
                  [ figure_ [class_ "image"]
                    [img_ [src_ $ cdnEdge <> "/assets/images/icons/hero-logo.png"]]
                    ]
                , hr_ []
                , form_  [] [
                  div_ [class_ "control"]
                    [ input_ [class_ "input", type_ "email", placeholder_ "Email"]]
                , div_ [class_ "control"]
                    [ input_ [class_ "input", type_ "password", placeholder_ "Password"]]
                , div_ [class_ "columns is-gapless"]
                    [ div_ [class_ "column control is-half"]
                      [ div_ [class_ "checkbox", id_ "checkbox"]
                        [ input_ [type_ "checkbox"]
                        , label_ [for_ "checkbox"] [text "Remember Me"]
                        ]
                      ]
                  , div_ [class_ "column control is-half"]
                    [ div_ [class_ "button is-black", id_ "login_button"]
                      [ text "Login"
                      ]
                    ]
                  ]
                ]
                , hr_ []
                , center_ []
                  [ p_ []
                    [ a_ [href_ "/"][text "Forgot your username or password?"]
                    ]
                  , p_ []
                    [ a_ [href_ "/"][text "Don't have an account? Sign Up"]
                    ]
                  ]
              ]
            ]
          ]
        ]
      ]
     ]

page404View :: View Action
page404View =
    text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif

-- Network.URI that points to the flipped route
flippedLink :: Network.URI
flippedLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Flipped)
#endif

-- Network.URI that points to the flipped route
loginLink :: Network.URI
loginLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Login)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Login)
#endif
