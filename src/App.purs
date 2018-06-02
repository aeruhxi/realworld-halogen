module App where


import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Home as Home
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AX
import Router as R
import Data.Maybe(Maybe(..))

type State = { route :: R.AppRoute }

type Input = Unit

type Message = Void

data Query a
  = ChangeRoute R.AppRoute a

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type Effect eff = Aff (ajax :: AX.AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Effect eff)
ui =
  H.parentComponent
    { initialState: const { route: R.Home }
    , render
    , eval
    , receiver: const Nothing
    }
  where

    render :: State -> H.ParentHTML Query Home.Query Slot (Effect eff)
    render state =
      HH.div_
        [ header
        , HH.slot Slot Home.ui unit absurd
        ]
        where
          header =
            HH.nav [ HP.class_ (HH.ClassName "navbar navbar-light") ]
              [ HH.div [ HP.class_ (HH.ClassName "container") ]
                [ HH.a [ HP.class_ (HH.ClassName "navbar-brand"), HP.href "index.html" ]
                  [ HH.text "Conduit" ]
                , HH.ul [ HP.class_ (HH.ClassName "nav navbar-nav pull-xs-right") ]
                  [ navItem "Home"
                  , navItem "Sign up"]
                ]
              ]

          navItem text =
            HH.li [ HP.class_ (HH.ClassName "nav-item") ]
              [ HH.a [ HP.class_ (HH.ClassName "nav-link") ]
                [ HH.text text ]
              ]

          -- HH.slot (ArticleSlot x.slug) Article.ui  {article: x} absurd
          renderRoute = case _ of
            R.Home -> HH.slot ()
            _ ->

    eval :: Query ~> H.ParentDSL State Query Home.Query Slot Message (Effect eff)
    eval = case _ of
      ChangeRoute newRoute next -> do
        H.modify (_ {route = newRoute})
        pure next