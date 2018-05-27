module App where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Home as Home
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AX

type State = Unit

type Query = Const Void

type Input = Unit

type Message = Void

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type Effect eff = Aff (ajax :: AX.AJAX | eff)

app :: forall eff. H.Component HH.HTML Query Input Message (Effect eff)
app =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState = unit

    render :: State -> H.ParentHTML Query Home.Query Slot (Effect eff)
    render state =
      HH.div_
        [ header
        , HH.slot Slot Home.home unit absurd
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

    eval :: Query ~> H.ParentDSL State Query Home.Query Slot Message (Effect eff)
    eval = unwrap >>> absurd