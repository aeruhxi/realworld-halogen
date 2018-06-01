module Article where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Article, formatArticleDate)
import Data.Maybe (Maybe(..))

type State = { article :: Article }

type Input = { article :: Article }

data Query a
  = HandleInput Input a

ui :: forall m. H.Component HH.HTML Query Input Void m
ui =
  H.component
    { initialState: \{ article } -> { article }
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

    render :: State -> H.ComponentHTML Query
    render { article: a } =
        HH.div [ HP.class_ (HH.ClassName "article-preview") ]
          [ HH.div [ HP.class_ (HH.ClassName "article-meta" )]
            [ HH.a [ HP.href "" ] [ HH.img [ HP.src a.author.image ] ]
            , HH.div [ HP.class_ (HH.ClassName "info") ]
                [ HH.a [ HP.class_ (HH.ClassName "author"), HP.href "" ] [ HH.text a.author.username ]
                , HH.span [ HP.class_ (HH.ClassName "date") ] [ HH.text $ formatArticleDate a.createdAt ]
                ]
            , HH.button [ HP.class_ (HH.ClassName "btn btn-outline-primary btn-sm pull-xs-right") ]
                [ HH.i [ HP.class_ (HH.ClassName "ion-heart") ] [ HH.text $ show a.favoritesCount ]
                ]
            ]

          , HH.a [ HP.class_ (HH.ClassName "preview-link") ]
              [ HH.h1_ [ HH.text a.title ]
              , HH.p_ [ HH.text a.description ]
              , HH.span_ [ HH.text "Read more..." ]
              ]
          ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval q = case q of
      HandleInput n next -> do
        H.put { article: n.article }
        pure next

