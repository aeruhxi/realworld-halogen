module Home where

import Prelude

import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AX
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Api (endPoints)
import Types (Article)
import Simple.JSON (readJSON)
import Data.Either (Either(..))
import Article as Article
import Data.Foreign (MultipleErrors)

type State =
  { loading :: Boolean
  , articles :: Array Article
  }

data Query a
  = Initialize a

type Input = Unit

type Message = Void

newtype ArticleSlot = ArticleSlot String
derive instance eqArticleSlot :: Eq ArticleSlot
derive instance ordArticleSlot :: Ord ArticleSlot

type Effect eff = Aff (ajax :: AX.AJAX | eff)

home :: forall eff. H.Component HH.HTML Query Input Message (Effect eff)
home =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
    where

      initialState :: State
      initialState = { loading: false, articles: [] }

      render :: State -> H.ParentHTML Query Article.Query ArticleSlot (Effect eff)
      render st =
        HH.div [ HP.class_ (HH.ClassName "home-page") ]
          [ HH.div [ HP.class_ (HH.ClassName "banner") ]
              [ HH.div [ HP.class_ (HH.ClassName "container") ]
                  [ HH.h1 [ HP.class_ (HH.ClassName "logo-font") ]
                      [ HH.text "conduit" ]
                  , HH.p_ [ HH.text "A place to share your knowledge" ]
                  ]
              ]

          , HH.div [ HP.class_ (HH.ClassName "container page") ]
              [ HH.div [ HP.class_ (HH.ClassName "row") ]

                [ HH.div [ HP.class_ (HH.ClassName "col-md-9") ]
                    [ HH.div [ HP.class_ (HH.ClassName "feed-toggle") ]
                        [ HH.ul [ HP.class_ (HH.ClassName "nav nav-pills outline-active") ]
                            [ HH.li [ HP.class_ (HH.ClassName "nav-item") ]
                                [ HH.a [ HP.class_ (HH.ClassName "nav-link active") ]
                                    [ HH.text "Global Feed" ]
                                ]
                            ]
                        ]
                    ]
                ]

              , HH.div_ ( map renderArticle st.articles )
              ]
          ]

      renderArticle x =
        HH.slot (ArticleSlot x.slug) Article.article  {article: x} absurd

      eval :: Query ~> H.ParentDSL State Query Article.Query ArticleSlot Message (Effect eff)
      eval = case _ of
        Initialize next -> do
          H.modify (_ { loading = true })
          res <- H.liftAff $ AX.get endPoints.listArticles
          case readJSON res.response :: Either MultipleErrors { articles :: Array Article } of
            Left errors -> H.modify (_ { loading = false })
            Right payload -> H.modify (_ { articles = payload.articles })
          pure next



