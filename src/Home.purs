module Home where

import Prelude

import Api (endPoints)
import Article as Article
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Simple.JSON (readJSON)
import Types as T
import Control.Parallel (sequential, parallel)

type State =
  { loading :: Boolean
  , articles :: Array T.Article
  , tags :: Array T.Tag
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
      initialState = { loading: false, articles: [], tags: [] }

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

                    , HH.div_ ( map renderArticle st.articles )
                    ]

                , HH.div [ HP.class_ (HH.ClassName "col-md-3") ]
                    [ HH.div [ HP.class_ (HH.ClassName "sidebar") ]
                        [ HH.p_ [ HH.text "Popular Tags" ]

                        , HH.div [ HP.class_ (HH.ClassName "tag-list") ]
                            (map
                              (\tag -> HH.a
                                [ HP.href "", HP.class_ (HH.ClassName "tag-pill tag-default") ] [ HH.text tag ])
                              st.tags
                            )
                        ]
                    ]
                ]
              ]
          ]

      renderArticle x =
        HH.slot (ArticleSlot x.slug) Article.article  {article: x} absurd

      eval :: Query ~> H.ParentDSL State Query Article.Query ArticleSlot Message (Effect eff)
      eval = case _ of
        Initialize next -> do
          H.modify (_ { loading = true })

          let articlesRequest = H.liftAff $ AX.get endPoints.listArticles
          let tagsRequest = H.liftAff $ AX.get endPoints.getTags

          responses <- sequential $
            { articles: _, tags: _ }
              <$> parallel articlesRequest
              <*> parallel tagsRequest

          case readJSON responses.articles.response :: Either MultipleErrors { articles :: Array T.Article } of
            Left errors -> H.modify (_ { loading = false })
            Right payload -> H.modify (_ { articles = payload.articles })

          case readJSON responses.tags.response :: Either MultipleErrors { tags :: Array T.Tag } of
            Left errors -> H.modify (_ { loading = false })
            Right payload -> H.modify (_ { tags = payload.tags })

          pure next



