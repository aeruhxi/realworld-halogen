module Api where

import Prelude

baseURL :: String
baseURL = "https://conduit.productionready.io"

apiUrl :: String -> String
apiUrl x = baseURL <> x

endPoints :: { listArticles :: String
, getTags :: String
}
endPoints =
  { listArticles: apiUrl "/api/articles"
  , getTags: apiUrl "/api/tags"
  }