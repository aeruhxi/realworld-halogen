module Api where

import Prelude

baseURL :: String
baseURL = "https://conduit.productionready.io"

apiUrl :: String -> String
apiUrl x = baseURL <> x

endPoints :: { listArticles :: String }
endPoints = {
  listArticles: apiUrl "/api/articles"
}