module Router where

import Prelude

import Data.Foldable (oneOf)
import Routing.Match.Class (lit, str, end)
import Routing.Match (Match)

data AppRoute
  = Home
  | Login
  | Register
  | Settings
  | EditorCreate
  | EditorEdit String
  | ArticleIndex String
  | ProfileIndex String
  | ProfileFavorites String
  | NotFound

routes :: Match AppRoute
routes =
  lit "" *> oneOf
    [ Home <$ end
    , Login <$ lit "login" <* end
    , Register <$ lit "register" <* end
    , Settings <$ lit "settings" <* end
    , EditorCreate <$ lit "editor" <* end
    , EditorEdit <$> (lit "editor" *> str <* end)
    , ArticleIndex <$> (lit "article" *> str <* end)
    , ProfileIndex <$> (lit "profile" *> str <* end)
    , ProfileFavorites <$> (lit "profile" *> str <* lit "favorites" <* end)
    ]