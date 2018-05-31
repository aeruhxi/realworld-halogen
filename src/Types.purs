module Types where

import Prelude
import Data.DateTime (DateTime)
import Simple.JSON (class ReadForeign, readImpl)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), unformat, format)
import Data.List (List(Nil), (:))
import Control.Monad.Except.Trans (except)
import Data.Foreign (F, ForeignError(..))
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe)

dateFormat :: Formatter
dateFormat
  = YearFull
  : Placeholder "-"
  : MonthTwoDigits
  : Placeholder "-"
  : DayOfMonthTwoDigits
  : Placeholder "T"
  : Hours24
  : Placeholder ":"
  : MinutesTwoDigits
  : Placeholder ":"
  : SecondsTwoDigits
  : Placeholder "."
  : Milliseconds
  : Placeholder "Z"
  : Nil

dateOutputFormat :: Formatter
dateOutputFormat
  = MonthFull
  : Placeholder " "
  : DayOfMonthTwoDigits
  : Placeholder ", "
  : YearFull
  : Nil

formatArticleDate :: NDateTime -> String
formatArticleDate (NDateTime dt) = format dateOutputFormat dt

newtype NDateTime = NDateTime DateTime

dtParser :: String -> F DateTime
dtParser = except <<< lmap (pure <<< ForeignError) <<< unformat dateFormat

instance readDateTime :: ReadForeign NDateTime where
  readImpl s = do
    str <- readImpl s
    dt <- dtParser str
    pure $ NDateTime dt

type Profile =
  { username :: String
  , bio :: Maybe String
  , image :: String
  , following :: Boolean
  }

type Article =
  { slug :: String
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  , createdAt :: NDateTime
  , updatedAt :: NDateTime
  , favorited :: Boolean
  , favoritesCount :: Int
  , author :: Profile
  }

type Tag = String