module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Control.Monad.Aff (forkAff)
import App as App
import Router as R
import Routing (matchesAff)
import Data.Tuple (Tuple(..))

main :: forall eff. Eff (HA.HalogenEffects (ajax :: AX.AJAX | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI App.ui unit body

  forkAff $ do
    Tuple _ new <- matchesAff R.routes
    driver.query $ H.action $ App.ChangeRoute new
