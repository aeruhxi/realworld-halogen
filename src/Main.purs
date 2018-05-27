module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import App (app)

main :: forall eff. Eff (HA.HalogenEffects (ajax :: AX.AJAX | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI app unit body