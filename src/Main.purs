module Main where

import Prelude

import Effect (Effect)
import Elmish.Boot as Elmish
import UI.Main as UI

main :: Effect Unit
main = Elmish.defaultMain { elementId: "app", def: UI.def }
