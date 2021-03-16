module UI.Skeleton
  ( Message
  , State
  , def
  ) where

import Prelude

import Elmish (ComponentDef, Dispatch, ReactElement, Transition)
import Elmish.React.DOM as R

data Message

type State = Unit

def :: forall m. ComponentDef m Message State
def = { init, view, update }

init :: forall m. Transition m Message State
init = pure unit

view :: State -> Dispatch Message -> ReactElement
view s dispatch = R.text "hello"

update :: forall m. State -> Message -> Transition m Message State
update s m = pure s
