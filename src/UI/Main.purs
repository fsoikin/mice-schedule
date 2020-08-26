module UI.Main
  ( Message
  , State
  , def
  ) where

import Prelude

import Data.Array (filter, last, (..))
import Data.Date (Date, Weekday(..), adjust)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDate)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition, bimap, fork, handle, lmap, (>#<))
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import GenSchedule (isDay)
import UI.Day as Day
import UI.Stats as Stats

data Message
  = Today Date
  | StatsMsg Stats.Message
  | LoadAnotherWeek

type State = Maybe
  { today :: Date
  , days :: Array Date
  , stats :: Stats.State
  }

def :: forall m. MonadEffect m => ComponentDef m Message State
def = { init, view, update }

init :: forall m. MonadEffect m => Transition m Message State
init = do
  fork $ liftEffect $ Today <$> nowDate
  pure Nothing

view :: State -> DispatchMsgFn Message -> ReactElement
view Nothing _ = R.empty
view (Just state) dispatch =
  H.div "container pt-5" $
  [ R.fragment $ state.days <#> Day.view
  , Stats.view state.stats (dispatch >#< StatsMsg)
  , H.button_ "btn btn-primary px-4 mb-5" { onClick: handle dispatch LoadAnotherWeek } "Ещё неделю!"
  ]

update :: forall m. MonadEffect m => State -> Message -> Transition m Message State
update Nothing m = case m of
  Today d -> do
    stats <- Stats.init { today: d } # lmap StatsMsg
    pure $ Just { today: d, days: weekFrom d, stats: stats }
  _ ->
    pure Nothing

update (Just state) msg = Just <$> case msg of
  Today _ -> pure state

  StatsMsg m ->
    Stats.update state.stats m
    # bimap StatsMsg state { stats = _ }

  LoadAnotherWeek -> do
    let lst = last state.days >>= adjust (Days 1.0) # fromMaybe state.today
    pure state { days = state.days <> weekFrom lst }

weekFrom :: Date -> Array Date
weekFrom d =
  (0..6)
  <#> (\i -> adjust (Days $ 1.0 * toNumber i) d # fromMaybe d)
  # filter (not $ isDay Saturday || isDay Sunday)