module UI.Stats
  ( Message
  , State
  , init
  , view
  , update
  ) where

import Prelude

import Control.MonadZero (guard)
import Data.Array (foldl)
import Data.Date (Date, Weekday(..), adjust, weekday)
import Data.Enum (pred, succ)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)

import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Elmish (DispatchMsgFn, ReactElement, Transition, handle)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import Schedule (schedule)
import Types (Kid, Subject(..), allKids, allSubjects, kidIcon, subjectName)
import UI.Utils (showDate, subjectStyle)

data Message
  = Toggle
  | MoveWeek Number

type State =
  { from :: Date
  , to :: Date
  , modal :: Boolean
  }

init :: forall m. { today :: Date } -> Transition m Message State
init { today } = pure
  { from: seekWeekday Monday pred today
  , to: seekWeekday Sunday succ today
  , modal: false
  }
  where
    seekWeekday day f curr
      | weekday curr == day = curr
      | Just next <- f curr = seekWeekday day f next
      | otherwise = curr

view :: State -> DispatchMsgFn Message -> ReactElement
view { modal: false } dispatch =
  H.div_ "position-fixed d-flex justify-content-end" { style: H.css { top: "2rem", right: "2rem" } } $
    H.button_ "btn btn-primary" { onClick: handle dispatch Toggle } "Предметы"

view state dispatch = R.fragment
  [ H.div "modal-backdrop fade show" R.empty
  , H.div "modal fade show d-block" $
      H.div "modal-dialog modal-dialog-scrollable modal-dialog-centered" $
        H.div "modal-content"

          [ H.div "modal-header justify-content-around align-items-center" $
            [ H.h4 "mb-0" $ showDate state.from <> " - " <> showDate state.to
            , H.div "d-flex"
              [ H.button_ "btn btn-outline-secondary" { onClick: handle dispatch $ MoveWeek (-1.0) } "<"
              , H.button_ "btn btn-outline-secondary ml-2" { onClick: handle dispatch $ MoveWeek 1.0 } ">"
              ]
            ]

          , H.div "modal-body px-5"
            [ H.div "row my-2 align-items-center"
              [ H.div "col-8" R.empty
              , R.fragment $ allKids <#> \k -> H.div "col-2 pl-2" $ kidIcon k
              ]
            , R.fragment $ counts <#> \c ->
                H.div_ "row my-1" { style: subjectStyle c.subject }
                [ H.div "col-8" $
                    H.strong "" $ subjectName c.subject
                , R.fragment $ allKids <#> \k ->
                    H.div "col-2" $ show $ fromMaybe 0 $ M.lookup k c.counts
                ]
            ]

          , H.div "modal-footer" $
              H.button_ "btn btn-primary px-4" { onClick: handle dispatch Toggle } "Угу, понятно"
          ]
  ]
  where
    dates = state.from # unfoldr \d -> if d > state.to then Nothing else Just $ Tuple d (fromMaybe d $ succ d)

    counts :: Array { subject :: Subject, counts :: M.Map Kid Int }
    counts = do
      subject <- allSubjects
      guard $ subject /= Lunch
      pure
        { subject
        , counts: countsPerKid <#> \m -> M.lookup subject m # fromMaybe 0
        }

    countsPerKid :: M.Map Kid (M.Map Subject Int)
    countsPerKid =
      M.fromFoldable $
        allKids <#> \kid ->
          Tuple kid $
            dates
            >>= (\d -> schedule d kid)
            # foldl (\m r -> m # M.insertWith (+) r.subject 1) M.empty

update :: forall m. State -> Message -> Transition m Message State
update state = case _ of
  Toggle ->
    pure state { modal = not state.modal }

  MoveWeek factor -> do
    let diff = Days $ 7.0 * factor
    pure state
      { from = state.from # adjust diff # fromMaybe state.from
      , to = state.to # adjust diff # fromMaybe state.to
      }
