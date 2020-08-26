module UI.Day
  ( view
  ) where

import Prelude

import Data.Date (Date, weekday)
import Data.String as String
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import GenSchedule (TimedRow)
import Schedule (schedule)
import Time (addMinutes)
import Types (allKids, kidName, subjectName)
import UI.Utils (showDate, subjectStyle)

view :: Date -> ReactElement
view date = R.fragment
  [ H.h2 "bg-very-light px-4 py-2" $
      show (weekday date) <> " " <> showDate date
  , H.div "row mb-5 px-4" $
      allKids <#> \kid ->
        H.div "col-6"
        [ H.h4 "" $ kidName kid
        , renderKid $ schedule date kid
        ]
  ]

renderKid :: Array TimedRow -> ReactElement
renderKid rows =
  H.div "card" $
    H.div "card-body" $
      rows <#> \r ->
        H.div_ "row my-1 py-1" { style: subjectStyle r.subject }
        [ H.div "col-3" $ showTime r.time
        , H.div "col" $
            H.strong "" $ subjectName r.subject
        ]

  where
    showTime t = String.joinWith ""
      [ show t.hour
      , ":"
      , show t.minute # padZero
      , " - "
      , show end.hour
      , ":"
      , show end.minute # padZero
      ]
      where
        end = addMinutes t.duration t

    padZero s
      | String.length s < 2 = "0" <> s
      | otherwise = s