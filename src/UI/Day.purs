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
  [ H.h2 "bg-very-light px-2 px-md-4 py-2" $
      show (weekday date) <> " " <> showDate date
  , H.div "row mb-5 px-2 px-md-4" $
      allKids <#> \kid ->
        H.div "col-6"
        [ H.h4 "" $ kidName kid
        , renderKid $ schedule date kid
        ]
  ]

renderKid :: Array TimedRow -> ReactElement
renderKid rows =
  H.div "card" $
    H.div "card-body px-4 py-2 py-md-3" $
      rows <#> \r ->
        H.div_ "row my-1 py-1 py-md-2 align-items-center" { style: subjectStyle r.subject }
        [ H.div "small col-12 col-md-3"
          [ showTime r.time
          , if r.softTime then R.text " *" else R.empty
          ]
        , H.div "col text-nowrap" $
            H.strong "" $ subjectName r.subject
        ]

  where
    showTime t =
      R.text $ hourMinute t <> " - " <> hourMinute end
      where
        end = addMinutes t.duration t
        hourMinute x = (show x.hour) <> ":" <> (show x.minute # padZero)

    padZero s
      | String.length s < 2 = "0" <> s
      | otherwise = s