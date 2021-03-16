module UI.Day
  ( view
  ) where

import Prelude

import Data.Array (drop, zipWith)
import Data.Date (Date, weekday)
import Data.Maybe (Maybe(..))
import Data.String as String
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Elmish.React.DOM as R
import GenSchedule (TimedRow)
import Schedule (schedule)
import Time (addMinutes, minuteDiff)
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
      renderRow <$> rowPairs
  where
    rowPairs =
      zipWith { current: _, next: _ } rows $ (Just <$> drop 1 rows) <> [Nothing]

    renderRow { current, next } =
      H.div_
        "row my-1 py-1 py-md-2 align-items-center position-relative"
        { style: subjectStyle current.subject }
        [ overlap
        , H.div "small col-12 col-md-3"
          [ R.text $ hourMinute current.time <> " - " <> hourMinute slotEnd
          , if current.softTime then R.text " *" else R.empty
          ]
        , H.div "col text-nowrap" $
            H.strong "" $ subjectName current.subject
        ]
      where
        slotEnd = addMinutes current.time.duration current.time
        hourMinute x = (show x.hour) <> ":" <> (show x.minute # padZero)

        overlap
          | Just n <- next, minuteDiff n.time slotEnd < 0 =
              H.div_
                "bg-danger position-absolute h-100"
                { style: H.css { width: "10px", left: "-12px" } }
                ""
          | otherwise =
              R.empty

    padZero s
      | String.length s < 2 = "0" <> s
      | otherwise = s
