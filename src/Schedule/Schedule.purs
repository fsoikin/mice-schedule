module Schedule
  ( schedule
  ) where

import Prelude


import Data.Date (Date, Weekday(..))
import GenSchedule (Rule, TimedRow, genSchedule, isDay, whenDate)
import Schedule.Fixed (fixedSlots)
import Schedule.Weekdays (weekdays)
import Types (Kid)

schedule :: Date -> Kid -> Array TimedRow
schedule = genSchedule
  [ weekdays
  , fixedSlots
  , noWeekends
  ]

noWeekends :: Rule
noWeekends = whenDate (isDay Saturday || isDay Sunday) \_ _ -> []

