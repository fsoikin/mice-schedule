module Schedule
  ( schedule
  ) where

import Prelude

import Data.Array (cons)
import Data.Date (Date, Weekday(..))
import GenSchedule (Rule, TimedRow, genSchedule, isDay, timeSlot, whenDate)
import Schedule.Fixed (fixedSlots)
import Schedule.Weekdays (weekdays)
import Types (Kid, Subject(..))

schedule :: Date -> Kid -> Array TimedRow
schedule = genSchedule
  [ weekdays
  , fixedSlots
  , lunch
  , noWeekends
  ]

lunch :: Rule
lunch _ _ = cons (Lunch # timeSlot 13 0 40)

noWeekends :: Rule
noWeekends = whenDate (isDay Saturday || isDay Sunday) \_ _ -> []
