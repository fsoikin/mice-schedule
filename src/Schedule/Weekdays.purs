module Schedule.Weekdays
  ( weekdays
  ) where

import Prelude

import Data.Array (concat)
import Data.Date (Weekday(..), weekday)
import Data.Foldable (elem)
import GenSchedule (Rule, slot)
import Types (Kid(..), Subject(..))

weekdays :: Rule
weekdays date kid = append $ concat $ matvey <> anya
  where
    matvey = for Matvey <$>
      [ Piano `on` [Monday, Tuesday, Thursday, Friday]
      , Maths `on` [Monday, Tuesday, Thursday, Friday]
      , VoiceTraining `on` [Tuesday, Thursday, Friday]
      , Essay `on` [Tuesday, Wednesday, Friday]
      , Spanish `on` [Wednesday, Friday]
      , RoomCleaning `on` [Wednesday]
      ]

    anya = for Anya <$>
      [ Piano `on` [Monday, Tuesday, Wednesday, Friday]
      ]

    on subj days
      | weekday date `elem` days = [slot subj]
      | otherwise = []

    for k x = if k == kid then x else []
