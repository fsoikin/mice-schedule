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
      [ PianoPractice `on` [Tuesday, Wednesday, Thursday, Friday]
      , VariousHomework `on` [Monday, Tuesday, Wednesday, Thursday, Friday]

      , Maths `on` [Monday]
      , Programming `on` [Tuesday]
      , Maths `on` [Wednesday]
      , Programming `on` [Thursday]
      , Calculus `on` [Friday]
      ]

    anya = for Anya <$>
      [ PianoPractice `on` [Tuesday, Wednesday, Thursday, Friday]
      , Maths `on` [Monday, Tuesday, Wednesday, Thursday, Friday]
      , French `on` [Monday, Tuesday, Wednesday, Thursday, Friday]
      , CreativeWriting `on` [Monday, Tuesday, Wednesday, Thursday, Friday]
      , VariousHomework `on` [Monday, Tuesday, Wednesday, Thursday, Friday]
      ]

    on subj days
      | weekday date `elem` days = [slot subj]
      | otherwise = []

    for k x = if k == kid then x else []
