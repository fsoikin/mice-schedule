module Schedule.Fixed
  ( fixedSlots
  ) where

import Prelude

import Data.Array (concat)
import Data.Date (Weekday(..), day, month, weekday, year)
import Data.Enum (fromEnum)
import Data.Foldable (elem)
import GenSchedule (Rule, timeSlot)
import Types (Kid(..), Subject(..))

fixedSlots :: Rule
fixedSlots date kid = append $ concat
  [ for Matvey $ (Spanish # at 9 50 60) `on` [Monday, Wednesday]
  , for Matvey $ (Piano # at 11 15 45) `on` [Monday, Thursday]

  , (History # at 11 0 45) `on` [Wednesday]
  , (Biology # at 14 0 45) `on` [Tuesday, Thursday]

  -- Lessons with Avatar
  , for Matvey $ onDate 2020 9 1 $ Outschool # at 14 30 60

  -- Creative writing
  , for Anya $ onDate 2020  9  8 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020  9 15 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020  9 22 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020  9 29 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 10  6 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 10 13 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 10 20 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 10 27 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 11  3 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 11 10 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 11 17 $ Outschool # at 11 30 60
  , for Anya $ onDate 2020 11 24 $ Outschool # at 11 30 60
  ]
  where
    on s days
      | weekday date `elem` days = [s]
      | otherwise = []

    onDate yr mnth dy x
      | (fromEnum $ year date) == yr && (fromEnum $ month date) == mnth && (fromEnum $ day date) == dy = [x]
      | otherwise = []

    for k x = if k == kid then x else []

    at = timeSlot