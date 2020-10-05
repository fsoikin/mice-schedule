module Schedule.Fixed
  ( fixedSlots
  ) where

import Prelude

import Data.Array (cons, filter)
import Data.Date (Weekday(..), day, month, weekday, year)
import Data.Enum (fromEnum)
import Data.Foldable (elem)
import GenSchedule (Rule, concatRules, timeSlot)
import Types (Kid(..), Subject(..))

fixedSlots :: Rule
fixedSlots date kid = concatRules
  [ for Matvey $ (Spanish # at 9 50 60) `on` [Monday, Wednesday]
  , for Matvey $ (Piano # at 11 15 45) `on` [Monday, Thursday]

  , for Matvey $ (Piano # at 13 15 45) `on` [Tuesday, Wednesday]
  , for Matvey $ (Piano # at 13  0 45) `on` [Friday]
  , for Matvey $ (move Lunch $ at 14 0 40) `on` [Tuesday, Wednesday, Friday]

  , for Matvey $ (Programming # at 13 50 50) `on` [Monday]
  , for Matvey $ (Programming # at 11 40 50) `on` [Tuesday]

  , for Anya $ (Piano # at 12 15 50) `on` [Thursday]
  , for Anya $ (Piano # at 14 15 50) `on` [Monday, Tuesday, Wednesday, Friday]
  , for Anya $ onDate 2020 9 7 $ remove Piano
  , for Anya $ (move Lunch $ at 13 30 40) `on` [Monday]

  , (History # at 11 0 45) `on` [Wednesday]
  , (Biology # at 14 0 45) `on` [Thursday]

  -- Lessons with Avatar
  , for Matvey $ onDate 2020 10  8 $ Outschool # at 15 10 40
  , for Matvey $ onDate 2020 10 15 $ Outschool # at 15 10 40
  , for Matvey $ onDate 2020 10 22 $ Outschool # at 15 10 40
  , for Matvey $ onDate 2020 10 29 $ Outschool # at 15 10 40

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
    on f days
      | weekday date `elem` days = f
      | otherwise = identity

    onDate yr mnth dy f
      | (fromEnum $ year date) == yr && (fromEnum $ month date) == mnth && (fromEnum $ day date) == dy = f
      | otherwise = identity

    for k f = if k == kid then f else identity

    move subj f = f subj <<< remove subj
    remove subj schd = schd # filter (\s -> s.subject /= subj)

    at yr mnth dy subj schd = timeSlot yr mnth dy subj `cons` schd