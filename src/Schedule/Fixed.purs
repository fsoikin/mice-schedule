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
  [ for Matvey $ (Spanish # at 15 30 60) `on` [Monday]
  , for Matvey $ (Spanish # at 14 30 60) `on` [Wednesday]
  , for Matvey $ (Piano # at 11 15 45) `on` [Monday, Thursday]

  , for Matvey $ (Programming # at 13 50 50) `on` [Monday]
  , for Matvey $ (Programming # at 11 40 50) `on` [Tuesday]

  , for Anya $ (Piano # at 12 15 40) `on` [Thursday]

  , (History # at 10 0 60) `on` [Thursday]
  , for Anya $ (History # at 11 0 30) `on` [Thursday]
  , for Matvey $ (History # at 12 30 30) `on` [Thursday]

  -- Creative writing
  , for Anya $ onDate 2021 2 17 $ Outschool # at 16 15 60
  , for Anya $ onDate 2021 2 24 $ Outschool # at 16 15 60
  , for Anya $ onDate 2021 3  3 $ Outschool # at 16 15 60
  , for Anya $ onDate 2021 3 10 $ Outschool # at 16 15 60
  , for Anya $ onDate 2021 3 17 $ Outschool # at 16 15 60
  , for Anya $ onDate 2021 3 24 $ Outschool # at 16 15 60
  , for Anya $ onDate 2021 3 31 $ Outschool # at 16 15 60
  ]
  where
    on f days
      | weekday date `elem` days = f
      | otherwise = identity

    onDate yr mnth dy f
      | (fromEnum $ year date) == yr && (fromEnum $ month date) == mnth && (fromEnum $ day date) == dy = f
      | otherwise = identity

    for k f = if k == kid then f else identity

    -- move subj f = f subj <<< remove subj
    -- remove subj schd = schd # filter (\s -> s.subject /= subj)

    at yr mnth dy subj schd = timeSlot yr mnth dy subj `cons` schd
