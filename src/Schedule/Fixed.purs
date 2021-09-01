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
  [ for Matvey $ (Spanish # at 16 30 60) `on` [Monday, Wednesday]

  , for Matvey $ (Piano # at 11 0 45) `on` [Monday, Thursday]
  , for Anya $ (Piano # at 12 0 40) `on` [Thursday]

  , for Matvey $ (Programming # at 12 0 50) `on` [Monday]
  , for Matvey $ (Programming # at 14 0 50) `on` [Thursday]
  , for Anya $ (Programming # at 14 0 50) `on` [Monday]

  , (Physics # at 12 0 45) `on` [Tuesday]
  , (History # at 10 0 120) `on` [Wednesday]
  , (Biology # at 10 0 120) `on` [Friday]

  , onDate 2021 9 1 $ remove Essay
  , onDate 2021 9 1 $ remove Spanish
  , onDate 2021 9 1 $ remove Maths

  , onDate 2021 9 2 $ remove Programming
  , onDate 2021 9 2 $ remove Typing
  , onDate 2021 9 2 $ remove VoiceTraining
  , onDate 2021 9 2 $ remove Maths

  , onDate 2021 9 3 $ remove Chinese
  , onDate 2021 9 3 $ remove Piano
  , onDate 2021 9 3 $ remove VoiceTraining
  , onDate 2021 9 3 $ remove Maths
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
    remove subj = filter (\s -> s.subject /= subj)

    at yr mnth dy subj schd = timeSlot yr mnth dy subj `cons` schd
