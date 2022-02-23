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
  [ for Matvey $ (Spanish # at 9 0 60) `on` [Wednesday]
  , for Matvey $ (Spanish # at 10 0 60) `on` [Friday]

  , for Matvey $ (Piano # at 12 0 45) `on` [Monday]
  , for Matvey $ (Piano # at 11 0 45) `on` [Thursday]
  , for Anya $ (Piano # at 10 0 40) `on` [Thursday]

  , for Matvey $ (Programming # at 12 30 50) `on` [Monday]
  , for Matvey $ (move Lunch $ at 13 30 45) `on` [Monday]
  , for Matvey $ (Programming # at 13 30 50) `on` [Thursday]
  , for Matvey $ (move Lunch $ at 14 30 45) `on` [Thursday]
  , for Anya $ (Programming # at 14 0 50) `on` [Monday]

  , (Physics # at 12 0 45) `on` [Tuesday]
  , (Russian # at 11 0 50) `on` [Wednesday, Friday]
  , (Biology # at 12 0 50) `on` [Wednesday]
  , (History # at 12 0 50) `on` [Friday]
  ]
  where
    on f days
      | weekday date `elem` days = f
      | otherwise = identity

    onDate :: forall a. Int -> Int -> Int -> (a -> a) -> a -> a
    onDate yr mnth dy f
      | (fromEnum $ year date) == yr && (fromEnum $ month date) == mnth && (fromEnum $ day date) == dy = f
      | otherwise = identity

    for k f = if k == kid then f else identity

    move subj f = f subj <<< remove subj
    remove subj = filter (\s -> s.subject /= subj)

    at yr mnth dy subj schd = timeSlot yr mnth dy subj `cons` schd
