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
fixedSlots date kid = concatRules $ matvey <> anya
  where
    matvey = for Matvey <$>
      [ (Spanish # at 10 0 60) `on` [Monday]
      , (Spanish # at 10 0 60) `on` [Thursday]

      , (Piano # at 12 0 45) `on` [Wednesday]

      , (Programming # at 13 0 50) `on` [Monday]
      , (move Lunch $ at 14 0 30) `on` [Monday]
      , (Programming # at 14 0 50) `on` [Wednesday]

      , (ArchitectureEdx # at 14 30 45) `on` [Monday, Tuesday, Thursday, Friday]
      , (ArchitectureEdx # at 11 0 45) `on` [Wednesday]
      , (move Lunch $ at 13 20 30) `on` [Wednesday]

      , (Physics # at 12 0 45) `on` [Tuesday]
      , (HigherMaths # at 13 30 45) `on` [Thursday]
      ]

    anya = for Anya <$>
      [ (Piano # at 10 0 45) `on` [Thursday]
      ]

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
