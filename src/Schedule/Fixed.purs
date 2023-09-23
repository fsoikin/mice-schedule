module Schedule.Fixed
  ( fixedSlots
  ) where

import Prelude

import Data.Array (cons)
import Data.Date (Weekday(..), weekday)

import Data.Foldable (elem)
import GenSchedule (Rule, concatRules, timeSlot)
import Types (Kid(..), Subject(..))

fixedSlots :: Rule
fixedSlots date kid = concatRules $ matvey <> anya
  where
    matvey = for Matvey <$>
      [ (College # at 8 0 90) `on` [Tuesday, Thursday]
      , (Spanish # at 10 0 60) `on` [Wednesday, Friday]
      , (Piano # at 9 30 45) `on` [Monday]
      ]

    anya = for Anya <$>
      [ (College # at 8 0 90) `on` [Monday, Wednesday]
      , (Piano # at 10 30 45) `on` [Monday]
      , (Physics # at 11 0 45) `on` [Tuesday]
      , (Programming # at 11 0 45) `on` [Thursday, Friday]
      ]

    on f days
      | weekday date `elem` days = f
      | otherwise = identity

    -- onDate :: forall a. Int -> Int -> Int -> (a -> a) -> a -> a
    -- onDate yr mnth dy f
    --   | (fromEnum $ year date) == yr && (fromEnum $ month date) == mnth && (fromEnum $ day date) == dy = f
    --   | otherwise = identity

    for k f = if k == kid then f else identity

    --move subj f = f subj <<< remove subj
    --remove subj = filter (\s -> s.subject /= subj)

    at yr mnth dy subj schd = timeSlot yr mnth dy subj `cons` schd
