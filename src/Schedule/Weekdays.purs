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
weekdays date kid = append $ concat
  [ for Matvey $ Piano `on` [Tuesday, Wednesday, Friday]
  , for Matvey $ Maths `on` [Monday, Tuesday, Thursday, Friday]
  , for Matvey $ Chinese `on` [Thursday, Friday]
  , for Matvey $ VoiceTraining `on` [Tuesday, Thursday, Friday]
  , for Matvey $ Essay `on` [Monday, Tuesday, Wednesday]
  , for Matvey $ Spanish `on` [Tuesday, Thursday]

  , for Anya $ Piano `on` [Monday, Tuesday, Wednesday, Friday]
  , for Anya $ Maths `on` [Monday, Tuesday, Thursday, Friday]
  , for Anya $ Essay `on` [Monday, Wednesday]
  , for Anya $ Typing `on` [Tuesday, Thursday]
  , for Anya $ VoiceTraining `on` [Tuesday, Thursday, Friday]
  , for Anya $ Writing `on` [Monday, Tuesday, Wednesday, Thursday, Friday]

  , History `on` [Tuesday, Thursday]
  ]
  where
    on subj days
      | weekday date `elem` days = [slot subj]
      | otherwise = []

    for k x = if k == kid then x else []
