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
  [ for Anya $ Maths `on` [Monday, Tuesday, Thursday, Friday]
  -- , LTW `on` [Wednesday, Friday]

  , for Matvey $ Maths `on` [Monday, Tuesday, Thursday, Friday]
  , for Matvey $ Piano `on` [Tuesday, Wednesday, Friday]
  , for Matvey $ VoiceTraining `on` [Tuesday, Thursday, Friday]
  , for Matvey $ Essay `on` [Monday, Tuesday, Wednesday, Thursday, Friday]
  , for Matvey $ Spanish `on` [Tuesday, Friday]
  , for Matvey $ Essay `on` [Monday, Tuesday, Wednesday, Thursday, Friday]
  -- , for Matvey $ Typing `on` [Tuesday, Wednesday]
  -- , for Matvey $ MasterClass `on` [Monday]
  -- , for Matvey $ KhanAcademy `on` [Friday]

  , for Anya $ Piano `on` [Monday, Tuesday, Wednesday, Friday]
  , for Anya $ Essay `on` [Monday, Thursday]
  , for Anya $ LTW `on` [Tuesday, Thursday]
  , for Anya $ MasterClass `on` [Monday, Wednesday, Friday]
  , for Anya $ Typing `on` [Tuesday, Thursday]
  , for Anya $ KhanAcademy `on` [Wednesday, Friday]
  , for Anya $ Outschool `on` [Wednesday, Friday]
  ]
  where
    on subj days
      | weekday date `elem` days = [slot subj]
      | otherwise = []

    for k x = if k == kid then x else []
