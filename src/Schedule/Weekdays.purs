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
  [ Maths `on` [Monday, Tuesday, Thursday, Friday]
  , LTW `on` [Wednesday, Friday]

  , for Matvey $ Essay `on` [Wednesday]
  , for Matvey $ Typing `on` [Tuesday, Wednesday]
  , for Matvey $ VoiceTraining `on` [Tuesday, Thursday, Friday]
  , for Matvey $ MasterClass `on` [Monday]
  , for Matvey $ KhanAcademy `on` [Friday]

  , for Anya $ Essay `on` [Monday, Wednesday]
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