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

  , Essay `on` [Monday, Wednesday]

  -- 🐺
  -- , Latin `on` [Tuesday, Thursday]
  -- , Solfege `on` [Tuesday, Thursday]

  -- Самостоятельно
  , for Matvey $ Typing `on` [Monday, Tuesday, Wednesday, Thursday, Friday]

  , LTW `on` [Wednesday, Friday]
  , for Anya $ LTW `on` [Tuesday, Thursday]
  , for Anya $ Typing `on` [Thursday]

  , MasterClass `on` [Wednesday, Friday]
  , for Anya $ MasterClass `on` [Monday]

  , KhanAcademy `on` [Wednesday, Friday]
  ]
  where
    on subj days
      | weekday date `elem` days = [slot subj]
      | otherwise = []

    for k x = if k == kid then x else []