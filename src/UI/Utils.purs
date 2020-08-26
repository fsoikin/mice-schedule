module UI.Utils
  ( showDate
  , subjectStyle
  ) where

import Prelude

import Data.Date (Date, day, month, year)
import Data.Enum (fromEnum)
import Data.String as String
import Elmish.HTML (CSS, css)
import Types (Subject, subjectColor)

subjectStyle :: Subject -> CSS
subjectStyle subj =
  css { color: color.fore, backgroundColor: color.back }
  where
    color = subjectColor subj

showDate :: Date -> String
showDate t = String.joinWith ""
  [ show $ fromEnum $ day t
  , "/"
  , show $ fromEnum $ month t
  , "/"
  , show $ fromEnum $ year t
  ]
