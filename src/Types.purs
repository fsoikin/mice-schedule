module Types
  ( Kid(..)
  , Subject(..)
  , Teacher(..)
  , allKids
  , allSubjects
  , kidIcon
  , kidName
  , slotLengthMinutes
  , subjectName
  , subjectColor
  ) where

import Prelude

import Data.Array (mapMaybe, (..))
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericFromEnum, genericToEnum)

data Kid = Matvey | Anya
derive instance eqKid :: Eq Kid
derive instance ordKid :: Ord Kid

data Teacher = Alisa | Fyodor | Ismini | Luba

data Subject
  = Lunch
  | Biology
  | Chinese
  | Essay
  | History
  | Maths
  | Outschool
  | Physics
  | Piano
  | Programming
  | Spanish
  | Typing
  | VoiceTraining
  | Writing
derive instance eqSubject :: Eq Subject
derive instance ordSubject :: Ord Subject
derive instance gSubject :: Generic Subject _

allKids :: Array Kid
allKids = [Matvey, Anya]

allSubjects :: Array Subject
allSubjects = mapMaybe genericToEnum (genericFromEnum (genericBottom :: Subject) .. genericFromEnum (genericTop :: Subject))

kidName :: Kid -> String
kidName k = kidIcon k <> case k of
  Matvey -> " Матвей"
  Anya -> " Аня"

kidIcon :: Kid -> String
kidIcon = case _ of
  Matvey -> "🦔"
  Anya -> "🐁"

subjectName :: Subject -> String
subjectName = case _ of
  Lunch -> "Lunch"
  Biology -> "Biology"
  Chinese -> "Chinese"
  Essay -> "Essay"
  History -> "History"
  Maths -> "Maths"
  Outschool -> "Outschool"
  Physics -> "Physics"
  Piano -> "Piano"
  Programming -> "Programming"
  Spanish -> "Spanish"
  Typing -> "Typing"
  VoiceTraining -> "Voice training"
  Writing -> "Writing"

subjectColor :: Subject -> { back :: String, fore :: String }
subjectColor = case _ of
  Lunch -> light "white"
  Maths -> dark "rgb(0,109,44)"
  Programming -> light "rgb(44,162,95)"
  Typing -> light "rgb(178,226,226)"
  Spanish -> dark "rgb(136,86,167)"
  Chinese -> dark "rgb(223,101,176)"
  Physics -> dark "rgb(186,228,188)"
  Piano -> dark "rgb(8,104,172)"
  VoiceTraining -> light "rgb(123,204,196)"
  History -> dark "rgb(150,150,150)"
  Outschool -> dark "rgb(221,28,119)"
  Essay -> light "rgb(253,204,138)"
  Biology -> light "rgb(204,204,204)"
  Writing -> light "rgb(215,181,216)"
  where
    light = { back: _, fore: "black" }
    dark = { back: _, fore: "white" }

slotLengthMinutes :: Subject -> Int
slotLengthMinutes = case _ of
  Typing -> 30
  _ -> 40
