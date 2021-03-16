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
  | Maths
  | Programming
  | Typing
  | Spanish
  | LTW
  | Latin
  | Piano
  | VoiceTraining
  | Solfege
  | Harmony
  | History
  | Outschool
  | MasterClass
  | KhanAcademy
  | Essay
  | Biology
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
  Maths -> "Maths"
  Programming -> "Programming"
  Typing -> "Typing"
  Spanish -> "Spanish"
  LTW -> "LTW"
  Latin -> "Latin"
  Piano -> "Piano"
  VoiceTraining -> "Voice training"
  Solfege -> "Solfege"
  Harmony -> "Harmony"
  History -> "History"
  Outschool -> "Outschool"
  MasterClass -> "MasterClass"
  KhanAcademy -> "KhanAcademy"
  Essay -> "Essay"
  Biology -> "Biology"

subjectColor :: Subject -> { back :: String, fore :: String }
subjectColor = case _ of
  Lunch -> light "white"
  Maths -> dark "rgb(0,109,44)"
  Programming -> light "rgb(44,162,95)"
  Typing -> light "rgb(178,226,226)"
  Spanish -> dark "rgb(136,86,167)"
  LTW -> dark "rgb(227,74,51)"
  Latin -> light "rgb(140,150,198)"
  Piano -> dark "rgb(8,104,172)"
  VoiceTraining -> light "rgb(123,204,196)"
  Solfege -> dark "rgb(67,162,202)"
  Harmony -> light "rgb(186,228,188)"
  History -> dark "rgb(150,150,150)"
  Outschool -> dark "rgb(221,28,119)"
  MasterClass -> light "rgb(223,101,176)"
  KhanAcademy -> light "rgb(215,181,216)"
  Essay -> light "rgb(253,204,138)"
  Biology -> light "rgb(204,204,204)"
  where
    light = { back: _, fore: "black" }
    dark = { back: _, fore: "white" }

slotLengthMinutes :: Subject -> Int
slotLengthMinutes = case _ of
  Typing -> 30
  _ -> 40
