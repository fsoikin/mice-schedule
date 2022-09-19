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
  | Essay
  | Maths
  | Physics
  | HigherMaths
  | Piano
  | Programming
  | ProgrammingEdx
  | ArchitectureEdx
  | Spanish
  | VoiceTraining
  | VariousHomework
  | RoomCleaning
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
  Essay -> "Essay"
  Maths -> "Maths"
  Physics -> "Physics"
  HigherMaths -> "Higher Maths"
  Piano -> "Piano"
  Programming -> "Programming"
  ProgrammingEdx -> "EdX programming"
  ArchitectureEdx -> "EdX architecture"
  Spanish -> "Spanish"
  VariousHomework -> "Various Homework"
  VoiceTraining -> "Voice Training"
  RoomCleaning -> "Room Cleaning"

subjectColor :: Subject -> { back :: String, fore :: String }
subjectColor = case _ of
  VariousHomework -> light "rgb(195,201,255)"
  Biology -> light "rgb(204,204,204)"
  Essay -> light "rgb(253,204,138)"
  Lunch -> light "white"
  Maths -> dark "rgb(0,109,44)"
  Physics -> dark "rgb(186,228,188)"
  HigherMaths -> light "rgb(108,226,226)"
  Piano -> dark "rgb(8,104,172)"
  Programming -> light "rgb(44,162,95)"
  ProgrammingEdx -> light "rgb(88,162,95)"
  ArchitectureEdx -> light "rgb(178,226,226)"
  Spanish -> dark "rgb(136,86,167)"
  VoiceTraining -> light "rgb(88,162,95)"
  RoomCleaning -> light "rgb(215,181,216)"
  where
    light = { back: _, fore: "black" }
    dark = { back: _, fore: "white" }

slotLengthMinutes :: Subject -> Int
slotLengthMinutes = case _ of
  Spanish -> 20
  Piano -> 30
  VoiceTraining -> 20
  Essay -> 30
  _ -> 40
