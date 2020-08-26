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
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericFromEnum, genericToEnum)

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
  Maths -> light "#EFE"
  Programming -> light "#FEE"
  Typing -> light "#EAE"
  Spanish -> light "#FFE"
  LTW -> light "#EEF"
  Latin -> light "#BFB"
  Piano -> light "BBF"
  Solfege -> light "FBB"
  Harmony -> light "BFF"
  History -> light "FFB"
  Outschool -> light "9C9"
  MasterClass -> light "99C"
  KhanAcademy -> light "9CC"
  Essay -> light "CC9"
  Biology -> light "C99"
  where
    light = { back: _, fore: "black" }
    dark = { back: _, fore: "white" }

slotLengthMinutes :: Subject -> Int
slotLengthMinutes = case _ of
  Typing -> 30
  _ -> 40