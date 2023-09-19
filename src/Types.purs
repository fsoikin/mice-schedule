module Types
  ( Kid(..)
  , Subject(..)
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
derive instance Eq Kid
derive instance Ord Kid

data Subject
  = Maths
  | Physics
  | Programming
  | Piano
  | PianoPractice
  | Spanish
  | French
  | VariousHomework
  | College
  | CreativeWriting
  | Calculus
derive instance Eq Subject
derive instance Ord Subject
derive instance Generic Subject _

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
  Maths -> "Mathematics"
  Physics -> "Physics"
  Programming -> "Programming"
  Piano -> "Piano"
  PianoPractice -> "Piano practice"
  Spanish -> "Spanish"
  French -> "French"
  VariousHomework -> "Various homework"
  College -> "College"
  CreativeWriting -> "Writing"
  Calculus -> "Calculus"

subjectColor :: Subject -> { back :: String, fore :: String }
subjectColor = case _ of
  Maths -> { back: "#F4EDEA", fore: "#000000" }
  Physics -> { back: "#AEDFF7", fore: "#000000" }
  Programming -> { back: "#F4DADA", fore: "#000000" }
  Piano -> { back: "#B2DFDB", fore: "#000000" }
  PianoPractice -> { back: "#FFCDD2", fore: "#000000" }
  Spanish -> { back: "#FFECB3", fore: "#000000" }
  French -> { back: "#B3E5FC", fore: "#000000" }
  VariousHomework -> { back: "#D1C4E9", fore: "#000000" }
  College -> { back: "#FFE0B2", fore: "#000000" }
  CreativeWriting -> { back: "#C8E6C9", fore: "#000000" }
  Calculus -> { back: "#D7CCC8", fore: "#000000" }

slotLengthMinutes :: Subject -> Int
slotLengthMinutes = case _ of
  Spanish -> 60
  Piano -> 45
  CreativeWriting -> 30
  _ -> 40
