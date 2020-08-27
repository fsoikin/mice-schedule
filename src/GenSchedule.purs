module GenSchedule
  ( MergeSchedule
  , Rule
  , Slot
  , TimedRow
  , genSchedule
  , slot
  , timeSlot
  , whenDate
  , isDay
  ) where

import Prelude

import Data.Array (cons, drop, filter, find, head, last, mapMaybe, snoc, sortBy, uncons, zipWith)
import Data.Date (Date, Weekday, weekday)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Time (TimeRange, addMinutes, cutTimeRange, minuteDiff, time)
import Types (Kid, Subject, slotLengthMinutes)

breakLengthMinutes = 10 :: Int
dayStart = time 10 0 0 :: TimeRange

type Rule = Date -> Kid -> MergeSchedule
type Slot = { time :: Maybe TimeRange, subject :: Subject }
type MergeSchedule = Array Slot -> Array Slot

type TimedRow = { time :: TimeRange, softTime :: Boolean, subject :: Subject }

genSchedule :: Array (Date -> Kid -> MergeSchedule) -> Date -> Kid -> Array TimedRow
genSchedule scheds date kid =
  scheds
  # foldl (\sch f -> f date kid sch) []
  # split
  # merge
  # sortBy (comparing _.time.hour <> comparing _.time.minute)
  where
    split xs = { ordered, timed, gaps }
      where
        ordered = xs # filter (_.time >>> isNothing) <#> _.subject
        timed = xs # mapMaybe \x -> x.time <#> { subject: x.subject, softTime: false, time: _ }

        taken =
          timed
          <#> (\r -> r.time { duration = r.time.duration + breakLengthMinutes })
          # sortBy (comparing _.hour <> comparing _.minute)

        gaps =
          zipWith gap taken (taken # drop 1)
          # addInitialGap
          # addTrailingGap

        gap prev next =
          let start = addMinutes prev.duration prev
          in start { duration = minuteDiff next start }

        addInitialGap gs =
          let initial = dayStart { duration = minuteDiff <$> head taken <@> dayStart # fromMaybe 0 }
          in if initial.duration > 0 then cons initial gs else gs

        addTrailingGap gs =
          let start = last taken <#> (\lst -> addMinutes lst.duration lst) # fromMaybe dayStart
          in snoc gs $ start { duration = 60*10 }

    merge { ordered, timed, gaps } =
      case uncons ordered of
        Nothing -> timed
        Just { head, tail } ->
          let fit = case fitIntoGap head of
                      Nothing ->
                        { slot: attachAtEnd head, gaps }
                      Just f ->
                        { slot: f
                        , gaps: cutTimeRange gaps $ f.time { duration = f.time.duration + breakLengthMinutes }
                        }
          in merge { ordered: tail, timed: snoc timed fit.slot, gaps: fit.gaps }
      where
        fitIntoGap subj = do
          let slotLen = slotLengthMinutes subj
          g <- gaps # find \t -> t.duration >= slotLen + breakLengthMinutes
          pure { time: time g.hour g.minute slotLen, softTime: true, subject: subj }

        attachAtEnd subj =
          { time: time 23 0 (slotLengthMinutes subj), softTime: true, subject: subj }

whenDate :: (Date -> Boolean) -> (Kid -> MergeSchedule) -> Date -> Kid -> MergeSchedule
whenDate p f d
  | p d = f
  | otherwise = const identity

isDay :: Weekday -> Date -> Boolean
isDay wd = weekday >>> eq wd

timeSlot :: Int -> Int -> Int -> Subject -> Slot
timeSlot hour minute duration subject =
  { time: Just $ time hour minute duration, subject }

slot :: Subject -> Slot
slot = { time: Nothing, subject: _ }
