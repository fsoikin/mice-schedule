module Time
  ( TimeRange
  , Time', Time
  , addMinutes
  , cutTimeRange
  , intersect
  , minuteDiff
  , time
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))

type TimeRange = Time' ( duration :: Int )
type Time' r = { hour :: Int, minute :: Int | r }
type Time = Time' ()

time :: Int -> Int -> Int -> TimeRange
time = { hour: _, minute: _, duration: _ }

addMinutes :: forall r. Int -> Time' r -> Time' r
addMinutes m t = t { hour = t.hour + nextMinutes `div` 60, minute = nextMinutes `mod` 60 }
  where nextMinutes = t.minute + m

minuteDiff :: forall r. Time' r -> Time' r -> Int
minuteDiff a b = (a.hour*60 + a.minute) - (b.hour*60 + b.minute)

intersect :: TimeRange -> TimeRange -> TimeRange
intersect a b = start { duration = minuteDiff end start }
  where
    aEnd = addMinutes a.duration a
    bEnd = addMinutes b.duration b
    start = if minuteDiff a b < 0 then b else a
    end = if minuteDiff aEnd bEnd < 0 then aEnd else bEnd

cutTimeRange :: Array TimeRange -> TimeRange -> Array TimeRange
cutTimeRange segments t = do
  s <- segments

  let i = intersect s t
      iEnd = addMinutes i.duration i
      sEnd = addMinutes s.duration s
      before = s { duration = minuteDiff i s }
      after = iEnd { duration = minuteDiff sEnd iEnd }

  if i.duration > 0 then
    catMaybes
      [ if before.duration > 0 then Just before else Nothing
      , if after.duration > 0 then Just after else Nothing
      ]
  else
    [s]
