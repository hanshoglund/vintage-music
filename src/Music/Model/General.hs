

module Music.Model.General
(
  Time
, Attr
, Event
, Voice
, Part
, Orchestration
, Spacialization
, duration
, attributes
, sequence
, parallel
)
where

import Prelude hiding (sequence)
    
data TODO = Dummy

type Time  = Int
type Attr  = Int -> Int
type Event = TODO
type Part  = TODO

type Voice          = [Event]
type Orchestration  = Time -> [Part]
type Spacialization = Time -> (Float, Float, Float)

duration   :: Event -> Time
attributes :: Event -> [Attr]

sequence  :: Voice -> Voice -> Voice
parallel  :: Voice -> Voice -> Voice


duration   = undefined
attributes = undefined
sequence   = undefined
parallel   = undefined