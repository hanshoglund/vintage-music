{-|
    Module      :  Music.Render.Midi
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    ExistentialQuantification #-}

module Music.Render.Midi
(
    Seconds,
    Semitones,
    Midi(..),
    MidiNote(..),
    renderMidi,
    writeMidi,
)
where

import Prelude hiding ( reverse )

import Data.List(sortBy, partition)
import Data.Function(on)

import Data.Word
import Data.Binary
import Data.Binary.Put
import Data.Ord ( comparing )

import Codec.Midi hiding (Time)

import Music.Time
import Music.Time.Score
import Music.Time.Event
import Music.Time.EventList


type Seconds   = Double
type Semitones = Double

data MidiNote = MidiNote
    {
        -- | Midi channel.
        midiNoteChannel  :: Int,
        -- | Midi pitch.
        midiNotePitch    :: Int,
        -- | Number of semitones (up or down).
        midiNoteBend     :: Semitones,
        -- | Midi velocity.
        midiNoteVelocity :: Int
    }  
    deriving (Eq, Show)

renderMidi :: EventList Seconds MidiNote -> Midi
renderMidi = Midi MultiTrack (TicksPerBeat 1024) . removeEmptyTracks. renderMidiTracks . normalize

removeEmptyTracks = filter (not . null)

controlTrack :: Seconds -> Track Ticks
controlTrack totalDur = [(0, TempoChange 1000000), (renderTime (totalDur + 1), TrackEnd)]

renderMidiTracks :: EventList Seconds MidiNote -> [Track Ticks]
renderMidiTracks (EventList duration events) =
    [controlTrack duration] ++
        do  channel <- [0..16]
            return $ fromAbsTime . sortBy (comparing fst) $ concatMap renderEvent (filter (on channel) events)
            where
                on channel  =  (== channel) . midiNoteChannel . eventValue

renderEvent :: Event Seconds MidiNote -> [(Ticks, Message)]
renderEvent (Event t d (MidiNote c p b v)) =
    [
        ( renderTime t       , NoteOn  c p v ),
        ( renderTime (t + d) , NoteOff c p v )
    ]

renderTime :: Seconds -> Int
renderTime t = round (t * fromIntegral 1024)


writeMidi :: FilePath -> Midi -> IO ()
writeMidi = exportFile


--
-- Intonation
--

type TuneId = (KeyId, Cents)
type KeyId = Word8

type Cent0 = Word8
type Cent1 = Word8

type Cents = (Cent0, Cent1)

tuneParams :: Int -> Double -> Maybe TuneId
tuneParams p d = Just (fromIntegral p, c)
 --   | c == (0, 0) = Nothing
 --   | otherwise   = Just (fromIntegral p, c)
    where c = cents d

cents :: Double -> (Cent0, Cent1)
cents d = (fromIntegral c0, fromIntegral c1)
    where (c0, c1) = flip divMod (128::Int) $
                        fst $ properFraction (d/deltaTune)

-- | 1 semitone / 2^14
deltaTune :: Double
deltaTune = 0.000061


tuneMessage :: TuneId -> Message
tuneMessage (x, (a, b)) = Sysex 240 $
    runPut $ do
        putWord8 127
        putWord8 0
        putWord8 8
        putWord8 2
        putWord8 0
        putWord8 1
        putWord8 x
        putWord8 x
        putWord8 a
        putWord8 b
        putWord8 247


