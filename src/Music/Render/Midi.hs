{-|
    Module      :  Music.Render.Midi
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    ExistentialQuantification #-}

module Music.Render.Midi
(
    Seconds,
    Semitones,
    Midi(..),
    MidiNote(..),
    writeMidi,

    renderMidi,
    renderMidiTracks,
)
where

import Prelude hiding ( reverse )

import Data.Binary
import Data.Binary.Put
import Data.Convert
import Data.List( sortBy )
import Data.Maybe( maybeToList )
import Data.Ord ( comparing )
import Data.Word

import Codec.Midi hiding ( Time )

import Music.Time
import Music.Time.Score
import Music.Time.Event
import Music.Time.EventList
import Music.Internal.Time.Score ( renderScore )


type Seconds   = Double
type Semitones = Double

data MidiNote
    = MidiNote
    {
        -- | Midi channel (0-15).
        midiNoteChannel  :: Int,

        -- | Midi instrument (0-127).
        midiNoteInstrument :: Maybe Int,

        -- | Midi pitch (0-127).
        midiNotePitch    :: Int,

        -- | Midi pitch adjustment ((-1)-1).
        midiNoteBend     :: Semitones,

        -- | Midi velocity (0-127).
        midiNoteVelocity :: Int
    }
    | MidiTremNote
    {
        midiNoteChannel :: Int
    }
    deriving (Eq, Show)

instance Render (Score Seconds MidiNote) Midi where
    render = renderMidi . renderScore

instance Render (EventList Seconds MidiNote) Midi where
    render = renderMidi


renderMidi :: EventList Seconds MidiNote -> Midi
renderMidi = Midi MultiTrack (TicksPerBeat division)
--           . removeEmptyTracks
           . renderMidiTracks
           . normalize

-- FIXME does not remove now as the TrackEnd event makes every track non-empty
-- removeEmptyTracks :: [[a]] -> [[a]]
-- removeEmptyTracks = filter (not . null)

renderMidiTracks :: EventList Seconds MidiNote -> [Track Ticks]
renderMidiTracks (EventList totalDur events) =
    [controlTrack totalDur] ++
        do  channel <- [0..15]
            return . fromAbsTime
                   . sortBy (comparing fst)
                   $ map (\m -> (0, m)) (tuningProgramChange channel channel)
                       ++ concatMap renderNoteEvent (filter (eventChannel channel) events)
                       ++ [(renderTime (totalDur + 1), TrackEnd)]
            where
                eventChannel x  =  (== x) . midiNoteChannel . eventValue

controlTrack :: Seconds -> Track Ticks
controlTrack totalDur = [(0, TempoChange 1000000), (renderTime (totalDur + 1), TrackEnd)]

renderNoteEvent :: Event Seconds MidiNote -> [(Ticks, Message)]
renderNoteEvent (Event time dur (MidiNote channel instr pitch bend velocity)) =
    programChangeMessages ++ tuningMessages ++ noteMessages
    where  programChangeMessages  =  renderProgram time channel instr
           tuningMessages         =  renderTune time channel pitch' bend'
           noteMessages           =  renderNote time dur channel pitch' velocity
           (pitch', bend')        =  adjustPitch (pitch, bend)

renderProgram :: Seconds -> Int -> Maybe Int -> [(Ticks, Message)]
renderProgram time channel instr = do
    program <- maybeToList instr
    return (renderTime time, ProgramChange channel program)

renderTune :: Seconds -> Int -> Int -> Double -> [(Ticks, Message)]
renderTune time channel pitch bend | bend == 0  = []
renderTune time channel pitch bend | otherwise  = 
    [ ( renderTime time, tuneMessage channel $ tuneParams pitch bend ) ]

renderNote :: Seconds -> Seconds -> Int -> Int -> Int -> [(Ticks, Message)]
renderNote time dur channel pitch velocity =
    [ (renderTime time,         NoteOn  channel pitch velocity ),
      (renderTime (time + dur), NoteOff channel pitch velocity )]

adjustPitch :: (Int, Double) -> (Int, Double)
adjustPitch (p, b) | b <  (-1)        =  (p - 1, 0)
                   | b <  0           =  (p - 1, b + 1)
                   | b == 0           =  (p, 0)
                   | b >  0 && b < 1  =  (p, b)
                   | otherwise        =  (p, 1)

renderTime :: Seconds -> Ticks
renderTime t = round (t * fromIntegral division)


-- | Writes the given graphic to a MIDI file.
writeMidi :: FilePath -> Midi -> IO ()
writeMidi = exportFile

division = 1024

--
-- Intonation
--

type TuneId = (Word8, Cents)
type Cents = (Word8, Word8)

tuneParams :: Int -> Double -> TuneId
tuneParams p d = (fromIntegral p, cents d)
    where cents d   =  (fromIntegral c0, fromIntegral c1)
          (c0, c1)  =  flip divMod (128::Int) $
                           fst $ properFraction (d/deltaTune)

-- | 1 semitone / 2^14
deltaTune :: Double
deltaTune = 0.000061

tuningProgramChange :: Int -> Int -> [Message]
tuningProgramChange ch pgm =
    [ ControlChange ch 0x64 0x03, 
      ControlChange ch 0x65 0x0, 
      ControlChange ch 0x06 pgm ]

tuneMessage :: Int -> TuneId -> Message
tuneMessage p (x, (a, b)) = Sysex 0xf0 $
    runPut $ do
        putWord8 0x7f   -- universal SysEx real-time
        putWord8 0      -- device id
        putWord8 8      -- midi tuning
        putWord8 2      -- note change
        putWord8 p'     -- tuning prog n
        putWord8 1      -- number of changes
        putWord8 x      -- key
        putWord8 x      -- base midi note
        putWord8 a      -- low tuning bits
        putWord8 b      -- hi  tuning bits
        putWord8 0xf7
    where p' = fromIntegral p
