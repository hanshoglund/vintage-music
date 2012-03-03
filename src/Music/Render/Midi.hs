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

import Data.List(sortBy, partition)
import Data.Function(on)

import Data.Binary
import Data.Convert
import Data.Binary.Put
import Data.Ord ( comparing )
import Data.Word

import Codec.Midi hiding (Time)

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
    deriving (Eq, Show)

instance Render (Score Seconds MidiNote) Midi where
    render = renderMidi . renderScore

instance Render (EventList Seconds MidiNote) Midi where
    render = renderMidi


renderMidi :: EventList Seconds MidiNote -> Midi
renderMidi = Midi MultiTrack (TicksPerBeat division) 
           . removeEmptyTracks -- FIXME does not remove now as the TrackEnd event makes every track non-empty
           . renderMidiTracks 
           . normalize

removeEmptyTracks :: [[a]] -> [[a]]
removeEmptyTracks = filter (not . null)

controlTrack :: Seconds -> Track Ticks
controlTrack totalDur = [(0, TempoChange 1000000), (renderTime (totalDur + 1), TrackEnd)]

renderMidiTracks :: EventList Seconds MidiNote -> [Track Ticks]
renderMidiTracks (EventList totalDur events) =
    [controlTrack totalDur] ++
        do  channel <- [0..15] -- FIXME 0-15
            return . fromAbsTime 
                   . sortBy (comparing fst) 
                   $ map (\m -> (0, m)) (tuningProgramChange channel channel)
                     ++ concatMap renderEvent (filter (on channel) events) 
                     ++ [(renderTime (totalDur + 1), TrackEnd)]
            where
                on channel  =  (== channel) . midiNoteChannel . eventValue

renderEvent :: Event Seconds MidiNote -> [(Ticks, Message)]
renderEvent (Event t d (MidiNote c i p b v)) =
    let (p', b') = adjustPitch (p, b) in
        case i of 
            Nothing  -> []
            (Just pgm) -> [(renderTime t, ProgramChange c pgm)]
        ++
        [ ( renderTime t       , tuneMessage c $ tuneParams p' b' )
        , ( renderTime t       , NoteOn  c p' v )
        , ( renderTime (t + d) , NoteOff c p' v ) ]

adjustPitch :: (Int, Double) -> (Int, Double)
adjustPitch (p, b) | b <  (-1)        =  (p - 1, 0)
                   | b <  0           =  (p - 1, b + 1)
                   | b == 0           =  (p, 0)
                   | b >  0 && b < 1  =  (p, b)
                   | otherwise        =  (p, 1)
                   
renderTime :: Seconds -> Int
renderTime t = round (t * fromIntegral division)


-- | Writes the given graphic to a MIDI file.
writeMidi :: FilePath -> Midi -> IO ()
writeMidi = exportFile

division = 1024

--
-- Intonation
--

type TuneId = (KeyId, Cents)
type KeyId = Word8

type Cent0 = Word8
type Cent1 = Word8

type Cents = (Cent0, Cent1)

tuneParams :: Int -> Double -> TuneId
tuneParams p d = (fromIntegral p, cents d)

cents :: Double -> (Cent0, Cent1)
cents d = (fromIntegral c0, fromIntegral c1)
    where (c0, c1) = flip divMod (128::Int) $
                        fst $ properFraction (d/deltaTune)

-- | 1 semitone / 2^14
deltaTune :: Double
deltaTune = 0.000061


tuningProgramChange :: Int -> Int -> [Message]
tuningProgramChange ch pgm = 
    [ ControlChange ch 0x64 0x03
    , ControlChange ch 0x65 0x0
    , ControlChange ch 0x06 pgm ]

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
