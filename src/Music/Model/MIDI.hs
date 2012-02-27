{-|
    Module      :  Music.Model.Midi
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FunctionalDependencies #-}

module Music.Model.MIDI
-- (
-- )
where
    
import Data.Word

import Music.Pitch
import Music.Dynamics
import Music.Time
import Music.Time.Score

type MidiTime = Double

data MidiNote
    = MidiNote
    {
    midiNoteOn    :: [MidiEvent],
    midiNoteAfter :: [(MidiTime, MidiEvent)],
    midiNoteOff   :: [MidiEvent]
    }
instance Pitched MidiTime MidiNote where
    composePitch f op (MidiNote on after off) 
        = MidiNote (map (composePitch f op) on) 
                   (map (\(t, e) -> (t, composePitch f op e)) after)
                   (map (composePitch f op) off)
--    frequency :: p -> t -> Frequency
instance Dynamic MidiTime MidiNote where

data MidiEvent
    = MidiEvent
    { 
    midiEventStatus :: Word8,
    midiEventData1  :: Word8,
    midiEventData2  :: Word8
    }
instance Pitched MidiTime MidiEvent where
instance Dynamic MidiTime MidiEvent where
    
