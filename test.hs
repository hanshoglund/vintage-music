
{-# LANGUAGE 
--    MonadComprehensions,
    TransformListComp,
    NoMonomorphismRestriction #-}

import Prelude hiding (reverse)

import Music                  
import Music.Time.EventList
import Music.Render.Midi
import Music.Render.Graphics
import Music.Utilities

c, d, e, f, g, a, b :: Int
cb = (-1)
c  = 0
c' = 1
db = 1
d  = 2
d' = 3
eb = 3
e  = 4
e' = 5
fb = 5
f  = 5
f' = 6
gb = 6
g  = 7
g' = 8
ab = 8
a  = 9
a' = 10
bb = 10
b  = 11
b' = 12

frere =     line [c, d, e, c] 
        >>> line [c, d, e, c]
        >>> line [e, f] >>> stretch 2 (note g)
        >>> line [e, f] >>> stretch 2 (note g)
        >>> stretch (1/2) (line [g, a, g, f]) >>> line [e, c]
        >>> stretch (1/2) (line [g, a, g, f]) >>> line [e, c]
        >>> note c >>> note (g - 12) >>> stretch 2 (note c)
        >>> note c >>> note (g - 12) >>> stretch 2 (note c)

canon 0 t x s = instant
canon n t x s = s ||| delay t (stretch x (canon (n - 1) t x s))

frere' =  canon 4 8 1 frere 
    ||| delay 28 (canon 4  7.9 0.9 frere)
    ||| (fmap (+ 12) . delay 46) (canon 4 7.9 0.9 frere)
    ||| (fmap (- 12) . delay 60) (canon 4 7.9 1.1 frere)
    ||| (fmap (+ 16) . delay 75) (canon 4 4.5 0.5 frere)


phrase = stretch (1/6) $ line [0, 5, 7, 0, (-5), 2] 
reich = (before 200) $ loop phrase ||| (stretch 1.01 (loop phrase))


minor  = chord [0,3,7]
chords = loop minor
melody = loop . stretch (1/2) $ line [0,-1,0,-5,-3,-1]
both   = stretch (1/2) . before 6 $ melody ||| chords

test :: Score Double Int
test = reich

toMidiNotes :: Score Double Int -> Score Double MidiNote
toMidiNotes = fmap (\p -> MidiNote 0 (p+60) 0 60)

openScore score = do
    openMidi score
    openPdf score

openMidi score = do
    writeMidi "test.mid" (renderMidi . render . toMidiNotes $ score)
    openMidiFile "test.mid"
    return ()

openPdf score = do
    writePdf "test.pdf" (renderGraphics score)
    openFile "test.pdf"
    return ()

main = openScore test