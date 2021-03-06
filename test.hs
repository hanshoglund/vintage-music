
{-# LANGUAGE 
    FlexibleInstances,
    MultiParamTypeClasses,
    NoMonomorphismRestriction #-}

import Prelude hiding (reverse)
import Data.Convert

import Music                  
import Music.Time.EventList
import Music.Time.Tremolo
import Music.Render.Midi
import Music.Render.Graphics
import Music.Inspect

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


--
-- Frère Jaques variation
--
frere, frere' :: Score Double Int
frere =     
            line [c, d, eb, c] 
        >>> line [c, d, eb, c]
        >>> line [eb, f] >>> stretch 2 (note g)
        >>> line [eb, f] >>> stretch 2 (note g)
        >>> stretch (1/2) (line [g, ab, g, f]) >>> line [eb, c]
        >>> stretch (1/2) (line [g, ab, g, f]) >>> line [eb, c]
        >>> note c >>> note (g - 12) >>> stretch 2 (note c)
        >>> note c >>> note (g - 12) >>> stretch 2 (note c)

canon :: Time t => Int -> t -> t -> Score t a -> Score t a
canon 0 t x s = instant
canon n t x s = s ||| delay t (stretch x (canon (n - 1) t x s))

frere' =      
        stretch (1/2) $
            canon 4 8 1 frere 
        ||| delay 28 (canon 4  7.9 0.9 frere)
        ||| (fmap (+ 12) . delay 46) (canon 4 7.9 0.9 frere)
        ||| (fmap (- 12) . delay 60) (canon 4 7.9 1.1 frere)
        ||| (fmap (+ 16) . delay 75) (canon 4 4.5 0.5 frere)

--
-- Steve Reich style example
--

phrase, reich :: Score Double Int
phrase = stretch (1/6) $ line [0, 5, 7, 0, (-5), 2] 
reich = before 200 $ loop phrase ||| (stretch 1.01 (loop phrase))

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)


--
-- Romantic example
--

both :: Score Double Int
minor  = chord [0,3,7]
minors = loop minor
melody = loop . stretch (1/2) $ line [0,-1,0,-5,-3,-1]
both   = stretch (1/2) . before 6 $ melody ||| minors


instance Render (Score Double Int) Midi where
    render = render . fmap (\p -> MidiNote 0 Nothing (p + 60) 0 60)

instance Render (Score Double Integer) Midi where
    render = render . fmap (\p -> MidiNote 0 Nothing (fromIntegral p + 60) 0 60)

instance Render (Score Double (Int, Double)) Midi where
    render = render . fmap (\(p,b) -> MidiNote 0 Nothing (p+60) b 60)


-- test :: Score Double MidiNote
-- test = stretch 5 $ 
--                       note (MidiNote 0 60   0.9   80)
--        ||| delay 0.2 (note (MidiNote 1 60   0.45  80))
--        ||| delay 0.4 (note (MidiNote 2 60   0.0   80))

-- test :: Score Double Int
-- test = frere'

-- main = inspect test