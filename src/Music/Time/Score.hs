{-|
    Module      :  Music.Time.Score
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    DeriveFunctor, 
    DeriveFoldable #-}

module Music.Time.Score
(
-- * Score type
    Score,
    
-- * Creating scores
    note,               -- :: Time t => a -> Score t a  
    line,               -- :: Time t => [a] -> Score t a
    chord,              -- :: Time t => [a] -> Score t a
    lines,              -- :: Time t => [[a]] -> Score t a
    chords,             -- :: Time t => [[a]] -> Score t a

    lineStretch,        -- :: Time t => [(a, t)] -> Score t a
    chordDelay,         -- :: Time t => [(a, t)] -> Score t a
--    chordDelayStretch,  -- :: Time t => [(t, t, a)] -> Score t a
    arpeggio,           -- :: Time t => t -> [a] -> Score t a
    
-- * Miscellaneous
    firstEvent,         -- :: Time t => Score t a -> Maybe a
    lastEvent,          -- :: Time t => Score t a -> Maybe a
    compress,           -- :: Time t => t -> Score t a -> Score t a
    restBefore,         -- :: Time t => t -> Score t a -> Score t a
    restBoth,           -- :: Time t => t -> Score t a -> Score t a
    restAfter,          -- :: Time t => t -> Score t a -> Score t a
    stretchTo,          -- :: Time t => t -> Score t a -> Score t a
    normalizeDuration,  -- :: Time t => Score t a -> Score t a

    printScoreEvents,   -- :: (Time t, Show t, Show a) => Score t a -> String
)
where

import Prelude hiding ( lines )
import Music.Internal.Time.Score


