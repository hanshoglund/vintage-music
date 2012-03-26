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
    
-- * Creating
    note,               -- :: Time t => a -> Score t a  
    line,               -- :: Time t => [a] -> Score t a
    chord,              -- :: Time t => [a] -> Score t a
    lines,              -- :: Time t => [[a]] -> Score t a
    chords,             -- :: Time t => [[a]] -> Score t a

    lineStretch,        -- :: Time t => [(a, t)] -> Score t a
    chordDelay,         -- :: Time t => [(a, t)] -> Score t a
    chordDelayStretch,  -- :: Time t => [(t, t, a)] -> Score t a
    arpeggio,           -- :: Time t => t -> [a] -> Score t a
    
-- * Transforming
    restBefore,         -- :: Time t => t -> Score t a -> Score t a
    restBoth,           -- :: Time t => t -> Score t a -> Score t a
    restAfter,          -- :: Time t => t -> Score t a -> Score t a
    stretchTo,          -- :: Time t => t -> Score t a -> Score t a
    normalizeDuration,  -- :: Time t => Score t a -> Score t a

-- * Folding
    numberOfEvents,
    meanDuration,
    firstEvent,         -- :: Time t => Score t a -> Maybe a
    lastEvent,          -- :: Time t => Score t a -> Maybe a
    printScoreEvents,   -- :: (Time t, Show t, Show a) => Score t a -> String

-- TODO normal forms and related maps
    -- mapLines :: [a] -> [b] -> Score t a -> Score t b
    -- mapChords :: [a] -> [b] -> Score t a -> Score t b
    -- mapHomophonic :: ([[a]] -> [[b]]) -> Score t a -> Score t b
    -- mapPolyphonic :: ([[a]] -> [[b]]) -> Score t a -> Score t b
    -- normalizeHomophinic :: Score t a -> [[a]]
    -- normalizePolyphonic :: Score t a -> [[a]]
)
where

import Prelude hiding ( lines )
import Music.Internal.Time.Score


