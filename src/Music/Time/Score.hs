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
    arpeggio,           -- :: Time t => t -> [a] -> Score t a
    
-- * Folds
    foldOffset,         -- :: (Time t, Monoid m) => (t -> m) -> Score t a -> m
    foldDuration,       -- :: (Time t, Monoid m) => (t -> m) -> Score t a -> m
    foldValue,          -- :: (Time t, Monoid m) => (a -> m) -> Score t a -> m

    numberOfEvents,     -- :: Time t => Score t a -> Int
    meanDuration,       -- :: Time t => Score t a -> t
    normalizeDuration,  -- :: Time t => Score t a -> Score t a
)
where

import Prelude hiding ( lines )
import Music.Internal.Time.Score


