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
    
-- * Miscellaneous
    normalizeDuration,  -- :: Time t => Score t a -> Score t a
)
where

import Prelude hiding ( lines )
import Music.Internal.Time.Score


