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

    lineStretch,        -- :: Time t => [(a, t)] -> Score t a
    chordDelay,         -- :: Time t => [(a, t)] -> Score t a
    arpeggio,           -- :: Time t => t -> [a] -> Score t a
    
-- -- * Normal forms
--    homophonic,         -- :: Time t => Score t a -> [Score t a]
--    polyphonic,         --  :: Time t => Score t a -> [Score t a]

-- * Exporting scores
    render,
    unrender
)
where

import Music.Internal.Time.Score


