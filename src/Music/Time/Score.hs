{-|
    Module      :  Music.Time.Score
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Music.Time.Score
(
-- * Score type
    Score,
    
-- * Creating
    note,                 
    line,               
    chord,              
    lines,              
    chords,             
                        
    lineStretch,        
    chordDelay,         
    chordDelayStretch,  
    arpeggio,           
                        
-- * Transforming
    normalizeDuration,  

-- * Folding
    filterEvents,
    removeEvents,
    partitionEvents,
    numberOfEvents,
    meanDuration,
    firstEvent,         
    lastEvent,     
    mapFirstEvent,     
    printScoreEvents,   
    toList,
    toEvents,
    toEventList,

-- TODO normal forms and related maps
)
where

import Prelude hiding ( lines )
import Music.Internal.Time.Score

