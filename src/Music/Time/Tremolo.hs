{-|
    Module      :  Music.Time.Segment
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    DeriveFunctor,
    DeriveFoldable #-}

module Music.Time.Tremolo
(
    Tremolo,
    tremolo,     
    tremoloBetween,
    tremoloSequence,
    renderTremolo,
    renderTremolo',
--    renderTremoloWith
)
where

import Control.Monad ( join )
import System.Random

import Music.Time
import Music.Time.Functors
import Music.Time.Score

-- | A tremolo represents an infinite sequence of events played at some interval.
--
--   This type is intended to be composed with 'Score' as follows:
--
--   * @Score t (Tremolo t a)@ represent a score of events invariant under 'stretch'
--
--   * @Score t (Either a (Tremolo t b))@  represent a score of ordinary events and events invariant under 'stretch'


data Tremolo t a 
    = Tremolo 
    {                       
        tremoloPeriod :: t,
        tremoloEvents :: [a] -- always infinite
    }
    deriving ( Eq, Show ) -- FIXME show for debug

-- | Repeats the given event at the given period.
tremolo :: Time t => t -> a -> Tremolo t a
tremolo p x = tremoloSequence p [x]

-- | Alternatively play the given event at the given period.
tremoloBetween :: Time t => t -> a -> a -> Tremolo t a
tremoloBetween p x y = tremoloSequence p [x, y]

-- | Repeat the given sequence of events at the given period.
tremoloSequence :: Time t => t -> [a] -> Tremolo t a
tremoloSequence p []  =  errorEmpty "Music.Time.Tremolo.tremoloSequence"
tremoloSequence p xs  =  Tremolo p . cycle $ xs

-- | Render tremolos to events.
renderTremolo :: Time t => Score t (Tremolo t a) -> Score t a
renderTremolo = join . dmap renderTremolo'

renderTremolo' d (Tremolo p xs) = 
    compress (d * recip p) . restAfter f . line . take n $ xs
    where (n, f) = properFraction (d / p)
    
-- TODO randomize scaling etc
-- renderTremoloWith :: RandomGen g => g -> Time t => Score t (Tremolo a) -> Score t a
-- renderTremoloWith = undefined

errorEmpty name = error $ name ++ ": empty list"