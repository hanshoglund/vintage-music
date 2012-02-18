{-|
    Module      :  Music.Time.Score
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

module Music.Time.Score
(
-- * Score type
    Score,
    
-- * Creating scores
    note,  
    melody,
    chord,           
    
-- * Exporting scores
    render
)
where

import Prelude hiding ( reverse )

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Foldable

import Music.Time
import Music.Time.Event ( Event(..) )
import Music.Time.EventList ( EventList(..) )
import qualified Music.Time.EventList as EventList


{-|
    A discrete temporal structure, generalising standard music notation.            

    > melody [1, 2, 3]
    
    Useful monadic functions:
        'join', 'msum', 'mfilter', 'liftM', 'liftM2', 'liftM3' etc.
-}
data Score t a
    = RestS t
    | NoteS t a
    | ParS (Score t a) (Score t a)
    | SeqS (Score t a) (Score t a)
    deriving 
    (
    -- Eq, 
    -- Show, 
    Functor,
    Foldable
    )

instance Time t => Temporal (Score t) where
    instant  =  RestS 0
    x ||| y  =  x `ParS` y
    x >>> y  =  x `SeqS` y

instance Time t => Loop (Score t) where
    loop x  =  x >>> loop x

instance Time t => Reverse (Score t) where
    reverse (SeqS x y)  =  SeqS (reverse y) (reverse x)
    reverse (ParS x y)  =  ParS (reverse x) (reverse y)
    reverse x           =  x

instance Time t => Timed t (Score t) where
    duration (RestS d)    =  d
    duration (NoteS d x)  =  d
    duration (SeqS x y)   =  duration x + duration y
    duration (ParS x y)   =  duration x `max` duration y

    stretch t (RestS d)   =  RestS (d * t)
    stretch t (NoteS d x) =  NoteS (d * t) x
    stretch t (ParS x y)  =  ParS (stretch t x) (stretch t y)
    stretch t (SeqS x y)  =  SeqS (stretch t x) (stretch t y)

instance Time t => Delayed t (Score t) where
    rest       =  RestS
    delay t x  =  rest t >>> x
    
instance Time t => Monoid (Score t a) where
    mempty   =  RestS 0
    mappend  =  SeqS
                    
instance Time t => Applicative (Score t) where
    pure   =  return
    (<*>)  =  ap

instance Time t => Monad (Score t) where
    return   =  note
    s >>= f  =  (joinScore . fmap f) s


--  This is monadic join for (Score t)
-- 
--  Unfortunately we can not implement Monads in terms of join directly, so we
--  implement (>>=) in terms of joinScore and fmap instead. Users should call
--  join instead of this function as usual.

joinScore :: Time t => Score t (Score t a) -> Score t a
joinScore = 
    mconcat 
    . fmap arrange 
    . EventList.events 
    . render
    where 
        arrange (Event t d x) = (delay t . stretch d) x


--
-- Note and Render
--

note :: Time t => a -> Score t a
render :: Time t => Score t a -> EventList t a

note = NoteS 1

--  The basic implementation for render looks like this, but the one below is more efficient.
-- 
--  render (RestS d)   =  EventList d []
--  render (NoteS d x) =  EventList d [Event t d x]
--  render (ParS x y)  =  render x ||| render y
--  render (SeqS x y)  =  render x >>> render y

render score = 
    let (d, xs) = render' 0 score in 
        EventList.normalize (EventList d xs)

render' t (RestS d)    =  (d, [])

render' t (NoteS d x)  =  (d, [Event t d x])

render' t (ParS x y)   =  let (dx, ex) = render' t x
                              (dy, ey) = render' t y         in  (dx `max` dy, ex ++ ey)

render' t (SeqS x y)   =  let (dx, ex) = render' t x
                              (dy, ey) = render' (t + dx) y  in  (dx + dy, ex ++ ey)



chord :: Time t => [a] -> Score t a
chord = mconcat . map note

melody :: Time t => [a] -> Score t a
melody = undefined

homophonic :: Time t => Score t a -> [Score t a]
homophonic = undefined                          

polyphonic :: Time t => Score t a -> [Score t a]
polyphonic = undefined






-- instance (Time t, Eq a, Show a) => Num (Score t a) where
--     x + y       = RestS $ duration x + duration y
--     x * y       = RestS $ duration x + duration y
--     x - y       = RestS $ duration x + duration y
--     abs         = RestS . abs . duration
--     signum      = RestS . signum . duration
--     fromInteger = RestS . fromInteger




-- parallelNormalForm   = parNF
-- sequentialNormalForm = seqNF
-- 
-- -- TODO adjust durs
-- parNF (SeqS(ParSa b) (ParSc d))             
--     | duration a  <   duration b    = undefined -- prolong a to be same length as b
--     | duration a  ==  duration b    = ParS(SeqS(parNF a) (parNF c)) (SeqS(parNF b) (parNF d))
--     | duration a  >   duration b    = undefined -- prolong b to be same length as a
-- 
-- parNF (SeqSx y) = SeqS(parNF x) (parNF y)
-- parNF (ParSx y) = ParS(parNF x) (parNF y)
-- parNF x         = x
-- 
-- 
-- -- TODO adjust durations
-- -- This is actually not possible with non-truncating composition!
-- seqNF (ParS(SeqSa b) (SeqSc d))
--     | duration a  <   duration c    = undefined
--     | duration a  ==  duration c    = (SeqS(ParS(seqNF a) (seqNF c)) (ParS(seqNF b) (seqNF d)))
--     | duration a  >   duration c    = undefined
-- 
-- seqNF (ParSx y) = ParS(seqNF x) (seqNF y)
-- seqNF (SeqSx y) = SeqS(seqNF x) (seqNF y)
-- seqNF x         = x
                      


