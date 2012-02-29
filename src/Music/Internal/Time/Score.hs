{-|
    Module      :  Music.Internal.Time.Score
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

module Music.Internal.Time.Score where

import Prelude hiding ( reverse )

import Control.Monad
import Control.Applicative

import Data.Monoid
import Data.Foldable

import Music.Time
import Music.Time.Functors
import Music.Time.Event ( Event(..) )
import Music.Time.EventList ( EventList(..) )
import qualified Music.Time.EventList as EventList


-- | A discrete temporal structure, generalising standard music notation.
--
--   > melody [1, 2, 3]
--
--   Useful monadic functions:
--       'join', 'liftM', 'liftM2', 'liftM3' etc.

data Score t a
    = RestS t
    | NoteS t a
    | ParS (Score t a) (Score t a)
    | SeqS (Score t a) (Score t a)
    deriving (Functor, Foldable)

instance Time t => Temporal (Score t) where
    instant  =  RestS 0
    (|||)    =  ParS
    (>>>)    =  SeqS

instance Time t => Loop (Score t) where
    loop x  =  x >>> loop x

instance Time t => Reverse (Score t) where
    reverse (ParS x y)  =  ParS (reverse x') (reverse y') where (x', y') = x `assureEqualDur` y
    reverse (SeqS x y)  =  SeqS (reverse y) (reverse x)
    reverse x           =  x

instance Time t => Timed t (Score t) where
    duration (RestS d)    =  d
    duration (NoteS d x)  =  d
    duration (SeqS x y)   =  duration x + duration y
    duration (ParS x y)   =  duration x `max` duration y
    
    stretch t (RestS d)   | t >= 0    =  RestS (d * t)
                          | otherwise = negativeError "Music.Time.Timed.stretch"
    stretch t (NoteS d x) | t >= 0    =  NoteS (d * t) x
                          | otherwise = negativeError "Music.Time.Timed.stretch"
    stretch t (ParS x y)  | t >= 0    =  ParS (stretch t x) (stretch t y)
                          | otherwise = negativeError "Music.Time.Timed.stretch"
    stretch t (SeqS x y)  | t >= 0    =  SeqS (stretch t x) (stretch t y)
                          | otherwise = negativeError "Music.Time.Timed.stretch"

instance Time t => Delayed t (Score t) where
    rest t | t >= 0     =  RestS t
           | otherwise  =  negativeError "Music.Time.Timed.rest"
    delay t x | t >= 0     =  rest t >>> x
              | otherwise  =  negativeError "Music.Time.Timed.delay"

instance Time t => Split t (Score t) where
    before z = filterScore (\t d   -> t < z)
                           (\t d x -> t < z)
    after  z = filterScore (\t d   -> t >= z)
                           (\t d x -> t >= z)

instance Time t => Applicative (Score t) where
    pure   =  return
    (<*>)  =  ap

instance Time t => Monad (Score t) where
    return   =  note
    s >>= f  =  (joinScore . fmap f) s

--   Monadic join for (Score t)
--
--   Unfortunately we can not implement Monads in terms of join directly, so we
--   implement (>>=) in terms of this function instead.
--
--   Control.Monad.join is equivalent and should be used outside this module.

joinScore :: Time t => Score t (Score t a) -> Score t a
joinScore = concatPar . map arrange . toEvents
    where
        arrange (Event t d x) = (delay t . stretch d) x

instance Time t => TimeFunctor t (Score t) where
    tdmap f = foldScore (\t d   -> RestS d)
                        (\t d x -> NoteS d $ f t d x)
                        (\x y   -> ParS x y)
                        (\x y   -> SeqS x y)


--
-- Note and Render
--

-- | Creates a score containing the given element.
note :: Time t => a -> Score t a
note = NoteS 1

-- | Render the given score to an `EventList`.
render :: Time t => Score t a -> EventList t a

--   The basic implementation for render looks like this:
--
--   render (RestS d)   =  EventList d []
--   render (NoteS d x) =  EventList d [Event 0 d x]
--   render (ParS x y)  =  render x ||| render y
--   render (SeqS x y)  =  render x >>> render y
--
--   The actual implementation optimizes this by computing offsets before rendering, instead of after.

render score = let (d, xs) = render' 0 score
                   in EventList.normalize $ EventList d xs

-- offset -> score -> (dur, events)
render' t (RestS d)    =  (d, [])
render' t (NoteS d x)  =  (d, [Event t d x])
render' t (ParS x y)   =
    let (dx, ex) = render' t x
        (dy, ey) = render' t y
                       in (dx `max` dy, ex ++ ey)
render' t (SeqS x y)   =
    let (dx, ex) = render' t x
        (dy, ey) = render' (t + dx) y                -- TODO see below
                       in (dx + dy, ex ++ ey)

-- TODO one single expression above (t + dx) prevents us from reimplementing in terms of foldScore
-- Can we generalize foldScore to include this?


-- | Unrender the given `EventList` back to a score.
unrender :: Time t => EventList t a -> Score t a
unrender = chordStretchDelay . map (\(Event t d x) -> (t, d, x)). eventListEvents



--
-- Derived combinators
--

-- | Creates a score containing the given elements, composed in sequence.
line :: Time t => [a] -> Score t a
line = concatSeq . map note

-- | Creates a score containing the given elements, composed in parallel.
chord :: Time t => [a] -> Score t a
chord = concatPar . map note

-- | Like line, but stretching each note by the given factors.
lineStretch :: Time t => [(t, a)] -> Score t a
lineStretch = concatSeq . map ( \(d, x) -> stretch d $ note x )

-- | Like chord, but delaying each note the given amounts.
chordDelay :: Time t => [(t, a)] -> Score t a
chordDelay = concatPar . map ( \(t, x) -> delay t $ note x )

-- | Like chord, but delays and stretches each note the given amounts.
chordStretchDelay :: Time t => [(t, t, a)] -> Score t a
chordStretchDelay = concatPar . map ( \(t, d, x) -> delay t . stretch d $ note x )

-- | Like chord, but delaying each note the given amount.
arpeggio :: Time t => t -> [a] -> Score t a
arpeggio t xs = chordDelay (zip [0, t ..] xs)


--
-- Internals
--

filterScore :: Time t =>
    (t -> t -> Bool) ->
    (t -> t -> a -> Bool) ->
    Score t a ->
    Score t a
filterScore r n = foldScore (\t d   -> if (r t d)   then RestS d else instant)
                            (\t d x -> if (n t d x) then NoteS d x else instant)
                            (\x y   -> ParS x y)
                            (\x y   -> SeqS x y)

foldScore :: Time t =>
    (t -> t -> b) ->
    (t -> t -> a -> b) ->
    (b -> b -> b) ->
    (b -> b -> b) ->
    Score t a ->
    b
foldScore r n p s score =
    let (d, score') = foldScore' r n p s 0 score in score'

foldScore' :: Time t =>
    (t -> t -> b) ->
    (t -> t -> a -> b) ->
    (b -> b -> b) ->
    (b -> b -> b) ->
    t ->
    Score t a ->
    (t, b)

foldScore' r n p s t (RestS d)    =  (d, r t d)
foldScore' r n p s t (NoteS d x)  =  (d, n t d x)
foldScore' r n p s t (ParS x y)   =
    let (dx, sx) = foldScore' r n p s t x
        (dy, sy) = foldScore' r n p s t y
                                      in (dx `max` dy, p sx sy)
foldScore' r n p s t (SeqS x y)   =
    let (dx, sx) = foldScore' r n p s t x
        (dy, sy) = foldScore' r n p s (t + dx) y
                                      in (dx + dy, s sx sy)

assureEqualDur :: Time t => Score t a -> Score t a -> (Score t a, Score t a)
assureEqualDur x y | dx <  dy  =  (assureDur dy x, y)
                   | dx == dy  =  (x, y)
                   | dx >  dy  =  (x, assureDur dx y)
    where dx = duration x
          dy = duration y

assureDur :: Time t => t -> Score t a -> Score t a
assureDur t s | duration s < t  =  s >>> rest (t - duration s)
              | otherwise       =  s

toEvents :: Time t => Score t a -> [Event t a]
toEvents = eventListEvents . render

negativeError :: String -> a
negativeError name = error $ name ++ ": negative value"



{-

--
-- Normal forms
--

-- TODO

homophonic :: Time t => Score t a -> [Score t a]
homophonic = undefined

polyphonic :: Time t => Score t a -> [Score t a]
polyphonic = undefined

parallelNormalForm   = parNF
sequentialNormalForm = seqNF

-- TODO adjust durs
parNF (SeqS(ParSa b) (ParSc d))
    | duration a  <   duration b    = undefined -- prolong a to be same length as b
    | duration a  ==  duration b    = ParS(SeqS(parNF a) (parNF c)) (SeqS(parNF b) (parNF d))
    | duration a  >   duration b    = undefined -- prolong b to be same length as a

parNF (SeqSx y) = SeqS(parNF x) (parNF y)
parNF (ParSx y) = ParS(parNF x) (parNF y)
parNF x         = x


-- TODO adjust durations
-- This is actually not possible with non-truncating composition!
seqNF (ParS(SeqSa b) (SeqSc d))
    | duration a  <   duration c    = undefined
    | duration a  ==  duration c    = (SeqS(ParS(seqNF a) (seqNF c)) (ParS(seqNF b) (seqNF d)))
    | duration a  >   duration c    = undefined

seqNF (ParSx y) = ParS(seqNF x) (seqNF y)
seqNF (SeqSx y) = SeqS(seqNF x) (seqNF y)
seqNF x         = x  -}


