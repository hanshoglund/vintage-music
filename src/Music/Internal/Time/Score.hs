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

module Music.Internal.Time.Score 
where

import Prelude hiding ( reverse )

import Control.Monad
import Control.Applicative

import Data.Convert
import Data.Monoid
import Data.Foldable

import Music.Time
import Music.Time.Functors
import Music.Time.Event ( Event(..) )
import Music.Time.EventList ( EventList(..), printEvents )
import qualified Music.Time.EventList as EventList


-- | A discrete temporal structure, generalising standard music notation.
--
--   Values are lifted to score level using the unit constructor function 'note'.
--   There are several utility construction functions, implemented in terms of 'note', notably
--   'line' and 'chord'. Durations are specified by using 'delay' and 'stretch'.
--
--   > line [1, 2, 3]   =  note 1 >>> note 2 >>> note 3
--   > chord [0, 3, 5]  =  note 0 ||| note 3 ||| note 5
--
--   Useful applicative functions:
--       'pure', '<*>', 'liftA', 'liftA2' etc.
--
--   Useful monadic functions:
--       'return', 'join', 'liftM', 'liftM2' etc.

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
    reverse (ParS x y)  =  ParS (reverse a) (reverse b) where (a, b) = x `assureEqualDur` y
    reverse (SeqS x y)  =  SeqS (reverse y) (reverse x)    
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
    rest t     =  RestS t
    delay t x  =  rest t >>> x


instance Time t => Split t (Score t) where
    before z = filterScore ( \t d   -> t < z )
                           ( \t d x -> t < z )
                           ( \t     -> t < z )
                           ( \t     -> t < z )

    after  z = filterScore ( \t d   -> t >= z )
                           ( \t d x -> t >= z )
                           ( const True )
                           ( const True )


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
    tdmap f = foldScore (const RestS)
                        ( \t d x -> NoteS d $ f t d x )
                        (const ParS)
                        (const SeqS)


--
-- Rendering
--

instance Time t => Render (Score t a) (EventList t a) where
    render = renderScore

instance Time t => Render (EventList t a) (Score t a) where
    render = unrenderScore
    
--  The basic implementation for render looks like this:
--  
--  render (RestS d)   =  EventList d []
--  render (NoteS d x) =  EventList d [Event 0 d x]
--  render (ParS x y)  =  render x ||| render y
--  render (SeqS x y)  =  render x >>> render y
--  
--  The actual implementation optimizes this by computing offsets before rendering, instead of after.
--  
--  TODO reimplement in terms of foldScore (?)

renderScore score = let (d, xs) = renderScore' 0 score
                   in EventList.normalize $ EventList d xs

-- offset -> score -> (dur, events)
renderScore' t (RestS d)    =  (d, [])
renderScore' t (NoteS d x)  =  (d, [Event t d x])
renderScore' t (ParS x y)   =  (dx `max` dy, ex ++ ey)
    where 
        (dx, ex) = renderScore' t x
        (dy, ey) = renderScore' t y

renderScore' t (SeqS x y)   =  (dx + dy, ex ++ ey)
    where
        (dx, ex) = renderScore' t x
        (dy, ey) = renderScore' (t + dx) y


unrenderScore :: Time t => EventList t a -> Score t a
unrenderScore = chordDelayStretch . map (\(Event t d x) -> (t, d, x)). eventListEvents



--
-- Constrution
--

-- | Creates a score containing the given element.
--   Equivalent to 'pure' and 'return'.
note :: Time t => a -> Score t a
note = NoteS 1

-- | Creates a score containing the given elements, composed in sequence.
line :: Time t => [a] -> Score t a
line = concatSeq . map note

-- | Creates a score containing the given elements, composed in parallel.
chord :: Time t => [a] -> Score t a
chord = concatPar . map note

-- | Creates a score from a the given lines, composed in parallel.
lines :: Time t => [[a]] -> Score t a
lines = concatPar . map line

-- | Creates a score from a the given chords, composed in sequence.
chords :: Time t => [[a]] -> Score t a
chords = concatSeq . map chord

-- | Like line, but stretching each note by the given factors.
lineStretch :: Time t => [(t, a)] -> Score t a
lineStretch = concatSeq . map ( \(d, x) -> stretch d $ note x )

-- | Like chord, but delays each note the given amounts.
chordDelay :: Time t => [(t, a)] -> Score t a
chordDelay = concatPar . map ( \(t, x) -> delay t $ note x )

-- | Like chord, but delays and stretches each note the given amounts.
chordDelayStretch :: Time t => [(t, t, a)] -> Score t a
chordDelayStretch = concatPar . map ( \(t, d, x) -> delay t . stretch d $ note x )

-- | Like chord, but delaying each note the given amount.
arpeggio :: Time t => t -> [a] -> Score t a
arpeggio t xs = chordDelay (zip [0, t ..] xs)


--
-- Folds
--

foldScore :: Time t =>
    (t -> t -> b) ->
    (t -> t -> a -> b) ->
    (t -> b -> b -> b) ->
    (t -> b -> b -> b) ->
    Score t a ->
    b
foldScore r n p s score =
    let (d, score') = foldScore' r n p s 0 score in score'

foldScore' :: Time t =>
    (t -> t -> b) ->
    (t -> t -> a -> b) ->
    (t -> b -> b -> b) ->
    (t -> b -> b -> b) ->
    t ->
    Score t a ->
    (t, b)

foldScore' r n p s t (RestS d)    =  (d, r t d)
foldScore' r n p s t (NoteS d x)  =  (d, n t d x)

foldScore' r n p s t (ParS x y)   =  (dx `max` dy, p t sx sy)
    where 
        (dx, sx) = foldScore' r n p s t x
        (dy, sy) = foldScore' r n p s t y

foldScore' r n p s t (SeqS x y)   =  (dx + dy, s t sx sy)
    where
        (dx, sx) = foldScore' r n p s t x
        (dy, sy) = foldScore' r n p s (t + dx) y



filterScore :: Time t =>
    (t -> t -> Bool) ->
    (t -> t -> a -> Bool) ->
    (t -> Bool) ->
    (t -> Bool) ->
    Score t a ->
    Score t a

filterScore r n p s = foldScore ( \t d   -> if (r t d)   then RestS d   else instant )
                                ( \t d x -> if (n t d x) then NoteS d x else instant )
                                ( \t x y -> if (p t)     then ParS x y  else instant )
                                ( \t x y -> if (s t)     then SeqS x y  else instant )

foldOffset :: (Time t, Monoid m) => (t -> m) -> Score t a -> m
foldOffset f = foldScore ( \t d   -> f t )
                         ( \t d x -> f t )
                         ( \t x y -> x `mappend` y )
                         ( \t x y -> x `mappend` y )

foldDuration :: (Time t, Monoid m) => (t -> m) -> Score t a -> m
foldDuration f = foldScore ( \t d   -> f d )
                           ( \t d x -> f d )
                           ( \t x y -> x `mappend` y )
                           ( \t x y -> x `mappend` y )

-- | First event in score.
firstEvent :: Time t => Score t a -> Maybe a
firstEvent = getFirst . 
    foldScore ( \t d   -> First $ Nothing )
              ( \t d x -> First $ Just x )
              ( \t x y -> x `mappend` y )
              ( \t x y -> x `mappend` y )

-- | Last event in score.
lastEvent :: Time t => Score t a -> Maybe a
lastEvent = getLast . 
    foldScore ( \t d   -> Last $ Nothing )
              ( \t d x -> Last $ Just x )
              ( \t x y -> x `mappend` y )
              ( \t x y -> x `mappend` y )

-- | The number of events in the score.
numberOfEvents :: Time t => Score t a -> Int
numberOfEvents = foldScore (\t d -> 0) (\t d x -> 1) (const (+)) (const (+))

-- | The mean duration of the score.
meanDuration :: Time t => Score t a -> t
meanDuration score = (getSum . foldDuration Sum) score / fromIntegral (numberOfEvents score)

-- | Prepend a rest of the given duration to the score.
restBefore :: Time t => t -> Score t a -> Score t a
restBefore = delay

-- | Append a rest of the given duration to the score.
restAfter :: Time t => t -> Score t a -> Score t a
restAfter t x
    | t <= 0     =  x
    | otherwise  =  x >>> r
    where r = rest t

-- | Prepend and append a rest of half the given duration to the score.
restBoth :: Time t => t -> Score t a -> Score t a
restBoth t x
    | t <= 0     =  x
    | otherwise  =  r >>> x >>> r
    where 
        r = rest (t / 2)

-- | Stretch the score to the given duration.
stretchTo :: Time t => t -> Score t a -> Score t a
stretchTo t x 
    | duration x == t  =  x
    | otherwise        =  stretch (t / duration x) x

-- | Resize the score so that the mean duration is one.
normalizeDuration :: Time t => Score t a -> Score t a
normalizeDuration score = stretch (1 / meanDuration score) score

--
-- Internals
--


assureEqualDur :: Time t => Score t a -> Score t a -> (Score t a, Score t a)
assureEqualDur x y 
    | dx <  dy  =  (assureDur dy x, y)
    | dx == dy  =  (x, y)
    | dx >  dy  =  (x, assureDur dx y)
    where 
        dx = duration x
        dy = duration y

assureDur :: Time t => t -> Score t a -> Score t a
assureDur t s | duration s < t  =  s >>> rest (t - duration s)
              | otherwise       =  s

toEvents :: Time t => Score t a -> [Event t a]
toEvents = eventListEvents . renderScore

negativeError :: String -> a
negativeError name = error $ name ++ ": negative value"

--
-- Debug
--       

-- | A convenient synonym for 'printEvents' @.@ 'render'.
printScoreEvents :: (Time t, Show t, Show a) => Score t a -> String
printScoreEvents = printEvents . renderScore

