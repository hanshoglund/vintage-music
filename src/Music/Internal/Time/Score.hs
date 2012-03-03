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
import Data.Convert

import Music.Time
import Music.Time.Functors
import Music.Time.Event ( Event(..) )
import Music.Time.EventList ( EventList(..) )
import qualified Music.Time.EventList as EventList


-- | A discrete temporal structure, generalising standard music notation.
--
--   Values are lifted to score level using the unit constructor function 'note'.
--   There are several utility construction functions, implemented in terms of 'note', notably
--   'line' and 'chord'. Durations are specified by using 'delay' and 'stretch'.
--
--   >     melody [1, 2, 3]
--   >     chord  [0, 3, 5]
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
    reverse (ParS x y)  =  ParS (reverse x') (reverse y') 
                               where (x', y') = x `assureEqualDur` y
    
    reverse (SeqS x y)  =  SeqS (reverse y) (reverse x)
    
    reverse x           =  x


instance Time t => Timed t (Score t) where
    duration (RestS d)    =  d
    duration (NoteS d x)  =  d
    duration (SeqS x y)   =  duration x + duration y
    duration (ParS x y)   =  duration x `max` duration y

    stretch t (RestS d)   | t >= 0     =  RestS (d * t)
                          | otherwise  =  negativeError "Music.Time.Timed.stretch"

    stretch t (NoteS d x) | t >= 0     =  NoteS (d * t) x
                          | otherwise  =  negativeError "Music.Time.Timed.stretch"

    stretch t (ParS x y)  | t >= 0     =  ParS (stretch t x) (stretch t y)
                          | otherwise  =  negativeError "Music.Time.Timed.stretch"

    stretch t (SeqS x y)  | t >= 0     =  SeqS (stretch t x) (stretch t y)
                          | otherwise  =  negativeError "Music.Time.Timed.stretch"


instance Time t => Delayed t (Score t) where
    rest t    | t >= 0     =  RestS t
              | otherwise  =  negativeError "Music.Time.Timed.rest"

    delay t x | t >= 0     =  rest t >>> x
              | otherwise  =  negativeError "Music.Time.Timed.delay"


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
    tdmap f = foldScore ( \t d   -> RestS d )
                        ( \t d x -> NoteS d $ f t d x )
                        ( \t x y -> ParS x y )
                        ( \t x y -> SeqS x y )

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
renderScore' t (ParS x y)   =
    let (dx, ex) = renderScore' t x
        (dy, ey) = renderScore' t y
                       in (dx `max` dy, ex ++ ey)
renderScore' t (SeqS x y)   =
    let (dx, ex) = renderScore' t x
        (dy, ey) = renderScore' (t + dx) y
                       in (dx + dy, ex ++ ey)


unrenderScore :: Time t => EventList t a -> Score t a
unrenderScore = chordStretchDelay . map (\(Event t d x) -> (t, d, x)). eventListEvents


--
-- Constrution
--

-- | Creates a score containing the given element.
note :: Time t => a -> Score t a
note = NoteS 1

-- | Creates a score containing the given elements, composed in sequence.
line :: Time t => [a] -> Score t a
line = concatSeq . map note

-- | Creates a score containing the given elements, composed in parallel.
chord :: Time t => [a] -> Score t a
chord = concatPar . map note

-- | Creates a from a group of parallel lines.
lines :: Time t => [[a]] -> Score t a
lines = concatPar . map line

-- | Creates a from a sequence of chords.
chords :: Time t => [[a]] -> Score t a
chords = concatSeq . map chord

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
foldScore' r n p s t (ParS x y)   =
    let (dx, sx) = foldScore' r n p s t x
        (dy, sy) = foldScore' r n p s t y
                                      in (dx `max` dy, p t sx sy)
foldScore' r n p s t (SeqS x y)   =
    let (dx, sx) = foldScore' r n p s t x
        (dy, sy) = foldScore' r n p s (t + dx) y
                                      in (dx + dy, s t sx sy)

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

foldValue :: (Time t, Monoid m) => (a -> m) -> Score t a -> m
foldValue f = foldScore ( \t d   -> mempty )
                        ( \t d x -> f x )
                        ( \t x y -> x `mappend` y )
                        ( \t x y -> x `mappend` y )

numberOfEvents :: Time t => Score t a -> Int
numberOfEvents = foldScore (\t d -> 0) (\t d x -> 1) (const (+)) (const (+))

meanDuration :: Time t => Score t a -> t
meanDuration score = (getSum . foldDuration Sum) score / fromIntegral (numberOfEvents score)

normalizeDuration :: Time t => Score t a -> Score t a
normalizeDuration score = stretch (1 / meanDuration score) score


--
-- Internals
--


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
toEvents = eventListEvents . renderScore

negativeError :: String -> a
negativeError name = error $ name ++ ": negative value"


