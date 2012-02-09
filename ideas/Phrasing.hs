{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Phrasing where

import Prelude hiding (reverse)
import qualified Data.List as List
import Data.Ord (comparing)

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Foldable

infixr 9 >>>
infixr 8 |||

type Time = Double

class Position t where
    position :: t a -> Time

class Duration d where
    duration :: d a -> Time

class PositionFunctor f where
    mapPosition :: (Time -> Time) -> f a -> f a

class DurationFunctor f where
    mapDuration :: (Time -> Time) -> f a -> f a


class Duration t => Temporal t where
    rest    :: Time -> t a
    note    :: Time -> a -> t a
    
    instant :: t a
    delay   :: Time -> t a -> t a
    stretch :: Time -> t a -> t a
    (|||)   :: t a -> t a -> t a
    (>>>)   :: t a -> t a -> t a
    
    instant   = rest 0
    delay t x = rest t >>> x

class Temporal t => LoopTemporal t where
    loop    :: t a -> t a
    loop x  = x >>> loop x

class Temporal t => ReverseTemporal t where
    reverse :: t a -> t a

class Temporal t => SplitTemporal t where    
    split   :: Time -> t a -> (t a, t a, t a)
    before  :: Time -> t a -> t a
    at      :: Time -> t a -> t a
    after   :: Time -> t a -> t a
    split  t x = (before t x, at t x, after t x)
    before t x = a where (a, b, c) = split t x
    at     t x = b where (a, b, c) = split t x
    after  t x = c where (a, b, c) = split t x

--
--
-- -- Temporal as Monoid and Monad
--
-- data TM t a = TM { runTM :: t a }
--
-- instance Temporal s => Monoid (TM t a) where
--     mempty        = TM $ instant
--     x `mappend` y = TM $ runTM x ||| runTM y
--
-- instance Temporal s => Monad (TM s) where
--     return      = TM . note
--     TM x >>= f  = foldMap f x

data Score a
    = Rest Time
    | Note Time a
    | Par  (Score a) (Score a)
    | Seq  (Score a) (Score a)
    deriving (Eq, Show, Functor, Foldable)


parallelNormalForm   = parNF
sequentialNormalForm = seqNF

-- | TODO adjust durations
parNF (Seq (Par a b) (Par c d))             
    | duration a  <   duration b    = undefined -- prolong a to be same length as b
    | duration a  ==  duration b    = Par (Seq (parNF a) (parNF c)) (Seq (parNF b) (parNF d))
    | duration a  >   duration b    = undefined -- prolong b to be same length as a

parNF (Seq x y) = Seq (parNF x) (parNF y)
parNF (Par x y) = Par (parNF x) (parNF y)
parNF x         = x


-- | TODO adjust durations
-- This is actually not possible with non-truncating composition!
seqNF (Par (Seq a b) (Seq c d))
    | duration a  <   duration c    = undefined
    | duration a  ==  duration c    = (Seq (Par (seqNF a) (seqNF c)) (Par (seqNF b) (seqNF d)))
    | duration a  >   duration c    = undefined

seqNF (Par x y) = Par (seqNF x) (seqNF y)
seqNF (Seq x y) = Seq (seqNF x) (seqNF y)
seqNF x         = x


instance Temporal Score where
    rest      = Rest
    note      = Note
    x ||| y   = Par x y
    x >>> y   = Seq x y
    stretch t = mapDuration (* t)

instance LoopTemporal Score where
    loop x    = Seq x (loop x)

instance ReverseTemporal Score where
    reverse (Seq x y) = Seq (reverse y) (reverse x)
    reverse (Par x y) = Par (reverse x) (reverse y)
    reverse x         = x

instance Monoid (Score a) where
    mempty  = instant
    mappend = (|||)
                    
instance Monad (Score) where
    return  = note 1
    s >>= f = (joinScore . fmap f) s

instance Applicative (Score) where
    pure  = return
    (<*>) = ap
    
instance MonadPlus (Score) where
    mzero = mempty
    mplus = mappend

instance Alternative (Score) where
    empty = mempty
    (<|>) = mappend

    

joinScore :: Score (Score a) -> Score a
joinScore = mconcat . fmap arrange . events . render
    where arrange (Event t d x) = (delay t . stretch d) x

instance (Eq a, Show a) => Num (Score a) where
    x + y       = Rest $ duration x + duration y
    x * y       = Rest $ duration x + duration y
    x - y       = Rest $ duration x + duration y
    abs         = Rest . abs . duration
    signum      = Rest . signum . duration
    fromInteger = Rest . fromInteger

instance (Eq a, Show a) => Fractional (Score a) where
    x / y        = Rest $ duration x + duration y
    fromRational = Rest . fromRational


instance Duration Score where
    duration (Rest d)        = d
    duration (Note d x)      = d
    duration (Par  x y)      = duration x `max` duration y
    duration (Seq  x y)      = duration x + duration y

instance DurationFunctor Score where
    mapDuration f (Rest d)   = Rest (f d)
    mapDuration f (Note d x) = Note (f d) x
    mapDuration f (Par  x y) = Par (mapDuration f x) (mapDuration f y)
    mapDuration f (Seq  x y) = Seq (mapDuration f x) (mapDuration f y)



------------------------------------------------------------------------------------------

data Event a
    = Event {
        eventPosition :: Time,
        eventDuration :: Time,
        eventContent  :: a
    }
    deriving (Eq, Show, Functor, Foldable)

instance Position Event
    where position = eventPosition

instance Duration Event
    where duration = eventDuration

instance PositionFunctor Event where
    mapPosition f (Event t d x) = Event (f t) d x

instance DurationFunctor Event where
    mapDuration f (Event t d x) = Event t (f d) x


------------------------------------------------------------------------------------------

data EventList a
    = EventList { 
        eventListDuration :: Time,
        eventListContent :: [Event a]
    }
    deriving (Eq, Show, Functor, Foldable)

instance Monoid (EventList a) where
    mempty        = EventList 0 []
    mappend xs ys = EventList (duration xs `max` duration ys) (events xs ++ events ys)

instance Duration EventList where
    duration = eventListDuration

instance DurationFunctor EventList where
    mapDuration f (EventList d xs) = EventList (f d) (map (mapDuration f) xs)

events :: EventList a -> [Event a]
events = eventListContent

render :: Score a -> EventList a
render = (\(EventList d xs) -> EventList d (List.sortBy (comparing eventPosition) xs)) . render' 0

render' t (Rest d)   = EventList d []
render' t (Note d x) = EventList d [Event t d x]
render' t (Par  x y) = render' t x `mappend` render' t y
render' t (Seq  x y) = EventList (duration xs + duration ys) (events xs ++ events ys)
    where xs = render' t x
          ys = render' (t + duration xs) y

line = Note 1 60 >>> Note 1 62 >>> Note 1 65 >>> Note 1 55 >>> Note 1 59
epos =  map eventPosition . events . render
edur =  map eventDuration . events . render
econ =  map eventContent . events . render





-- instance SplitTemporal Score where
--     before t (Seq x y) | 0 < t     = Seq x (before (t - duration x) y)
--                        | otherwise = before t x
--     before t x         | 0 < t     = x
--                        | otherwise = instant    
--     at t (Seq x y)     | 0 == t    = Seq x (at (t - duration x) y)
--                        | otherwise = at (t - duration x) y
--     at t x             | 0 == t    = x
--                        | otherwise = instant
--     after t (Seq x y)  | 0 > t     = Seq x (after (t - duration x) y)
--                        | otherwise = after t y
--     after t x          | 0 > t     = x
--                        | otherwise = instant
-- 


--instance Functor (TemporalFunctor a)
--
--
-- class Temporal s => Dynamic s where
--     dynamic :: v -> t a -> s v a
--
-- class Temporal s => Phrased s where
--     attack   :: b -> t a -> s (b,c,d) a
--     phrasing :: c -> t a -> s (b,c,d) a
--     release  :: d -> t a -> s (b,c,d) a
--
-- class Temporal s => Instrumentation s where
--     instrument :: i -> t a -> s i a
--