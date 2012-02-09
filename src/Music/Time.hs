{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, DeriveFunctor, DeriveFoldable #-}

{-|
    Module      :  Music.Time
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Music.Time 
(
-- * Temporal values
    Temporal(..),

-- * Specialized temporal values
    LoopTemporal(..),
    ReverseTemporal(..),
    SplitTemporal(..),

-- * Offset and duration
    Time(..),
    Timed(duration, stretch),
    Delayed(..),

-- * Score
    Score,
    note,  
    render,
    events,
    
-- * Events and event lists
    Event(..),
    EventList(..),
)
where

import Prelude hiding ( reverse )
import Data.Ord ( comparing )
import qualified Data.List as List

import Control.Monad
import Control.Applicative

import Data.Monoid
import Data.Ratio
import Data.Foldable

infixr 9 <<<
infixr 9 >>>
infixr 8 |||


{-| 
    Value with temporal semantics. Minimal complete definition: `instant`, `>>>` and `|||`.

    * `instant` and `|||` should form a monoid, as should `instant` and `>>>`:
    
    > instant >>> x         = x
    > x       >>> instant   = x
    >
    > instant ||| x         = x
    > x       ||| instant   = x
    >
    > x >>> (y >>> z) = (x >>> y) >>> z
    > x ||| (y ||| z) = (x ||| y) ||| z

    * Instances for `Timed`, should satisfy the following laws:

    > duration (x >>> y) = duration x + duration y
    > duration (x <<< y) = duration x + duration y
    > duration (x ||| y) = duration x `max` duration y
-}
class Temporal e where 
    -- | The instantanous temporal value. 
    instant :: e a

    -- | Parallel composition of temporal values. 
    (|||)   :: e a -> e a -> e a

    -- | Sequential composition of temporal values. 
    (>>>)   :: e a -> e a -> e a

    -- | Reverse sequential composition of temporal values. 
    (<<<)   :: e a -> e a -> e a
    a <<< b = a >>> b


-- | Time values must be ordered and support numeric operations.
class (Ord t, Num t) => Time t where
instance Time Double


-- | Values with a duration.
class Time t => Timed t d where
    duration        :: d a -> t
    withDuration    :: (t -> t) -> d a -> d a

    stretch         :: t -> d a -> d a
    stretch t       = withDuration (* t)

-- | Values with an offset.
class (Time t, Temporal d) => Delayed t d where
    rest    :: t -> d a

    delay   :: t -> d a -> d a
    delay t x = rest t >>> x


-- | Values that can be looped.
class (Time t, Temporal d) => LoopTemporal t d | d -> t where

    -- | Loop the given value for ever.
    loop    :: d a -> d a
    loop x  = x >>> loop x


-- | Values that can be reversed.
class (Time t, Temporal d) => ReverseTemporal t d | d -> t   where

    -- | Reverse the given value.
    reverse :: d a -> d a


-- | Values that can be cut.
--   Minimal complete definition: either `split` or all the others.
class (Time t, Temporal d) => SplitTemporal t d | d -> t   where    
    split :: t -> d a -> (d a, d a, d a)
    split t x = (before t x, at t x, after t x)
 
    before  :: t -> d a -> d a
    at      :: t -> d a -> d a
    after   :: t -> d a -> d a
    before t x = a where (a, b, c) = split t x
    at     t x = b where (a, b, c) = split t x
    after  t x = c where (a, b, c) = split t x     
 


data Score t a
    = Rest t
    | Note t a
    | Par  (Score t a) (Score t a)
    | Seq  (Score t a) (Score t a)
    deriving (Eq, Show, Functor, Foldable)


-- parallelNormalForm   = parNF
-- sequentialNormalForm = seqNF
-- 
-- -- TODO adjust durs
-- parNF (Seq (Par a b) (Par c d))             
--     | duration a  <   duration b    = undefined -- prolong a to be same length as b
--     | duration a  ==  duration b    = Par (Seq (parNF a) (parNF c)) (Seq (parNF b) (parNF d))
--     | duration a  >   duration b    = undefined -- prolong b to be same length as a
-- 
-- parNF (Seq x y) = Seq (parNF x) (parNF y)
-- parNF (Par x y) = Par (parNF x) (parNF y)
-- parNF x         = x
-- 
-- 
-- -- TODO adjust durations
-- -- This is actually not possible with non-truncating composition!
-- seqNF (Par (Seq a b) (Seq c d))
--     | duration a  <   duration c    = undefined
--     | duration a  ==  duration c    = (Seq (Par (seqNF a) (seqNF c)) (Par (seqNF b) (seqNF d)))
--     | duration a  >   duration c    = undefined
-- 
-- seqNF (Par x y) = Par (seqNF x) (seqNF y)
-- seqNF (Seq x y) = Seq (seqNF x) (seqNF y)
-- seqNF x         = x


instance Time t => Temporal (Score t) where
    instant   = Rest 0
    x ||| y   = Par x y
    x >>> y   = Seq x y

instance Time t => Timed t (Score t) where
    duration (Rest d)        = d
    duration (Note d x)      = d
    duration (Seq  x y)      = duration x + duration y
    duration (Par  x y)      = duration x `max` duration y

    withDuration f (Rest d)   = Rest (f d)
    withDuration f (Note d x) = Note (f d) x
    withDuration f (Par  x y) = Par (withDuration f x) (withDuration f y)
    withDuration f (Seq  x y) = Seq (withDuration f x) (withDuration f y)

instance Time t => Delayed t (Score t) where
    rest      = Rest
    
instance Time t => LoopTemporal t (Score t)

instance Time t => ReverseTemporal t (Score t) where
    reverse (Seq x y) = Seq (reverse y) (reverse x)
    reverse (Par x y) = Par (reverse x) (reverse y)
    reverse x         = x

instance Time t => Monoid (Score t a) where
    mempty  = Rest 0
    mappend = Seq
                    
instance Time t => Monad (Score t) where
    return  = Note 1
    s >>= f = (join' . fmap f) s

instance Time t => Applicative (Score t) where
    pure  = return
    (<*>) = ap
    
instance Time t => MonadPlus (Score t) where
    mzero = mempty
    mplus = mappend

instance Time t => Alternative (Score t) where
    empty = mempty
    (<|>) = mappend

note   :: Time t => a -> Score t a
note   = Note 1

events :: Time t => Score t a -> [Event t a]
events = eventListContent . render
    
join' :: Time t => Score t (Score t a) -> Score t a
join' = mconcat . fmap arrange . events
    where arrange (Event t d x) = (delay t . stretch d) x

-- instance (Time t, Eq a, Show a) => Num (Score t a) where
--     x + y       = Rest $ duration x + duration y
--     x * y       = Rest $ duration x + duration y
--     x - y       = Rest $ duration x + duration y
--     abs         = Rest . abs . duration
--     signum      = Rest . signum . duration
--     fromInteger = Rest . fromInteger
-- 
-- instance (Time t, Eq a, Show a) => Fractional (Score t a) where
--     x / y        = Rest $ duration x + duration y
--     fromRational = Rest . fromRational





------------------------------------------------------------------------------------------

data Event t a
    = Event {
        eventPosition :: t,
        eventDuration :: t,
        eventContent  :: a
    }
    deriving (Eq, Show, Functor, Foldable)

instance Time t => Timed t (Event t) where
    duration = eventDuration
    withDuration f (Event t d x) = Event t (f d) x


------------------------------------------------------------------------------------------

data EventList t a
    = EventList { 
        eventListDuration :: t,
        eventListContent :: [Event t a]
    }
    deriving (Eq, Show, Functor, Foldable)

instance Time t => Monoid (EventList t a) where
    mempty        = EventList 0 []
    mappend xs ys = EventList (duration xs `max` duration ys) (eventListContent xs ++ eventListContent ys)

instance Time t => Timed t (EventList t) where
    duration = eventListDuration
    withDuration f (EventList d xs) = EventList (f d) (map (withDuration f) xs) -- FIXME only works for stretching

render :: Time t => Score t a -> EventList t a
render = (\(EventList d xs) -> EventList d (List.sortBy (comparing eventPosition) xs)) . render' 0

render' t (Rest d)   = EventList d []
render' t (Note d x) = EventList d [Event t d x]
render' t (Par  x y) = render' t x `mappend` render' t y
render' t (Seq  x y) = EventList (duration xs + duration ys) (eventListContent xs ++ eventListContent ys)
    where xs = render' t x
          ys = render' (t + duration xs) y

-- line = Note 1 60 >>> Note 1 62 >>> Note 1 65 >>> Note 1 55 >>> Note 1 59
-- epos =  map eventPosition . events . render
-- edur =  map eventDuration . events . render
-- econ =  map eventContent . events . render
-- 
-- 

                                                             