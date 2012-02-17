{-|
    Module      :  Music.Time.Score
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FunctionalDependencies, 
    FlexibleInstances, 
    DeriveFunctor, 
    DeriveFoldable #-}

module Music.Time.Score
(
-- * Score
    Score,
    note,  
    render,
)
where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Foldable

import Prelude hiding ( reverse )
import Data.Ord ( comparing )
import qualified Data.List as List

import Music.Time
import Music.Time.EventList
import qualified Music.Time.EventList as E


-- | The Score type.
data Score t a
    = Rest t
    | Note t a
    | Par  (Score t a) (Score t a)
    | Seq  (Score t a) (Score t a)
    deriving 
    (
    -- Eq, 
    -- Show, 
    Functor 
    -- Foldable
    )


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

    stretch t (Rest d)   = Rest (d * t)
    stretch t (Note d x) = Note (d * t) x
    stretch t (Par  x y) = Par  (stretch t x) (stretch t y)
    stretch t (Seq  x y) = Seq  (stretch t x) (stretch t y)

instance Time t => Delayed t (Score t) where
    rest = Rest
    delay t x = rest t >>> x
    -- offset (Rest d)   = d
    -- offset (Note d x) = 0
    -- offset (Seq  x y) = offset x + offset y
    -- offset (Par  x y) = offset x `min` offset y
    
instance Time t => Loop (Score t)

instance Time t => Reverse (Score t) where
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

-- instance Time t => MonadPlus (Score t) where
--     mzero = mempty
--     mplus = mappend
-- 
-- instance Time t => Alternative (Score t) where
--     empty = mempty
--     (<|>) = mappend

note   :: Time t => a -> Score t a
note   = Note 1

events :: Time t => Score t a -> [Event t a]
events = E.val . render
    
join' :: Time t => Score t (Score t a) -> Score t a
join' = mconcat . fmap arrange . events
    where arrange (Event t d x) = (delay t . stretch d) x

render :: Time t => Score t a -> EventList t a
render = (\(EventList d xs) -> EventList d (List.sortBy (comparing E.posE) xs)) . render' 0

render' t (Rest d)   = EventList d []
render' t (Note d x) = EventList d [Event t d x]
render' t (Par  x y) = render' t x `mappend` render' t y
render' t (Seq  x y) = EventList (duration xs + duration ys) (E.val xs ++ E.val ys)
    where xs = render' t x
          ys = render' (t + duration xs) y


-- instance (Time t, Eq a, Show a) => Num (Score t a) where
--     x + y       = Rest $ duration x + duration y
--     x * y       = Rest $ duration x + duration y
--     x - y       = Rest $ duration x + duration y
--     abs         = Rest . abs . duration
--     signum      = Rest . signum . duration
--     fromInteger = Rest . fromInteger




