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
    DeriveFoldable,
    RankNTypes #-}

module Music.Time.Segment
(
    Segment,
    segment,
    at
)
where

import Control.Applicative    
import Data.Monoid    

import Music.Time


data Segment t a
    = Segment 
    { 
        segmentDuration :: t,
        segmentValue    :: Monoid a => t -> a
    }
    deriving
    (
    -- Eq,
    -- Show
    )
    

instance Time t => Temporal (Segment t) where
    instant = Segment 0 (\x -> mempty)

    Segment d f ||| Segment d' g 
        = Segment 
            (d `max` d') 
            (\x -> f x `mappend` g x)

    Segment d f >>> Segment d' g 
        = Segment 
            (d + d') 
            (\x -> if x < d then f x else f (x - d))

instance Time t => Loop (Segment t) where
    loop x = x >>> loop x

instance Time t => Reverse (Segment t) where
    reverse (Segment d f) = Segment d (\x -> f $ negate x + d)

instance Time t => Timed t (Segment t) where
    duration  (Segment d f) = d
    stretch t (Segment d f) = Segment (d * t) (\x -> f $ x / t)

instance Time t => Delayed t (Segment t) where
    rest d   = Segment d (\x -> mempty)
    delay t x = rest t >>> x   


-- TODO a signature like this would be necessary to implement functor, applicative etc.
-- Could be nice to use with Monoid wrappers such as Sum, All etc (?)
 
-- (Monoid m, Time t) => (t -> a) -> (a -> m) -> (m -> a) -> Segment t a

segment :: (Monoid a, Time t) => (t -> a) -> Segment t a
segment f = Segment 1 f

at :: (Monoid a, Time t) => Segment t a -> t -> a
at (Segment d f) t = if (t > d) then mempty else f t

test = segment (\x -> Sum 1) :: Segment Double (Sum Int)



