{-|
    Module      :  Music.Time.Segment
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
    DeriveFoldable,
    RankNTypes #-}

module Music.Time.Segment

where

import Data.Monoid    
import Music.Time


data Segment t a
    = Segment 
    { 
        segmentDuration :: t,
        segmentValue    :: Monoid a => t -> a 
    }
    

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

instance Time t => Timed t (Segment t) where
    duration       (Segment d xs) = d
    withDuration f (Segment d xs) = Segment (f d) xs

instance Time t => Delayed t (Segment t) where
    rest d = Segment d (\x -> mempty)
    
instance Time t => Loop (Segment t)

instance Time t => Reverse (Segment t) where
    reverse (Segment d f) = Segment d (\x -> f $ negate x + d)

segment :: (Monoid a, Time t) => (t -> a) -> Segment t a
segment f = Segment 1 f

atTime :: (Monoid a, Time t) => Segment t a -> t -> a
atTime (Segment d f) t = if (t > d) then mempty else f t


test = segment (\x -> Sum 1) :: Segment Double (Sum Int)



