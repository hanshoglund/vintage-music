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

module Music.Time.EventList
(
    Event(..),
    EventList(..),
)
where

import Data.Monoid
import Data.Foldable

import Music.Time

data Event t a
    = Event 
    {
        posE :: t,
        durE :: t,
        valE :: a
    }
    deriving 
    (
    Eq, 
    Show, 
    Functor
    -- Foldable
    )

instance Time t => Timed t (Event t) where
    duration = durE
    stretch a (Event t d x) = Event t (d * a) x


data EventList t a
    = EventList 
    { 
        dur :: t,
        val :: [Event t a]
    }
    deriving
    (
    Eq, 
    Show, 
    Functor
    -- Foldable
    )

instance Time t => Monoid (EventList t a) where
    mempty        = EventList 0 []
    mappend xs ys = EventList (duration xs `max` duration ys) (val xs ++ val ys)

instance Time t => Timed t (EventList t) where
    duration = dur
    stretch t (EventList d xs) = EventList (d * t) (map (stretch t) xs)

