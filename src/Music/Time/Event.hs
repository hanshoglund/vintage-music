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

module Music.Time.Event
(
    Event(..),
)
where

import Data.Monoid
import Data.Foldable

import Music.Time

data Event t a
    = Event 
    {
        eventOffset   :: t,
        eventDuration :: t,
        eventValue    :: a
    }
    deriving (Eq, Show, Functor, Foldable)

instance Time t => Timed t (Event t) where
    duration (Event t d x) = d
    stretch a (Event t d x) = Event (t * a) (d * a) x

instance Time t => Delayed t (Event t) where
    rest = undefined -- FIXME
    delay v (Event t d x) = Event (t + v) d x