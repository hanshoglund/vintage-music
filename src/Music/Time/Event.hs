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
        pos   :: t,
        dur   :: t,
        value :: a
    }
    deriving 
    (
    Eq, 
    Show, 
    Functor,
    Foldable
    )

instance Time t => Timed t (Event t) where
    duration = dur
    stretch a (Event t d x) = Event (t * a) (d * a) x

instance Time t => Delayed t (Event t) where
    rest = undefined -- FIXME
    delay v (Event t d x) = Event (t + v) d x