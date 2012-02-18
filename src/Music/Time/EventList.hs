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

module Music.Time.EventList
(
    EventList(..),
    normalize
)
where

import Data.Monoid
import Data.Foldable
import Data.Ord (comparing)
import qualified Data.List as List

import Music.Time
import Music.Time.Event hiding (dur)


data EventList t a
    = EventList
    {
        dur    :: t,
        events :: [Event t a]
    }
    deriving
    (
    Eq,
    Show,
    Functor
    -- Foldable
    )

instance Time t => Temporal (EventList t) where
    instant = EventList 0 []
    x ||| y = EventList (dur x `max` dur y) (events x ++ events y)
    x >>> y = EventList (dur x  +    dur y) (events x ++ map (delay $ dur x) (events y) )

instance Time t => Timed t (EventList t) where
    duration = dur
    stretch t (EventList d xs) = EventList (d * t) (map (stretch t) xs)

instance Time t => Delayed t (EventList t) where
    rest d = EventList d []
    delay v (EventList d xs) = EventList (d + v) (map (delay v) xs)

-- instance Time t => Monoid (EventList t a) where
--     mempty  = instant
--     mappend = (>>>)

normalize :: Time t => EventList t a -> EventList t a
normalize (EventList d xs) = EventList d (List.sortBy (comparing pos) xs)

