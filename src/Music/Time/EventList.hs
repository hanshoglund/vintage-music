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
    normalize,
    printEvents
)
where

import Data.Monoid
import Data.Foldable
import Data.Ord ( comparing )
import qualified Data.List as List

import Music.Time
import Music.Time.Event ( Event(..) )

data EventList t a
    = EventList
    {
        eventListDuration :: t,
        eventListEvents   :: [Event t a]
    }
    deriving (Eq, Show, Functor, Foldable)

instance Time t => Temporal (EventList t) where
    instant = EventList 0 []

    EventList dx ex  |||  EventList dy ey  =  
        EventList (dx `max` dy) (ex ++ ey)

    EventList dx ex  >>>  EventList dy ey  =  
        EventList (dx + dy) (ex ++ map (delay dx) ey)

instance Time t => Timed t (EventList t) where
    duration (EventList d xs) = d
    stretch t (EventList d xs) = EventList (d * t) (map (stretch t) xs)

instance Time t => Delayed t (EventList t) where
    rest d = EventList d []
    delay v (EventList d xs) = EventList (d + v) (map (delay v) xs)


-- | Returns a normal form event list, with events sorted by position. 
normalize :: Time t => EventList t a -> EventList t a
normalize (EventList d xs) = EventList d (List.sortBy (comparing eventOffset) xs)

printEvents :: (Time t, Show t, Show a) => EventList t a -> String
printEvents (EventList d xs) = 
    "Total duration: " ++ show d ++ "\n" ++
        List.concatMap ( \(Event t d x) -> "  " ++ show t ++ "  " ++ show d ++ "  " ++ show x ++ "\n" ) xs

