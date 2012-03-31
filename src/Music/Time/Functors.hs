{-|
    Module      :  Music.Time.Functors
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FunctionalDependencies #-}

module Music.Time.Functors
(
    TimeFunctor(..),
    tmapRight,
    dmapRight,
    tdmapRight,
    tmapEither,
    dmapEither,
    tdmapEither,
)
where

import Music.Time
import Music.Util.Either

-- | Refines the `Functor` class to support mapping over offset and duration.
--   Minimal complete definition: `tdmap`. 
--
class (Time t, Functor d) => TimeFunctor t d | d -> t where

    -- | Map with offset.
    tmap  :: (t -> a -> b) -> d a -> d b
    tmap f = tdmap (\t d -> f t)

    -- | Map with duration.
    dmap  :: (t -> a -> b) -> d a -> d b
    dmap f = tdmap (\t d -> f d)

    -- | Map with offset and duration.
    tdmap :: (t -> t -> a -> b) -> d a -> d b
    
tmapRight :: (Time t, TimeFunctor t f) => (t -> a -> b) -> f (Either c a) -> f (Either c b)
tmapRight f = tmap (\t -> mapRight $ f t)

dmapRight :: (Time t, TimeFunctor t f) => (t -> a -> b) -> f (Either c a) -> f (Either c b)
dmapRight f = dmap (\t -> mapRight $ f t)

tdmapRight :: (Time t, TimeFunctor t f) => (t -> t -> a -> b) -> f (Either c a) -> f (Either c b)
tdmapRight f = tdmap (\t d -> mapRight $ f t d)


tmapEither :: (Time t, TimeFunctor t f) => (t -> a -> b) -> (t -> c -> d) -> f (Either a c) -> f (Either b d)
tmapEither f g = tmap (\t -> l t . r t)
    where l t = mapLeft $ f t
          r t = mapRight $ g t

dmapEither :: (Time t, TimeFunctor t f) => (t -> a -> b) -> (t -> c -> d) -> f (Either a c) -> f (Either b d)
dmapEither f g = dmap (\d -> l d . r d)
    where l d = mapLeft $ f d
          r d = mapRight $ g d

tdmapEither :: (Time t, TimeFunctor t f) => (t -> t -> a -> b) -> (t -> t -> c -> d) -> f (Either a c) -> f (Either b d)
tdmapEither f g = tdmap (\t d -> l t d . r t d)
    where l t d = mapLeft $ f t d
          r t d = mapRight $ g t d




