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
    tmapR,
    dmapR,
    tdmapR,
    tmapE,
    dmapE,
    tdmapE,
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
    
tmapR :: (Time t, TimeFunctor t f) => (t -> a -> b) -> f (Either c a) -> f (Either c b)
tmapR f = tmap (\t -> mapR $ f t)

dmapR :: (Time t, TimeFunctor t f) => (t -> a -> b) -> f (Either c a) -> f (Either c b)
dmapR f = dmap (\t -> mapR $ f t)

tdmapR :: (Time t, TimeFunctor t f) => (t -> t -> a -> b) -> f (Either c a) -> f (Either c b)
tdmapR f = tdmap (\t d -> mapR $ f t d)


tmapE :: (Time t, TimeFunctor t f) => (t -> a -> b) -> (t -> c -> d) -> f (Either a c) -> f (Either b d)
tmapE f g = tmap (\t -> l t . r t)
    where l t = mapL $ f t
          r t = mapR $ g t

dmapE :: (Time t, TimeFunctor t f) => (t -> a -> b) -> (t -> c -> d) -> f (Either a c) -> f (Either b d)
dmapE f g = dmap (\d -> l d . r d)
    where l d = mapL $ f d
          r d = mapR $ g d

tdmapE :: (Time t, TimeFunctor t f) => (t -> t -> a -> b) -> (t -> t -> c -> d) -> f (Either a c) -> f (Either b d)
tdmapE f g = tdmap (\t d -> l t d . r t d)
    where l t d = mapL $ f t d
          r t d = mapR $ g t d




