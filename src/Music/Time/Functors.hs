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
)
where

import Music.Time

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
    
