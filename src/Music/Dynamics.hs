{-|
    Module      :  Music.Dynamics
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FlexibleInstances #-}

module Music.Dynamics
where

import Prelude hiding ( reverse )
import Music.Time
import Music.Time.Score
import Music.Time.Segment


type Amplitude = Rational

class TemporalDynamic t d where
    level :: d a -> Amplitude
    cresc :: d a -> d a
    dim   :: d a -> d a
    scale :: Amplitude -> d a -> d a

class TemporalTrans t where
    liftTemporal :: Temporal d => d a -> t d a
        
type Dynamic t a = DynamicT t (Score t) a
newtype DynamicT t d a = DynamicT { runDynamic :: d (a, t -> Amplitude) }

-- instance TemporalTrans (DynamicT t) where
--     liftTemporal x = DynamicT (x, const 0)
                                         

{-|
    Render dynamic levels.
-}
renderDynamic :: Time t => Dynamic t a -> Score t (a, Amplitude)
renderDynamic = undefined

{-|
    Render dynamic levels.
-}
renderDynamicT :: (Time t, Temporal d) => DynamicT t d a -> d (a, Amplitude)
renderDynamicT = undefined

--
-- Temporal instances
--

-- instance (Time t, Monad d) => Monad (DynamicT t d) where
--     return x          =  DynamicT (return (x, const 0))
--     DynamicT x >>= f  =  DynamicT (x >>= runDynamic . f)
-- 
-- 
-- instance Temporal d => Temporal (DynamicT t d) where
--     instant                    =   DynamicT instant
--     DynamicT x >>> DynamicT y  =   DynamicT (x >>> y)
--     DynamicT x ||| DynamicT y  =   DynamicT (x ||| y)
-- 
-- instance Loop d => Loop (DynamicT t d) where
--     loop (DynamicT x)  =  DynamicT (loop x)
-- 
-- instance Reverse d => Reverse (DynamicT t d) where
--     reverse (DynamicT x)  =  DynamicT (reverse x)
-- 
-- instance (Time t, Timed t d) => Timed t (DynamicT t d) where
--     duration (DynamicT x)   =  duration x
--     stretch d (DynamicT x)  =  DynamicT (stretch d x)
-- 
-- instance (Time t, Delayed t d) => Delayed t (DynamicT t d) where
--     rest d                =  DynamicT (rest d)
--     delay d (DynamicT x)  =  DynamicT (delay d x)
-- 
-- instance (Time t, Split t d) => Split t (DynamicT t d) where
--     before t (DynamicT x)  =  DynamicT (before t x)
--     after  t (DynamicT x)  =  DynamicT (after  t x)
-- 
--                                                              

