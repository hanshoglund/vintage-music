{-|
    Module      :  Music.Dynamics
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FlexibleInstances,
    RankNTypes #-}

module Music.Dynamics
where

import Prelude hiding ( reverse )

import Music.Time
import Music.Time.Functors
import Music.Time.Score


type Amplitude = Rational

class Dynamic t a where   
    composeDynamics  :: (t -> Amplitude) -> (Amplitude -> Amplitude -> Amplitude) -> a -> a
    amplitude        :: a -> t -> Amplitude

-- instance (Time t, Dynamic t a) => Dynamic t (t -> Amplitude, a) where
--     composeDynamics g op (f, x) = (\t -> f t `op` g t, x)
--     amplitude (f, x) t = f t


-- data DynamicT t d a = DynamicT (t -> Amplitude) (d a)
-- 
-- instance (Time t, Dynamic t a) => Dynamic t (DynamicT t d a) where
--     composeDynamics g op (DynamicT f x) = DynamicT (\t -> f t `op` g t) x
--     amplitude (DynamicT f x) t = f t
-- 
-- instance Temporal d => Temporal (DynamicT t d) where
--     instant = DynamicT (const 0)  instant
--     DynamicT _ x >>> DynamicT _ y = DynamicT (const 0) (x >>> y)
--     DynamicT _ x ||| DynamicT _ y = DynamicT (const 0) (x ||| y)
-- 
-- instance Loop d => Loop (DynamicT t d) where
--     loop (DynamicT _ x) = DynamicT (const 0) (loop x)
-- 
-- instance Reverse d => Reverse (DynamicT t d) where
--     reverse (DynamicT _ x) = DynamicT (const 0) (reverse x)
-- 
-- instance (Time t, Timed t d) => Timed t (DynamicT t d) where
--     duration (DynamicT _ x) = duration x
--     stretch d (DynamicT _ x) = DynamicT (const 0) (stretch d x)
-- 
-- instance (Time t, Delayed t d) => Delayed t (DynamicT t d) where
--     rest d = DynamicT (const 0) (rest d)
--     delay d (DynamicT _ x) = DynamicT (const 0) (delay d x)   

                                        
restrict :: Time t => t -> t -> (t -> a) -> t -> a
restrict t d f t' = f ((t' + t) * d)
    
renderDynamics :: (Time t, Dynamic t a) => (t -> Amplitude) -> Score t a -> Score t a
renderDynamics f = tdmap $ \t d x -> composeDynamics (restrict t d f) (*) x