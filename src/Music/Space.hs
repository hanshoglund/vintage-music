{-|
    Module      :  Music.Space
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FlexibleInstances #-}

module Music.Space
where

import Music.Time


-- | Location in an Euclidean space, measured in metres.
type Location = (Double, Double, Double)


class Spacial t a where
    -- | @location x t@ returns the location of the spacial value @x@ at the time @t@.
    location :: Time t => a -> t -> Location        
    

instance (Time t, Temporal d, Spacial t p) => Spacial t (d p) where
    location = undefined
