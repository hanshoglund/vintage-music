{-|
    Module      :  Music.Pitch
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FunctionalDependencies #-}

module Music.Pitch where
import Music.Time
    
type Frequency = Rational

class Pitched t p where
    composePitch :: (t -> Frequency) -> (Frequency -> Frequency -> Frequency) -> p -> p
    frequency :: p -> t -> Frequency

