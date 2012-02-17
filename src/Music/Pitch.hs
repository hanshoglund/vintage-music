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
    

type Frequency = Rational    

class Pitched p where
    toFrequency :: p -> Rational