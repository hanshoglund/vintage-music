{-|
    Module      :  Music.Pitch
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FunctionalDependencies,
    GeneralizedNewtypeDeriving #-}

module Music.Pitch 
where

import Data.Convert
import Music.Time

-- | Frequency in Hertz    
type Frequency = Double

-- | Logarithmic pitch reprentation.
--
-- > f * 2 = convert f + Octave 1    
newtype Octave = Octave Frequency
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

-- | Logarithmic pitch reprentation.    
--
-- > f * 2 = convert f + Cent 1200    
newtype Cent   = Cent   Frequency
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

instance Convert Frequency Octave  where  convert f          = Octave (logBase 2 f)
instance Convert Octave Frequency  where  convert (Octave f) = 2 ** f
instance Convert Cent Octave       where  convert (Cent f)   = Octave (f / 1200)
instance Convert Octave Cent       where  convert (Octave f) = Cent   (f * 1200)
instance Convert Frequency Cent    where  convert f          = Cent   (logBase 2 f * 1200)
instance Convert Cent Frequency    where  convert (Cent f)   = 2 ** (f / 1200)


unitFrequency :: Frequency
unitFrequency = 1

unitOctave :: Octave
unitOctave = Octave 0

unitCent :: Cent
unitCent = Cent 0

class Pitched t p where
    composePitch :: (t -> Frequency -> Frequency) -> p -> p
    frequency :: p -> t -> Frequency

