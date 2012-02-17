{-|
    Module      :  Music.Space
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses, 
    FunctionalDependencies #-}

module Music.Space
where

type Location = (Rational, Rational, Rational)

type Frequency = Rational    

class Spacial p where
    toLocation :: p -> Location