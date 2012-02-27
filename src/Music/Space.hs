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

import Music.Time


type Location = (Rational, Rational, Rational)

class Spacial a where
    location :: Time t => a -> t -> Location