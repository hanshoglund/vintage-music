{-|
    Module      :  Data.Convert
    Copyright   :  Hans Höglund 2005

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses #-}

module Data.Convert
(
    Convert(..)
)
where
class Convert a b where
    convert :: a -> b

