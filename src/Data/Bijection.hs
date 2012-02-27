{-|
    Module      :  Data.Inverse
    Copyright   :  Hans Höglund 2005

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Data.Bijection
-- (        
-- Permutation(..),
-- inverse
-- )
where

import Control.Applicative
import Data.Monoid       
import Prelude hiding (id, (.))
import Control.Category (Category, id, (.))

class Bijective bi where
    inverse :: bi a b -> bi b a
    
-- |
--   Bijective function, represented as a function with its inverse.
--
newtype Bijection a b = Bijection (a -> b, b -> a)

instance Bijective Bijection where
    inverse (Bijection (f, f')) = Bijection (f', f)

instance Category Bijection where
    id = idB
    (.) = composeB


idB :: Bijection a a
idB = Bijection (id, id)

composeB :: Bijection b c -> Bijection a b -> Bijection a c
composeB (Bijection (g, g')) (Bijection (f, f')) = Bijection (g . f, f' . g')

applyB :: Bijection a b -> a -> b
applyB (Bijection (f, f')) = f

succB :: Enum a => Bijection a a
predB :: Enum a => Bijection a a
succB = Bijection (succ, pred)
predB = inverse succB            

addB :: Num a => a -> Bijection a a
subB :: Num a => a -> Bijection a a
mulB :: Fractional a => a -> Bijection a a
divB :: Fractional a => a -> Bijection a a
addB n = Bijection ((+) n, (-) n)
subB n = inverse (addB n)                                
mulB n = Bijection ((*) n, (/) n)
divB n = inverse (mulB n)                                




