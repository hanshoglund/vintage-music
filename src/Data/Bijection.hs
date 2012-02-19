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


{-|
    Bijective function, represented as a function with its inverse.
-}
newtype Bijection a b = Bijection (b, a)

newtype Permutation a = Permutation (a, a)

inverse :: Permutation a -> Permutation a
inverse (Permutation (f, g)) = Permutation (g, f)


instance Functor (Bijection a) where
    fmap f (Bijection (b, a)) = Bijection (f b, a)
instance Applicative (Bijection a) where
    pure f = Bijection (f, undefined)                           -- FIXME does not work out!
    Bijection (g, f) <*> Bijection (b, a) = Bijection (g b, a)

instance Functor (Permutation) where
    fmap f (Permutation (a, b)) = Permutation (f a, f b)
instance Applicative (Permutation) where
    pure f = Permutation (f, f)
    Permutation (f, g) <*> Permutation (a, b) = Permutation (f a, g b)