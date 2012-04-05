
{-|
    Module      :  Music.Util.Either
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable

    This module reexports and augments 'Data.Either' with some new functions.
-}

module Music.Util.Either
(
    module Data.Either,

    getLeft,
    getRight,
    mapLeft,
    mapRight,
    fmapEither,
    fmapLeft,
    fmapRight,
)
where

import Data.Either

getLeft :: Either a b -> a
getLeft (Left x)   = x

getRight :: Either a b -> b
getRight (Right x) = x

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)    =  Left (f x)
mapLeft f (Right x)   =  Right x

mapRight :: (a -> b) -> Either c a -> Either c b
mapRight f (Left x)   =  Left x
mapRight f (Right x)  =  Right (f x)

fmapEither :: Functor f => (a -> b) -> (c -> d) -> f (Either a c) -> f (Either b d)
fmapEither f g = fmap (mapLeft f . mapRight g)

fmapLeft :: Functor f => (a -> b) -> f (Either a c) -> f (Either b c)
fmapLeft f = fmap (mapLeft f)

fmapRight :: Functor f => (a -> b) -> f (Either c a) -> f (Either c b)
fmapRight f = fmap (mapRight f)
