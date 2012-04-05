
{-# LANGUAGE 
    TypeFamilies #-}

-- | This module reexports the components of the Diagrams API used throughout Notable, as
--   well as some handy synonyms.

module Notable.Core.Diagrams
(
    module Diagrams.Prelude,
    module Diagrams.TwoD.Text,

    leftTo,
    rightTo,
    above,
    below,

    getX,
    getY,
    catLeft,
    catRight,
    catUp,
    catDown,
)
where

import Data.Colour ( withOpacity )
import Data.Colour.SRGB ( sRGB24read )

import Diagrams.Prelude hiding ( Render, render, (|||), (===) )
import Diagrams.TwoD.Text ( Text )


leftTo, rightTo, above, below :: (V a ~ R2, Semigroup a, Juxtaposable a) => a -> a -> a
leftTo  = beside unitX
rightTo = beside (negateV unitX)
above   = beside (negateV unitY)
below   = beside unitY

getX :: R2 -> Double
getX = fst . unr2

getY :: R2 -> Double
getY = snd . unr2

catLeft :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catLeft = cat (negateV unitY)

catRight :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catRight = cat unitX

catUp :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catUp = cat unitY

catDown :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catDown = cat (negateV unitY)
