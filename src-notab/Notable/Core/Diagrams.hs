
{-# LANGUAGE 
    TypeFamilies #-}

-- | This module reexports standard components of the Diagrams API used
--   throughout Notable.
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

