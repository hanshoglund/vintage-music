
{-# LANGUAGE 
    TypeFamilies #-}

-- | This module reexports the components of the Diagrams API used throughout Notable, as
--   well as some handy definitions.

module Music.Notable.Core.Diagrams
(
    module Diagrams.Prelude,
    module Diagrams.TwoD.Text,

    getX,
    getY,
    leftTo,
    rightTo,
    above,
    below,
    catLeft,
    catRight,
    catUp,
    catDown,
    emptyEnvelope,
)
where

import Data.Colour ( withOpacity )
import Data.Colour.SRGB ( sRGB24read )

import Diagrams.Prelude hiding ( Render, render, (|||), (===), Dynamic, fromDynamic )
import Diagrams.TwoD.Text ( Text )
import Graphics.Rendering.Diagrams.Envelope( Envelope(..) )

-- | Horizontal projection of the given vector.
getX :: R2 -> Double
getX = fst . unr2

-- | Vertical projection of the given vector.
getY :: R2 -> Double
getY = snd . unr2

-- | @x \`leftTo\` y@ places @x@ left to @y@.
leftTo :: (V a ~ R2, Semigroup a, Juxtaposable a) => a -> a -> a
leftTo  = beside unitX

-- | @x \`rightTo\` y@ places @x@ right to @y@.
rightTo :: (V a ~ R2, Semigroup a, Juxtaposable a) => a -> a -> a
rightTo = beside (negateV unitX)

-- | @x \`above\` y@ places @x@ above @y@.
above :: (V a ~ R2, Semigroup a, Juxtaposable a) => a -> a -> a
above   = beside (negateV unitY)

-- | @x \`below\` y@ places @x@ below @y@.
below :: (V a ~ R2, Semigroup a, Juxtaposable a) => a -> a -> a
below   = beside unitY

-- | Catenation from right to left.
catLeft :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catLeft = cat (negateV unitX)

-- | Catenation from left to right.
catRight :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catRight = cat unitX

-- | Upwards catenation.
catUp :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catUp = cat unitY

-- | Downwards catenation.
catDown :: (V a ~ R2, HasOrigin a, Monoid a, Semigroup a, Juxtaposable a) => [a] -> a
catDown = cat (negateV unitY)

emptyEnvelope :: Envelope R2
emptyEnvelope = Envelope $ Option Nothing

