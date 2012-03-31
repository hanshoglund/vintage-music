
{-# LANGUAGE  
    TypeFamilies,
    RankNTypes,
    FlexibleContexts #-}

-- | This module contains preliminaries for music engraving.
module Notable.Core
(
-- * Base units
    Spaces,
    HalfSpaces,
    Direction,

    space,
    halfSpace,
    upwards,
    downwards,
          
    StaffLines,

-- * Core types
    Notation(..),
    Engraving,

-- * Music engraving
    spaceRect,
    spaceRectR,
    moveToPosition,
    engraveSymbol
)
where

import Notable.Core.Diagrams
import Notable.Core.Symbols

--
-- Preliminaries
--

-- | Base unit of engraving. Equal to the space between two note lines.
type Spaces = Double

-- | Unit of half a space. Equal to half the space between two note lines, so
--
--   > space / 2 = halfSpace
type HalfSpaces = Double

space :: Spaces
space = 0.25

halfSpace :: HalfSpaces
halfSpace = space / 2

-- | Direction of note stem. 
--   Needed for calculating placement of articulations, lines etc. 
type Direction = Bool
upwards   = True
downwards = False

-- | Number of lines in a staff.
type StaffLines = Int



--
-- Notation and Engraving
--

spaceRect :: Double -> Double -> Engraving
spaceRect x y = rect x y # fc blue 
    -- # opacity 0.1
    # opacity 0

spaceRectR :: R2 -> Engraving
spaceRectR v = spaceRect (fst . unr2 $ v) (snd . unr2 $ v)

moveToPosition :: (V t ~ R2, Transformable t) => Double -> t -> t
moveToPosition pos = translate (r2 (0, space * pos / 2))

engraveSymbol :: Symbol -> Engraving
engraveSymbol (symFont, symGlyph) = baselineText symGlyph # font symFont



-- | So far just a dummy type.
data Notation = Notation

-- | An engraved note symbol.                       
--
--   This is precisely a diagram in the diagrams library, so it supports transformations,
--   rendering and composition with other 'Engraving' values.
type Engraving = (Renderable Text b, Renderable (Path R2) b, Backend b R2) => Diagram b R2


