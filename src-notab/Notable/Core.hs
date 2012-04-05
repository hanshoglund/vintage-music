
{-# LANGUAGE  
    TypeFamilies,
    RankNTypes,
    FlexibleContexts #-}

-- | This module contains preliminaries for music engraving.
module Notable.Core
(
-- * Base units
    Spaces,
    space,
    HalfSpaces,
    halfSpace,

    -- prime,
    -- second,
    -- third,
    -- fourth,
    -- fifth,
    -- sixth,
    -- seventh,
    -- octave,
    
    NoteValue,
    
    Direction,
    upwards,
    downwards,
          
    StaffLines,

-- * Core types
    Notation(..),
    Engraving,

-- * Music engraving
    spaceRect,
    spaceRectV,
    engraveSymbol,
    engraveSymbolFloating,
    engraveSpacer,
    moveSpacesUp,
    moveHalfSpacesUp,
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

-- prime   = 0 * halfSpace
-- second  = 1 * halfSpace
-- third   = 2 * halfSpace
-- fourth  = 3 * halfSpace
-- fifth   = 4 * halfSpace
-- sixth   = 5 * halfSpace
-- seventh = 6 * halfSpace
-- octave  = 7 * halfSpace


-- | Standard note value (@1/4@ for quarter note etc).
type NoteValue = Double

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

-- | Creates a transparent rectangle.
--   This is useful as an alternative to 'withEnvelope' for debugging purposes.
spaceRect :: Double -> Double -> Engraving
spaceRect x y = style $ rect x y
    where
        style = fillColor blue . opacity 0

spaceRectV :: R2 -> Engraving
spaceRectV v = spaceRect (getX v) (getY v)

engraveSymbol :: Symbol -> Engraving
engraveSymbol s = engraveSymbolFloating s <> engraveSpacer s

engraveSymbolFloating :: Symbol -> Engraving
engraveSymbolFloating (font', glyph) = font font' $ baselineText glyph

engraveSpacer :: Symbol -> Engraving
engraveSpacer s = translate (symbolOffset s) $ spaceRectV (symbolSpacer s)


--
-- Positioning etc
--

moveSpacesUp :: (V t ~ R2, Transformable t) => HalfSpaces -> t -> t
moveSpacesUp x = translate (r2 (0, x * space))

moveHalfSpacesUp :: (V t ~ R2, Transformable t) => HalfSpaces -> t -> t
moveHalfSpacesUp x = translate (r2 (0, x * halfSpace))




-- | So far just a dummy type.
data Notation = Notation

-- | An engraved note symbol.                       
--
--   This is precisely a diagram in the diagrams library, so it supports transformations,
--   rendering and composition with other 'Engraving' values.
type Engraving = (Renderable Text b, Renderable (Path R2) b, Backend b R2) => Diagram b R2


