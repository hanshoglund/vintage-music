{-# LANGUAGE  
    TypeFamilies,
    RankNTypes,
    FlexibleContexts #-}

module Notable.Core
where

import Notable.Core.Diagrams


--
-- Preliminaries
--

-- | Base unit of engraving. Equal to the space between two note lines.
type Spaces = Double

-- | Unit of half a space.
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



--
-- Notation and Engraving
--

spaceRect :: Double -> Double -> Engraving
spaceRect x y = rect x y # fc blue # opacity 0

moveToPosition :: (V t ~ R2, Transformable t) => Double -> t -> t
moveToPosition pos = translate (r2 (0, space * pos / 2))

-- | So far just a dummy type.
data Notation = Notation

-- | An engraved note symbol.                       
--
--   This is precisely a diagram in the diagrams library, so it supports transformations,
--   rendering and composition with other engravings.
type Engraving = (Renderable Text b, Renderable (Path R2) b, Backend b R2) => Diagram b R2


