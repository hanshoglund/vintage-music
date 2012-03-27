{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

module Notable.Core
where

import Notable.Core.Diagrams


--
-- Preliminaries
--

-- | Base unit of engraving. Equal to the space between two note lines.
type Spaces = Double

space :: Spaces
space = 0.27

-- | Unit of half a space.
type HalfSpaces = Double

-- | Base stem direction.
--   Also used for determining placement of articulations etc. 
type Direction = Bool
upwards   = True
downwards = False


--
-- Notation and Engraving
--

data Notation = Notation
type Engraving = (Renderable Text b, Renderable (Path R2) b, Backend b R2) => Diagram b R2


