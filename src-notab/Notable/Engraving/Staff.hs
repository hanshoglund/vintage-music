
{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

-- | Staff-level engraving.
module Notable.Engraving.Staff
(
-- * Note lines
noteLineWeight,
noteLines,
standardNoteLines,

-- * Barlines
barLineWeight,
singleBarLine,
doubleBarLine,

-- * Clefs
ClefPos,
ClefType,
Clef,
clef,
clefSymbol,

frenchClef,
trebleClef,
sopranoClef,
mezzoSopranoClef,
altoClef,
tenorClef,
baritoneClef,
bassClef,
subBassClef,
)

where

import Notable.Core
import Notable.Core.Diagrams
import Notable.Core.Symbols

--
-- Constants
--

-- | Thickness of note lines.
noteLineWeight :: Double
noteLineWeight = 0.025

-- | Thickness of barlines.
barLineWeight :: Double
barLineWeight  = 0.04


--
-- Note lines
--

-- | Engraves a set of note line.
noteLines :: Int -> Double -> Engraving
noteLines num width = 
    foldr (===) mempty (replicate num noteLine)
        # moveOriginBy (r2 (0, (negate $ (fromIntegral num - 1) / 2) * space))
        where
            noteLine  =  hrule width # lw noteLineWeight 
                           <> 
                         spaceRect width space
                         
-- | Engraves a standard set of note line.
standardNoteLines :: Double -> Engraving
standardNoteLines = noteLines 5

--
-- Bar lines
--

-- | Engraves a single bar line.
singleBarLine :: Engraving
singleBarLine = lineE <> spaceE
    where
        lineE   =  vrule (4 * space) # lw barLineWeight
        spaceE  =  spaceRect (space * 4/9) (space * 4)


-- | Engraves a double bar line.
doubleBarLine :: Engraving
doubleBarLine = beside unitX (align unitX singleBarLine) singleBarLine

-- TODO
-- thickBarLine
-- dashedBarLine
-- shortBarLine
-- tickBarLine
-- finalBarLine
-- startRepriseBarLine
-- endRepriseBarLine
-- startEndRepriseBarLine


--
-- Clefs
--

type ClefPos = HalfSpaces
data ClefType
    = GClef
    | CClef
    | FClef

type Clef = (ClefType, ClefPos)

clef :: Clef -> Engraving
clef (clefType, pos) =
    moveToPosition pos $ clefE <> spaceE
    where   
        clefE   =  baselineText clefGlyph # font clefFont
        spaceE  =  spaceRect (3 * space) (4 * space)

        (clefFont, clefGlyph)  =  clefSymbol clefType


clefSymbol :: ClefType -> Symbol
clefSymbol GClef  =  (baseMusicFont, "&")
clefSymbol CClef  =  (baseMusicFont, "B")
clefSymbol FClef  =  (baseMusicFont, "?")

frenchClef        :: Engraving
trebleClef        :: Engraving
sopranoClef       :: Engraving
mezzoSopranoClef  :: Engraving
altoClef          :: Engraving
tenorClef         :: Engraving
baritoneClef      :: Engraving
bassClef          :: Engraving
subBassClef       :: Engraving
frenchClef        = clef (GClef, -4)
trebleClef        = clef (GClef, -2)
sopranoClef       = clef (CClef, -4)
mezzoSopranoClef  = clef (CClef, -2)
altoClef          = clef (CClef, -0)
tenorClef         = clef (CClef, 2)
baritoneClef      = clef (CClef, 4)
bassClef          = clef (FClef, 2)
subBassClef       = clef (FClef, 4)

