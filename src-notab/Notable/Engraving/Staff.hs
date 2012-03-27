
{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

-- | Staff-level engraving.
module Notable.Engraving.Staff
where

import Notable.Core
import Notable.Core.Diagrams
import Notable.Core.Symbols

noteLineWidth = 0.025
barLineWidth  = 0.04

noteLines :: Double -> Engraving
noteLines width = 
    foldr (===) mempty (replicate 5 noteLine)
        # moveOriginBy (r2 (0, -2 * space))
        where
            noteLine  =  hrule width # lw noteLineWidth 
                           <> 
                         spaceRect width space
                         
singleBarLine :: Engraving
singleBarLine = lineE <> spaceE
    where
        lineE   =  vrule (4 * space) # lw barLineWidth
        spaceE  =  spaceRect (space * 4/9) (space * 4)

doubleBarLine :: Engraving
doubleBarLine = beside unitX (align unitX singleBarLine) singleBarLine

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

