
{-# LANGUAGE
    FlexibleContexts #-}

-- | Low-level engraving of staff-level objects, such as note lines, bar lines, clefs, key and
--   time signatures and so on. Notes, rests and associated objects are delegated to the
--   "Notable.Engraving.Chord" module.
--
--   Staff-level objects are grouped into spaced an non-spaced. On the staff level, spaced objects are  take
--   those objects that take up horizontal space, including notes, rests, clefs, time signatures etc. In 
--   simple case such as tables or legends, such objects may simply be stacked using 'besideX'.
--   For more involved cases, see the "Notable.Spacing" module.
--
--   Non-spaced objects are placed in relation to spaced objects, using a position returned form the lower
--   engraving level. 
--     
module Notable.Engraving.Staff
(
-- * Note lines
    noteLineWidth,
    noteLines,
    noteLines',

-- * Spaced objects
-- ** Barlines
    barLineWeight,
    singleBarLine,
    doubleBarLine,
-- *** Rehearsal marks

-- ** Clefs
    ClefPos,
    ClefType(..),
    Clef,
    clefSymbol,
    engraveClef,

-- *** Standard clefs
    frenchClef,
    trebleClef,
    sopranoClef,
    mezzoSopranoClef,
    altoClef,
    tenorClef,
    baritoneClef,
    bassClef,
    subBassClef,

-- ** Key signatures
-- ** Time signatures
-- ** Cesuras
-- ** Chords


-- * Non-spaced objects
-- ** Beams
-- *** Tremolo beams
-- ** Ties
-- ** Slurs
-- ** Tuplets
-- ** Text
)

where

import Notable.Core
import Notable.Core.Symbols
import Notable.Core.Diagrams
import Notable.Engraving.Chord

--
-- Constants
--

-- | Thickness of note lines.
noteLineWidth :: Double
noteLineWidth = 0.025

-- | Thickness of barlines.
barLineWeight :: Double
barLineWeight  = 0.04


--
-- Note lines
--

-- | A standard set of five note lines. The origin will be at the left edge on the middle line.
--
--   Note lines engraved at length one. To obtain other lengths, use 'stretchX' or 'stretchToX'.
noteLines :: Engraving
noteLines = noteLines' 5

-- | A set of note lines. The origin will be at the left edge on the middle line or space.
--
--   Note lines engraved at length one. To obtain other lengths, use 'stretchX' or 'stretchToX'.
noteLines' :: StaffLines -> Engraving
noteLines' num =
    placement $ foldr above mempty (replicate num noteLine)
        where
            placement = moveOriginBy (r2 (0, (negate $ (fromIntegral num - 1) / 2) * space))
            noteLine  =  style $ hrule 1 <> {-spaceRect rect 1 space-} strutY space
                where { style = lineWidth noteLineWidth}

--
-- Bar lines
--

-- | A single bar line.
--
--   Bar lines engraved at length four, to fit into a standard five-line system. To obtain other
--   lengths, use 'stretchY' or 'stretchToY'.
singleBarLine :: Engraving
singleBarLine = lineE <> spaceE
    where
        spaceE  =  spaceRect (space * 4/9) (space * 4)
        lineE   =  style $ vrule (4 * space) 
            where { style = lineWidth barLineWeight }
        

-- | A double bar line.
--
--   Bar lines engraved at length four, to fit into a standard five-line system. To obtain other
--   lengths, use 'stretchY' or 'stretchToY'.
doubleBarLine :: Engraving
doubleBarLine = beside unitX (align unitX singleBarLine) singleBarLine
-- TODO factor out this pattern

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

-- | Position that the clef will indicate, offset from the middle line.
--
--   For example, a standard alto clef has position @0@, while a treble clef
--   has position @-2@.
--
type ClefPos = HalfSpaces

data ClefType
    = GClef
    | CClef
    | FClef
    deriving (Show, Eq)

type Clef = (ClefType, ClefPos)

-- | Symbol used to represent a given clef type.
clefSymbol :: ClefType -> Symbol
clefSymbol GClef  =  (baseMusicFont, "&")
clefSymbol CClef  =  (baseMusicFont, "B")
clefSymbol FClef  =  (baseMusicFont, "?")

-- | Engraves a standard size clef.
engraveClef :: Clef -> Engraving
engraveClef (clefType, pos) =
    moveHalfSpacesUp pos $ clefE <> spaceE
    where
        clefE   =  engraveSymbolFloating sym
        spaceE  =  spaceRectV (symbolSpacer sym) # translate (symbolOffset sym)
        sym     =  clefSymbol clefType


frenchClef        :: Engraving
trebleClef        :: Engraving
sopranoClef       :: Engraving
mezzoSopranoClef  :: Engraving
altoClef          :: Engraving
tenorClef         :: Engraving
baritoneClef      :: Engraving
bassClef          :: Engraving
subBassClef       :: Engraving
frenchClef        = engraveClef (GClef, -4)
trebleClef        = engraveClef (GClef, -2)
sopranoClef       = engraveClef (CClef, -4)
mezzoSopranoClef  = engraveClef (CClef, -2)
altoClef          = engraveClef (CClef, 0)
tenorClef         = engraveClef (CClef, 2)
baritoneClef      = engraveClef (CClef, 4)
bassClef          = engraveClef (FClef, 2)
subBassClef       = engraveClef (FClef, 4)




--
-- Key signatures
--

--
-- Time signatures
--

--
-- Cesuras
--

--
-- Chords
--




--
-- Beams
--

--
-- Tremolo beams
--

--
-- Ties
--

--
-- Slurs
--

--
-- Tuplets
--


--
-- Text
--



