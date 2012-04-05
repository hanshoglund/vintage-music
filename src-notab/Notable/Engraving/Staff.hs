
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleContexts #-}

-- | Low-level engraving of staff-level objects, such as note lines, bar lines, clefs, key and
--   time signatures and so on. 
--
--   Staff-level objects are grouped into spaced an non-spaced. On the staff level, spaced objects are 
--   take those objects that take up horizontal space, including notes, rests, clefs, time signatures etc.
--   In simple case such as tables or legends, such objects may simply be stacked using 'besideX'. For more
--   involved cases, see the "Notable.Spacing" module.
--
--   Non-spaced objects are placed in relation to spaced objects, using a position returned form the lower
--   engraving level. 
--     
module Notable.Engraving.Staff
(
-- * Note lines
    noteLineWeight,
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
    KeySignature,
    engraveKeySignature,
    gFlatMajor,
    dFlatMajor,
    aFlatMajor,
    eFlatMajor,
    bFlatMajor,
    fMajor,
    cMajor,
    gMajor,
    dMajor,
    aMajor,
    eMajor,
    bMajor,
    fSharpMajor,

-- ** Time signatures
    TimeSignature,
    engraveTimeSignature,

-- ** Cesuras
    apostrophe,
    cesura,

-- ** Chords
    engraveRest,    
    engraveNote,    
    engraveChord,    



-- * Non-spaced objects

-- ** Beams
    Beams,
    engraveBeams,

-- *** Tremolo beams
    TremoloBeams,
    engraveTremoloBeams,    

-- ** Ties
    engraveTie,    

-- ** Slurs    
    engraveSlur,

-- ** Tuplets
    engraveTuplet,

-- ** Text
    Instruction(..),
    engraveInstruction,

    

-- * Staves
    NonSpacedObject(..),
    SpacedObject(..),
    Staff(..),
    engraveStaff,
)

where

import Data.Indexed

import Notable.Core
import Notable.Core.Symbols
import Notable.Core.Diagrams
import Notable.Engraving.Chord

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
    -- TODO use cat' instead of foldr?
    placement $ foldr above mempty (replicate num noteLine)
        where
            placement = moveSpacesUp $ (fromIntegral num - 1) / 2
            noteLine  =  style $ hrule 1 <> {-spaceRect rect 1 space-} strutY space
                where { style = lineWidth noteLineWeight }

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
type ClefPos = HalfSpaces

data ClefType
    = GClef
    | CClef
    | FClef
    deriving (Show, Eq)

type Clef = (ClefType, ClefPos)

instance Symbolic ClefType where
    symbol GClef  =  (baseMusicFont, "&")
    symbol CClef  =  (baseMusicFont, "B")
    symbol FClef  =  (baseMusicFont, "?")

-- | Engraves a standard size clef.
engraveClef :: Clef -> Engraving
engraveClef (clefType, pos) =
    moveHalfSpacesUp pos $ engraveSymbol (symbol clefType)


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

type KeySignature = Int

engraveKeySignature :: KeySignature -> Engraving
engraveKeySignature = undefined

gFlatMajor  :: Engraving
dFlatMajor  :: Engraving
aFlatMajor  :: Engraving
eFlatMajor  :: Engraving
bFlatMajor  :: Engraving
fMajor      :: Engraving
cMajor      :: Engraving
gMajor      :: Engraving
dMajor      :: Engraving
aMajor      :: Engraving
eMajor      :: Engraving
bMajor      :: Engraving
fSharpMajor :: Engraving
gFlatMajor  = engraveKeySignature (-6)
dFlatMajor  = engraveKeySignature (-5)
aFlatMajor  = engraveKeySignature (-4)
eFlatMajor  = engraveKeySignature (-3)
bFlatMajor  = engraveKeySignature (-2)
fMajor      = engraveKeySignature (-1)
cMajor      = engraveKeySignature 0
gMajor      = engraveKeySignature 1
dMajor      = engraveKeySignature 2
aMajor      = engraveKeySignature 3
eMajor      = engraveKeySignature 4
bMajor      = engraveKeySignature 5
fSharpMajor = engraveKeySignature 6


--
-- Time signatures
--

-- TODO c, alla breve, pulse group time sig etc.
type TimeSignature = (Int, Int)

engraveTimeSignature :: TimeSignature -> Engraving
engraveTimeSignature = undefined

--
-- Cesuras
--

apostrophe :: Engraving
apostrophe = undefined

cesura :: Engraving
cesura = undefined

--
-- Beams
--

-- TODO sub-beams, vertical positioning in case of multiple beams etc.
type Beams = Int

engraveBeams :: Beams -> R2 -> R2 -> Engraving
engraveBeams = undefined


--
-- Tremolo beams
--

type TremoloBeams = Int

engraveTremoloBeams :: TremoloBeams -> R2 -> R2 -> Engraving
engraveTremoloBeams = undefined


--
-- Ties
--

engraveTie :: Direction -> R2 -> R2 -> Engraving
engraveTie = undefined


--
-- Slurs
--

engraveSlur :: Direction -> R2 -> R2 -> Engraving
engraveSlur = undefined


--
-- Tuplets
--

engraveTuplet :: String -> Direction -> R2 -> R2 -> Engraving
engraveTuplet = undefined

--
-- Instructions
--

type Instruction = String

engraveInstruction :: Instruction -> Engraving
engraveInstruction = undefined


--
-- Staves
--

data NonSpacedObject 
    = Beams Beams
    | TremoloBeams TremoloBeams 
    | Tie Direction
    | Slur Direction 
    | TupletBracket Direction
    | Instruction String
    
data SpacedObject 
    = Clef Clef
    | KeySignature KeySignature 
    | TimeSignature TimeSignature 
    | Barline 
    | Cesura
    | Chord Chord
    
data Staff = 
    Staff { spacedObjects    :: [(HalfSpaces, SpacedObject)],
            nonSpacedObjects :: [([Index [SpacedObject]], NonSpacedObject)] } 

engraveStaff :: Staff -> Engraving
engraveStaff = undefined
