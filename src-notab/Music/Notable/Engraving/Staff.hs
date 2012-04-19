
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleContexts #-}

-- | This module handles engraving of staff-level objects, such as note lines, bar lines, clefs, key and time
--   signatures and so on. 
--
--   Staff-level objects are grouped into spaced an non-spaced. On the staff level, spaced objects are 
--   take those objects that take up horizontal space, including notes, rests, clefs, time signatures etc.
--   In simple case such as tables or legends, such objects may simply be stacked using 'beside'. For more
--   involved cases, see the "Notable.Spacing" module.
--
--   Non-spaced objects are placed in relation to spaced objects, using a position returned form the lower
--   engraving level. 
--     
module Music.Notable.Engraving.Staff
(
-- * Note lines
    noteLines,
    noteLines',

-- * Spaced objects

-- ** Barlines
    singleBarLine,
    doubleBarLine,
    thickBarLine,
    shortBarLine,
    tickBarLine,
    finalBarLine,

-- ** Sustain lines
    SustainLinePosition,
    SustainLineLength,
    SustainLine,
    engraveSustainLine,
    
-- *** Rehearsal marks

-- ** Clefs
    ClefPosition,
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

-- ** Time signatures
    TimeSignature,
    engraveTimeSignature,

-- ** Cesuras
    apostrophe,
    cesura,

-- * Non-spaced objects
-- ** Beams and tremolo beams
    Beams,
    engraveBeams,
    TremoloBeams,
    engraveTremoloBeams,    
-- ** Ties and slurs
    engraveTie,    
    engraveSlur,
-- ** Brackets
    engraveTuplet,

-- ** Text     
    Rehearsal,
    engraveRehearsal,

    BeatsPerMinute,
    toMetronomeScale,
    engraveMetronomeMark,

    DynamicLetter(..),
    Dynamic(..),
    dynamic,
    fromDynamic,
    fff, ff, f, mf, mp, p, pp, ppp,
    engraveDynamic,

    Instruction,
    Expression,
    engraveInstruction,
    engraveExpression,

-- * Staves
    NonSpacedObject(..),
    SpacedObject(..),
    StaffOptions(..), 
    Staff(..), 
    staffWidth,
    moveObjectsRight,
    moveObjectsLeft,
    addSpaceAtStart,
    addSpaceAtEnd,
    addSpace,
    scaleStaff,
    scaleStaffTo,
    justifyStaves,
    splitStaff,
    splitStaffWhen,
    engraveStaff,
)

where

import Data.Convert
import Data.Index
import Data.Trivial

import Music.Util
import Music.Util.List

import Music.Notable.Core
import Music.Notable.Core.Symbols
import Music.Notable.Core.Diagrams
import Music.Notable.Engraving.Chord

--
-- Constants
--

-- | Thickness of note lines.
kNoteLineWeight      :: Double
kNoteLineWeight      = 0.025

-- | Thickness of barlines.
kBarLineWeight       :: Double
kBarLineWeight       = 0.04

kMetronomeMarkOffset :: Spaces
kInstructionOffset   :: Spaces
kExpressionOffset    :: Spaces
kDynamicOffset       :: Spaces
kMetronomeMarkOffset = 5
kInstructionOffset   = 5
kExpressionOffset    = 6
kDynamicOffset       = 6

kMetronomeMarkScale  :: Double
kInstructionScale    :: Double
kExpressionScale     :: Double
kDynamicScale        :: Double
kMetronomeMarkScale  = 0.6
kInstructionScale    = 0.6
kExpressionScale     = 0.5
kDynamicScale        = 0.6


--
-- Note lines
--

-- | A standard set of five note lines. The origin will be at the left edge on the middle line.
--
--   Note lines engraved at length one. To obtain other lengths, use 'stretchX' or 'stretchTo'.
noteLines :: Engraving
noteLines = noteLines' 5

-- | A set of note lines. The origin will be at the left edge on the middle line or space.
--
--   Note lines engraved at length one. To obtain other lengths, use 'stretchX' or 'stretchTo'.
noteLines' :: StaffLines -> Engraving
noteLines' num =     
    -- TODO use cat' instead of foldr?
    placement $ foldr above mempty (replicate num noteLine)
        where
            placement = moveSpacesUp $ (fromIntegral num - 1) / 2
            noteLine  =  style $ hrule 1 <> {-spaceRect rect 1 space-} strutY (convert space)
                where { style = lineWidth kNoteLineWeight }

--
-- Bar lines
--

-- | A single bar line.
--
--   Bar lines engraved at length four, to fit into a standard five-line system. To obtain other
--   lengths, use 'stretchY' or 'stretchTo'.
singleBarLine :: Engraving
singleBarLine = barLine 1 4
        

-- | A double bar line.
--
--   Bar lines engraved at length four, to fit into a standard five-line system. To obtain other
--   lengths, use 'stretchY' or 'stretchTo'.
doubleBarLine :: Engraving
doubleBarLine = beside unitX (align unitX singleBarLine) (barLine 1 4)
-- TODO factor out this pattern

thickBarLine :: Engraving
thickBarLine = barLine 3 4

shortBarLine :: Engraving
shortBarLine = barLine 1 2

tickBarLine :: Engraving
tickBarLine  = moveSpacesUp 2 $ barLine 1 1

finalBarLine :: Engraving
finalBarLine = barLine 1 4 `leftTo` barLine 3 4

-- dashedBarLine
-- startRepriseBarLine
-- endRepriseBarLine
-- startEndRepriseBarLine

barLine thickness height = lineE <> spaceE
    where
        spaceE  =  spaceRect (convert space * 4/9) (convert space * 4)
        lineE   =  style $ vrule (height * convert space) 
            where { style = lineWidth (thickness * kBarLineWeight) }


--
-- Rehearsal marks
--                

type Rehearsal = String

engraveRehearsal :: Rehearsal -> Engraving
engraveRehearsal = mempty -- TODO

--
-- Sustain lines
--

-- | Position to indicate, offset from middle line.
type SustainLinePosition = HalfSpaces

-- | Length of line.
type SustainLineLength = Spaces

type SustainLine = (SustainLinePosition, SustainLineLength)

-- | Engraves a sustain line. The origin will be to the left, at position zero.
engraveSustainLine :: SustainLine -> Engraving
engraveSustainLine (pos, len) = p . s $ rect (convert len) (convert $ (2/5) * space)
    where                       
        p = moveHalfSpacesUp pos . alignL
        s = lineWidth 0 . fillColor black
--
-- Clefs
--

-- | Vertical position that the clef will indicate, offset from the middle line.
--
--   For example, a standard alto clef has position @0@, while a treble clef
--   has position @-2@.
type ClefPosition = HalfSpaces

data ClefType
    = GClef
    | CClef
    | FClef
    deriving (Eq, Show)

type Clef = (ClefType, ClefPosition)

instance Symbolic ClefType where
    symbol GClef  =  (baseMusicFont, "&")
    symbol CClef  =  (baseMusicFont, "B")
    symbol FClef  =  (baseMusicFont, "?")

-- | Engraves a standard size clef.
engraveClef :: Clef -> Engraving
engraveClef (clefType, pos) =
    moveHalfSpacesUp pos $ engraveSymbol (symbol clefType)


frenchClef        :: Clef
trebleClef        :: Clef
sopranoClef       :: Clef
mezzoSopranoClef  :: Clef
altoClef          :: Clef
tenorClef         :: Clef
baritoneClef      :: Clef
bassClef          :: Clef
subBassClef       :: Clef
frenchClef        = (GClef, -4)
trebleClef        = (GClef, -2)
sopranoClef       = (CClef, -4)
mezzoSopranoClef  = (CClef, -2)
altoClef          = (CClef, 0)
tenorClef         = (CClef, 2)
baritoneClef      = (CClef, 4)
bassClef          = (FClef, 2)
subBassClef       = (FClef, 4)




--
-- Key signatures
--

type KeySignature = Int

engraveKeySignature :: KeySignature -> Engraving
engraveKeySignature = undefined

gFlatMajor  :: KeySignature
dFlatMajor  :: KeySignature
aFlatMajor  :: KeySignature
eFlatMajor  :: KeySignature
bFlatMajor  :: KeySignature
fMajor      :: KeySignature
cMajor      :: KeySignature
gMajor      :: KeySignature
dMajor      :: KeySignature
aMajor      :: KeySignature
eMajor      :: KeySignature
bMajor      :: KeySignature
fSharpMajor :: KeySignature
gFlatMajor  = (-6)
dFlatMajor  = (-5)
aFlatMajor  = (-4)
eFlatMajor  = (-3)
bFlatMajor  = (-2)
fMajor      = (-1)
cMajor      = 0
gMajor      = 1
dMajor      = 2
aMajor      = 3
eMajor      = 4
bMajor      = 5
fSharpMajor = 6


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
-- Metronome marks
--

-- | BeatsPerMinute in beats per minute.
type BeatsPerMinute = Int

-- | Rounds the given tempo to one of the standard metronome settings.
toMetronomeScale :: BeatsPerMinute -> BeatsPerMinute
toMetronomeScale x
    | x <= 60    =  x - rem x 2
    | x <= 72    =  x - rem x 3
    | x <= 120   =  x - rem x 4
    | x <= 144   =  x - rem x 6
    | otherwise  =  x - rem x 8    

-- | Engrave a metronome mark, binding the given note value to the given tempo.
--
--   The mark will be positioned slightly above the staff aligned to the left. Use @centerX@ or @alignR@ if
--   another alignment is required.
engraveMetronomeMark :: NoteValue -> BeatsPerMinute -> Engraving
engraveMetronomeMark nv tempo = t $ mempty
    <> (engraveNote up 0 nh `leftTo` engraveText text)
    <> (translate (r2 (-0.8, 0.4)) . alignL $ spaceRect 4.2 1.5)
    where
        nh = noteHeadFromNoteValue nv
        text = " = " ++ show tempo
        t = moveSpacesUp kMetronomeMarkOffset . scale kMetronomeMarkScale


--
-- Dynamics
--

-- | The letters used in dynamic expressions.
data DynamicLetter = F | P | M | R | Z 
    deriving (Eq, Ord, Show, Enum)

dynCh F = 'f'
dynCh P = 'p'
dynCh M = 'm'
dynCh R = 'r'
dynCh Z = 'z'
chDyn 'f' = F
chDyn 'p' = P
chDyn 'm' = M
chDyn 'r' = R
chDyn 'z' = Z

-- | A dynamic expression.
type Dynamic = [DynamicLetter]

dynamic :: String -> Dynamic
dynamic = fmap chDyn                

fromDynamic :: Dynamic -> String 
fromDynamic = fmap dynCh

fff  =  dynamic "fff"
ff   =  dynamic "ff"
f    =  dynamic "f"
mf   =  dynamic "mf"
mp   =  dynamic "mp"
p    =  dynamic "p"
pp   =  dynamic "pp"
ppp  =  dynamic "ppp"


-- | Engraves a dynamic mark.
--
--   The mark will be positioned slightly below the staff aligned to the left. Use @centerX@ or @alignR@ if
--   another alignment is required.
engraveDynamic :: Dynamic -> Engraving
engraveDynamic d = t $ mempty
    <> engraveSpecialText (fromDynamic d)
    <> (translate (r2 (-0.2, 0.2)) . alignL $ spaceRect (0.2 + 0.65 * fromIntegral $ length d) 1.5)
    where
        t = moveSpacesDown (0.6 + kDynamicOffset) . scale kDynamicScale



--
-- Instructions
--

type Instruction = String
type Expression = String

engraveInstruction :: Instruction -> Engraving
engraveInstruction txt = t $ mempty
    <> engraveText txt
    <> (translate (r2 (-0.2, 0.2)) . alignL $ spaceRect (0.2 + 0.65 * fromIntegral $ length txt) 1.5)
    where
        t = moveSpacesUp kInstructionOffset . scale kInstructionScale


engraveExpression :: Expression -> Engraving
engraveExpression txt = t $ mempty
    <> (italic $ engraveText txt)
    <> (translate (r2 (-0.2, 0.2)) . alignL $ spaceRect (0.2 + 0.65 * fromIntegral $ length txt) 1.5)
    where
        t = moveSpacesDown (0.6 + kExpressionOffset) . scale kExpressionScale


--
-- Staves
--

-- | Spaced staff objects, associated with a horizontal position.    
data SpacedObject 
    = StaffNothing
    | StaffClef Clef
    | StaffKeySignature KeySignature 
    | StaffTimeSignature TimeSignature 
    | StaffBarLine 
    | StaffDoubleBarLine 
    | StaffThickBarLine 
    | StaffShortBarLine 
    | StaffTickBarLine 
    | StaffFinalBarLine 
    | StaffSustainLine SustainLine
    | StaffCesura
    | StaffChord Chord
    deriving (Eq, Show)

-- | Nonspaced staff objects, placed in relation to one or more spaced objects. 
data NonSpacedObject 
    = StaffBeams Beams
    | StaffTremoloBeams TremoloBeams 
    | StaffTie Direction
    | StaffSlur Direction 
    | StaffTupletBracket Direction
    | StaffMetronomeMark NoteValue BeatsPerMinute
    | StaffDynamic Dynamic
    | StaffInstruction Instruction
    | StaffExpression Expression
    deriving (Eq, Show)

data StaffOptions = 
    StaffOptions { spaceBefore :: Spaces,
                   spaceAfter  :: Spaces,
                   staffLines  :: StaffLines }
    deriving (Eq, Show)

instance Trivial StaffOptions where
    trivial = StaffOptions 0 0 5

instance Monoid StaffOptions where
    mempty = trivial
    x `mappend` y = StaffOptions (spaceBefore x) (spaceAfter y) (staffLines x `max` staffLines y)
    
data Staff = 
    Staff { staffOptions     :: StaffOptions,
            spacedObjects    :: [(Spaces, SpacedObject)],
            nonSpacedObjects :: [([Index [SpacedObject]], NonSpacedObject)] }
    deriving (Eq, Show)

-- | The trivial instance for staff has no objects and no extra space.
instance Trivial Staff where
    trivial = Staff trivial [] []

-- | The empty element is the 'trivial' staff.
--
--   The binary operation suporimposes the objects on two staves. The position of spaced objects are
--   not affected (in particular, they are not stacked horizontally), but the indices for non-spaced
--   objects are adjusted to refer to the same objects. 
instance Monoid Staff where
    mempty = trivial
    Staff ox xs xns `mappend` Staff oy ys yns = 
        Staff (ox `mappend` oy) (xs ++ ys) (xns ++ inc (length xs) yns)
        where   
            inc n = fmap (inc' n)
            inc' n (is, x) = (map (+ n) is, x)

-- | The width of the staff, defined as the position of its rightmost elements
--   plus any extra space before and after.
staffWidth :: Staff -> Spaces
staffWidth (Staff o s _) = spaceBefore o + w + spaceAfter o
    where
        w = maximumWith 0 . fmap fst $ s

-- | Move objects on the staff to the right.
moveObjectsRight :: Spaces -> Staff -> Staff
moveObjectsRight n (Staff o s ns) = Staff o (map (\(p, x) -> (p + n, x)) s) ns

-- | Move objects on the staff to the left.
moveObjectsLeft :: Spaces -> Staff -> Staff
moveObjectsLeft n = moveObjectsRight (negate n)

-- | Add extra space to the start of the staff.
addSpaceAtStart :: Spaces -> Staff -> Staff
addSpaceAtStart x = addSpace x 0

-- | Add extra space to the end of the staff.
addSpaceAtEnd :: Spaces -> Staff -> Staff
addSpaceAtEnd x = addSpace 0 x

-- | Add extra space to the start and end of the staff.
addSpace :: Spaces -> Spaces -> Staff -> Staff
addSpace x y (Staff o s ns) = Staff (f o) s ns
    where
        f (StaffOptions b a l) = StaffOptions (b + x) (a + y) l

-- | Scapes the given staff by stretching its objects.
scaleStaff :: Spaces -> Staff -> Staff
scaleStaff x (Staff o s ns) = Staff o (fmap (mapFirst (* x)) s) ns

scaleStaffTo :: Spaces -> Staff -> Staff
scaleStaffTo x staff@(Staff o s ns) = scaleStaff x' staff
    where
       x' =  ((x - a) / (staffWidth staff - a))
       a  =  spaceBefore o + spaceAfter o

-- | Assure staves have the same width by adding space at end.
justifyStaves :: [Staff] -> [Staff]
justifyStaves ss = map (\s -> addSpaceAtEnd (((m - staffWidth s) / 2) `max` 0) s) $ ss
    where
        m = maximum . map staffWidth $ ss

-- | Split a staff at the given position.
splitStaff :: Spaces -> Staff -> (Staff, Staff)
splitStaff x (Staff o s ns) = (sx, sy)
    where
        sx = (Staff ox sx nsx)
        sy = (Staff oy sy nsy)

        (ox, oy) = (o, o)

        ns' = sort ns
        

-- | Split a staff right before the first spaced object that satisfies the predicate.
splitStaffWhen :: (Spaces -> SpacedObject -> Bool) -> (Staff, Staff)
splitStaffWhen = undefined

engraveSpacedObject :: SpacedObject -> Engraving
engraveSpacedObject StaffNothing         =  mempty
engraveSpacedObject (StaffClef x)        =  engraveClef x
engraveSpacedObject (StaffChord x)       =  engraveChord x
engraveSpacedObject StaffBarLine         =  singleBarLine
engraveSpacedObject StaffDoubleBarLine   =  doubleBarLine
engraveSpacedObject StaffThickBarLine    =  thickBarLine
engraveSpacedObject StaffShortBarLine    =  shortBarLine
engraveSpacedObject StaffTickBarLine     =  tickBarLine
engraveSpacedObject StaffFinalBarLine    =  finalBarLine
engraveSpacedObject (StaffSustainLine x) =  engraveSustainLine x

engraveNonSpacedObject :: NonSpacedObject -> Engraving
engraveNonSpacedObject (StaffMetronomeMark nv bpm) = engraveMetronomeMark nv bpm
engraveNonSpacedObject (StaffDynamic x)            = engraveDynamic x
engraveNonSpacedObject (StaffInstruction x)        = engraveInstruction x
engraveNonSpacedObject (StaffExpression x)         = engraveExpression x

engraveStaff :: Staff -> Engraving
engraveStaff staff@(Staff opt sN nsN) = mempty
    <> (translateX spb $ sE <> nsE)
    <> (alignL . scaleX ({-width (sE <> nsE)-}(convert . staffWidth) staff + spb + spa) $ noteLines)
    where 
        spb = convert $ spaceBefore opt
        spa = convert $ spaceAfter opt
        sE  = mconcat $ fmap (\(p, x) -> moveSpacesRight p $ engraveSpacedObject x) sN
        nsE = mconcat $ fmap (\(i:is, x) -> moveSpacesRight (fst $ index i sN) $ engraveNonSpacedObject x) nsN

