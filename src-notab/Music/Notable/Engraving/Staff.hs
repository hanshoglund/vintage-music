
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
    StaffOptions(..),
    SpacedObject(..),
    NonSpacedObject(..),
    Staff(..),

-- ** Predicates
    isStaffEmpty,

-- ** Spacing
    staffWidth,
    addSpaceAtStart,
    addSpaceAtEnd,
    addSpace,
    scaleStaff,
    scaleStaffTo,
    justifyStaves,

-- ** Objects
    spacedObjectWidth,
    nonSpacedObjectWidth,
    moveObjectsRight,
    moveObjectsLeft,

-- ** Splitting
    splitStaff,
    splitStaffWhen,
    divideStaff,

-- ** Engraving
    engraveStaff,
)

where

import Data.Convert
import Data.Index
import Data.Ord
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

kRehearsalOffset     :: Spaces
kMetronomeMarkOffset :: Spaces
kInstructionOffset   :: Spaces
kExpressionOffset    :: Spaces
kDynamicOffset       :: Spaces
kRehearsalOffset     = 8.5
kMetronomeMarkOffset = 4.5
kInstructionOffset   = 4.5
kExpressionOffset    = 5.5
kDynamicOffset       = 5.5

kRehearsalSquare     :: Spaces
kRehearsalSquare     = 3.5

kRehearsalScale      :: Double
kMetronomeMarkScale  :: Double
kInstructionScale    :: Double
kExpressionScale     :: Double
kDynamicScale        :: Double
kRehearsalScale      = 0.8
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
noteLines' num = position $ foldr above mempty (replicate num noteLineE)
    where
        noteLineE  =  s $ hrule 1 <> strutY (convert space) where { s = lineWidth kNoteLineWeight }
        position   =  moveSpacesUp $ (fromIntegral num - 1) / 2



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

-- TODO c, alla breve, compound time
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
-- Rehearsal marks
--

type Rehearsal = String

engraveRehearsal :: Rehearsal -> Engraving
engraveRehearsal str = p . s $ text str <> square (convert kRehearsalSquare)
    where
        s = font {-textFont-}"Arial" . bold
        p = moveSpacesUp kRehearsalOffset . scale kRehearsalScale



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
engraveMetronomeMark nv tempo = p $ mempty
    <> (engraveNote up 0 nh `leftTo` engraveText text)
    <> (translate (r2 (-0.8, 0.4)) . alignL $ spaceRect 4.2 1.5)
    where
        nh    =  noteHeadFromNoteValue nv
        text  =  " = " ++ show tempo
        p     =  moveSpacesUp kMetronomeMarkOffset . scale kMetronomeMarkScale



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

engraveInstruction :: Instruction -> Engraving
engraveInstruction txt = t $ mempty
    <> engraveText txt
    <> (translate (r2 (-0.2, 0.2)) . alignL $ spaceRect (0.2 + 0.65 * fromIntegral $ length txt) 1.5)
    where
        t = moveSpacesUp kInstructionOffset . scale kInstructionScale


type Expression = String

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
    | StaffBarLine
    | StaffDoubleBarLine
    | StaffThickBarLine
    | StaffShortBarLine
    | StaffTickBarLine
    | StaffFinalBarLine
    | StaffSustainLine SustainLine
    | StaffClef Clef
    | StaffKeySignature KeySignature
    | StaffTimeSignature TimeSignature
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
    | StaffRehearsal Rehearsal
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


-- | Whether a staff has any objects or not.
isStaffEmpty :: Staff -> Bool
isStaffEmpty (Staff o s ns) = null s



insertSpacedObject :: Spaces -> SpacedObject -> Staff -> Staff
insertSpacedObject t x (Staff o s ns) = (Staff o s' ns')
    where
        n   = insertIndexBy (comparing fst) (t, x) s
        s'  = insertBy      (comparing fst) (t, x) s
        ns' = map (\(is,x) -> (map (\i -> if i < n then i else i + 1) is, x)) ns



-- | The width of the staff, defined as the position of its rightmost elements
--   plus any extra space before and after.
staffWidth :: Staff -> Spaces
staffWidth (Staff o s _) = a + w
    where
        w = maximumWith 0 . fmap fst $ s
        a = spaceBefore o + spaceAfter o

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

-- | Scale a staff by stretching its objects.
scaleStaff :: Spaces -> Staff -> Staff
scaleStaff x (Staff o s ns) = Staff o (fmap (mapFirst (* x)) s) ns

-- | Scale a staff to the given width by stretching its objects.
scaleStaffTo :: Spaces -> Staff -> Staff
scaleStaffTo x staff@(Staff o s ns) = scaleStaff x' staff
    where
       x' =  ((x - a) / (staffWidth staff - a))
       a  =  spaceBefore o + spaceAfter o

-- | Assure staves have the same width by adding space at end.
justifyStaves :: [Staff] -> [Staff]
justifyStaves ss = map (\s -> addSpaceAtEnd ((m - staffWidth s) `max` 0) s) $ ss
    where
        m = maximum . map staffWidth $ ss



-- | The width of a spaced object. This is used to detect objects spanning line breaks.
spacedObjectWidth :: SpacedObject -> Spaces
spacedObjectWidth (StaffSustainLine (p, w)) = w
spacedObjectWidth _ = 0

-- | The width of a non-spaced object. This is used to detect objects spanning line breaks.
nonSpacedObjectWidth :: NonSpacedObject -> Spaces
nonSpacedObjectWidth _ = 0

-- | Move objects on the staff to the right.
moveObjectsRight :: Spaces -> Staff -> Staff
moveObjectsRight t (Staff o s ns) = Staff o (map (\(p, x) -> (p + t, x)) s) ns

-- | Move objects on the staff to the left.
moveObjectsLeft :: Spaces -> Staff -> Staff
moveObjectsLeft n = moveObjectsRight (negate n)

move t = map (\(p, x) -> (p + t, x)) -- compare moveObjectsLeft, moveObjectsRight





-- | Split a staff at the given position.
splitStaff :: Spaces -> Staff -> (Staff, Staff)
splitStaff t = splitStaff' t . cutStaffObjects t

-- | Split a staff right before the first spaced object that satisfies the predicate.
splitStaffWhen :: (Spaces -> SpacedObject -> Bool) -> (Staff, Staff)
splitStaffWhen = undefined

-- | Split a staff at the given position.
--
--   This is the primitive splitting function, which does not perform any cutting, so
--   objects may be left dangling to the right of the first staff.
--
splitStaff' :: Spaces -> Staff -> (Staff, Staff)
splitStaff' t (Staff o s ns) = ((Staff ox sx nsx), (Staff oy sy' nsy'))
    where
        (ox, oy)   = splitStaffOptions o
        (sx, sy)   = splitSpacedStaffObjects t s
        (nsx, nsy) = splitNonSpacedStaffObjects (length sx) ns
        sy'        = move (negate t) sy
        nsy'       = map (\(is, x) -> (fmap (subtract (length sx)) is, x)) nsy

splitStaffOptions :: StaffOptions -> (StaffOptions, StaffOptions)
splitStaffOptions o = (o, o)

splitSpacedStaffObjects :: Spaces -> [(Spaces, SpacedObject)] -> ([(Spaces, SpacedObject)], [(Spaces, SpacedObject)])
splitSpacedStaffObjects t =
    span (\(p, _) -> p < t)

splitNonSpacedStaffObjects :: Int -> [([Index [SpacedObject]], NonSpacedObject)] -> ([([Index [SpacedObject]], NonSpacedObject)], [([Index [SpacedObject]], NonSpacedObject)])
splitNonSpacedStaffObjects n =
    span (\(i, _) -> head i < n)


-- Prepare a staff for splitting by cutting its objects, i.e. divide slurs, ties, sustain lines
-- Precond: assumes s and ns sorted
cutStaffObjects :: Spaces -> Staff -> Staff
cutStaffObjects t (Staff o s ns) = insertSpacedObjects ins $ Staff o short ns
    where
        (short, ins) = mapCollect (cutStaffObject t) s

insertSpacedObjects :: [(Spaces, SpacedObject)] -> Staff -> Staff
insertSpacedObjects = composeAll . map (uncurry insertSpacedObject)

cutStaffObject :: Spaces -> (Spaces, SpacedObject) -> ((Spaces, SpacedObject), Maybe (Spaces, SpacedObject))
cutStaffObject t (p, x)
    | spans t (p, x) && cuttable x =
        let t'     = t - p
            (a, b) = cut t' x  in   ((p, a), Just (t, b))
    | otherwise                   = ((p, x), Nothing)

spans :: Spaces -> (Spaces, SpacedObject) -> Bool
spans t (p, x) = inRange (p, p + spacedObjectWidth x) t

cuttable :: SpacedObject -> Bool
cuttable (StaffSustainLine _) = True
cuttabel _                    = False

cut :: Spaces -> SpacedObject -> (SpacedObject, SpacedObject)
cut t (StaffSustainLine (p, w)) = (StaffSustainLine (p, t), StaffSustainLine (p, w - t))
cut t x = error "cutSpacedStaffObject: Not able to cut this object"

-- | Divide a staff into staves of the given length.
divideStaff :: Spaces -> Staff -> [Staff]
divideStaff t = justifyStaves . unfoldr f
    where
        f s | isStaffEmpty s = Nothing
            | otherwise      = Just $ splitStaff t s




--
-- Engraving
--

engraveSpacedObject :: SpacedObject -> Engraving
engraveSpacedObject StaffNothing         =  mempty
engraveSpacedObject StaffBarLine         =  singleBarLine
engraveSpacedObject StaffDoubleBarLine   =  doubleBarLine
engraveSpacedObject StaffThickBarLine    =  thickBarLine
engraveSpacedObject StaffShortBarLine    =  shortBarLine
engraveSpacedObject StaffTickBarLine     =  tickBarLine
engraveSpacedObject StaffFinalBarLine    =  finalBarLine
engraveSpacedObject (StaffSustainLine x) =  engraveSustainLine x
engraveSpacedObject (StaffClef x)        =  engraveClef x
engraveSpacedObject (StaffChord x)       =  engraveChord x

engraveNonSpacedObject :: NonSpacedObject -> Engraving
engraveNonSpacedObject (StaffRehearsal x)          =  engraveRehearsal x
engraveNonSpacedObject (StaffMetronomeMark nv bpm) =  engraveMetronomeMark nv bpm
engraveNonSpacedObject (StaffDynamic x)            =  engraveDynamic x
engraveNonSpacedObject (StaffInstruction x)        =  engraveInstruction x
engraveNonSpacedObject (StaffExpression x)         =  engraveExpression x

-- | Engraves the given staff.
--   The origin will be at the left edge on the middle line or space.
engraveStaff :: Staff -> Engraving
engraveStaff staff@(Staff opt sN nsN) = mempty
    <> (translateX spb $ sE <> nsE)
    <> (alignL . scaleX w $ noteLines)
    where
        spb  =  convert $ spaceBefore opt
--        spa = convert $ spaceAfter opt
        -- w = width (sE <> nsE) + spb + spa
        w    =  convert . staffWidth $ staff

        sE   =  mconcat $ fmap (\(p, x) -> moveSpacesRight p $ engraveSpacedObject x) sN
        nsE  =  mconcat $ fmap (\(i:is, x) -> moveSpacesRight (fst $ index i sN) $ engraveNonSpacedObject x) nsN

