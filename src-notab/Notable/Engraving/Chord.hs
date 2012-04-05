
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleContexts #-}

-- | Low-level engraving of rests and notes, along with associated objects such as accidentals, dots,
--   stems, flags, articulations and so on. Note that the term chord is used to indicate a set of
--   note heads and associated objects, which is referred to as a rest if empty.
--
--   Objects that span multiple chords, including beams, ties and slurs are engraved separately, but this
--   module provides anchor points that indicate how to connect such objects to chords.
--
module Notable.Engraving.Chord
(
-- * Basic components
-- ** Rests
    Rest(..),

-- ** Note heads
    NoteHead(..),
    hasStem,

-- *** Position
    NoteHeadPosition,
    separateNoteHeads,
    partitionNoteHeads,

-- ** Stems
-- *** Size
    AdjustStem,
-- *** Direction
    StemType,
    stemUp,
    stemDown,
    stemFlip,
    stemDirection,

-- *** Flags
    Flags,
-- *** Cross beams
    CrossBeams,

-- ** Dots
    Dots,

-- ** From note values
    noteHeadFromNoteValue,
    flagsFromNoteValue,
    restFromNoteValue,
    dotsFromNoteValue,
    fromNoteValue,

-- * Additional components
-- ** Accidentals
    Accidental(..),
    minSpaceAbove,
    minSpaceBelow,

-- ** Articulation
    Articulation(..),
    alwaysAbove,

-- ** Vertical lines
    VerticalLine(..),

-- * Ledger lines
    LongShortOffset,
    LedgerLinesAbove,
    LedgerLinesBelow,
    LedgerLines(..),
    ledgerLines,
    ledgerLines',
    engraveLedgerLines,

-- * Chords
    Stem(..),
    Note(..),
    Chord(..),
    engraveNote,
    engraveRest,
    engraveChord,

-- ** Positions
    notePositions,
    highestNotePosition,
    lowestNotePosition,
    stemRootPosition,
    stemTopPosition,

    slurAboveAnchor,
    slurBelowAnchor,
    tieAboveAnchor,
    tieBelowAnchor,
    tupletAboveAnchor,
    tupletBelowAnchor,
    glissandoBeforeAnchor,
    glissandoAfterAnchor
)
where

import Data.Tuple ( swap )
import qualified Data.List

import Music.Util
import Music.Util.List

import Notable.Core
import Notable.Core.Diagrams
import Notable.Core.Symbols


--
-- Constants
--

noteStemWidth :: Double
noteStemWidth = 0.025

noteStemInset :: Double
noteStemInset = 0.013

noteStemShortenAtOuterNote :: Double
noteStemShortenAtOuterNote = 0.1 * space

-- | Thickness of ledger lines.
ledgerLineWeight :: Double
ledgerLineWeight = 0.035



--
-- Rests
--

-- | Represents a rest.
--   Typically, a rest is engraved when the set of notes in a chord is empty.
data Rest
    = BrevisNoteHeadNoteRest
    | WholeNoteHeadNoteRest
    | HalfNoteRest
    | QuarterNoteRest
    | EightNoteRest
    | SixteenthNoteRest
    | ThirtySecondNoteRest
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | (internal)
restFromIndex :: Int -> Rest
restFromIndex x = toEnum (x + 1)

instance Symbolic Rest where
    symbol WholeNoteHeadNoteRest  =  (baseMusicFont, "\183")
    symbol HalfNoteRest           =  (baseMusicFont, "\238")
    symbol QuarterNoteRest        =  (baseMusicFont, "\206")
    symbol EightNoteRest          =  (baseMusicFont, "\228")
    symbol SixteenthNoteRest      =  (baseMusicFont, "\197")
    symbol ThirtySecondNoteRest   =  (baseMusicFont, "\168")

--
-- Notes
--

-- | Represents a note head.
--   Typically one note head is engraved for each note in a chord.
data NoteHead
    = BrevisNoteHead
    | WholeNoteHead
    | UnfilledNoteHead
    | FilledNoteHead
    | DiamondNoteHead
    | SquareNoteHead
    | CrossNoteHead
    deriving (Show, Eq)

instance Symbolic NoteHead where
    symbol BrevisNoteHead    =  (baseMusicFont, "W")
    symbol WholeNoteHead     =  (baseMusicFont, "w")
    symbol UnfilledNoteHead  =  (specialMusicFont, "F")
    symbol FilledNoteHead    =  (specialMusicFont, "f")


-- | Position of a note head, offset from middle line.
type NoteHeadPosition = HalfSpaces

-- (internal)
noteHeadFromIndex :: Int -> NoteHead
noteHeadFromIndex x
    | x  <= -1   =  BrevisNoteHead
    | x  <=  0   =  WholeNoteHead
    | x  <=  1   =  UnfilledNoteHead
    | otherwise  =  FilledNoteHead

-- | Whether a given notehead should be engraved with a stem or not.
hasStem :: NoteHead -> Bool
hasStem BrevisNoteHead    =  False
hasStem WholeNoteHead     =  False
hasStem UnfilledNoteHead  =  True
hasStem FilledNoteHead    =  True


-- | Separates note heads to be engraved to the left and right of the stem respectively.
separateNoteHeads :: Direction -> [NoteHeadPosition] -> ([NoteHeadPosition], [NoteHeadPosition])
separateNoteHeads d = separateNoteHeads' d . assertNoPrimes . Data.List.sort
    where
        -- Sanity check as we do not handle primes yet
        assertNoPrimes xs
            | null (filter2 (==) xs)  =  xs
            | otherwise               =  error "assertNoPrimes"

        separateNoteHeads' direction positions
            | direction  =  ( lowers `merge` others, uppers )
            | otherwise  =  ( lowers, others `merge` uppers )
                where
                    (pairs, others)  =  partitionNoteHeads direction positions
                    (lowers, uppers) =  unzip pairs

-- | Separate small intervals (i.e. primes and seconds) from others.
--   Such intervals must be engraved on opposite sides of the stem to avoid collisions.
partitionNoteHeads :: Direction -> [NoteHeadPosition] -> ([(NoteHeadPosition, NoteHeadPosition)], [NoteHeadPosition])
partitionNoteHeads direction positions =
    partitioner collides positions
    where
        collides x y     =  y - x < 2
        partitioner
            | direction  =  partition2
            | otherwise  =  reversePartition2



--
-- Stems
--

-- | Whether the stem should be flipped.
type StemType = (Direction -> Direction)

-- | Always use an upward stem.
stemUp :: StemType
stemUp = const upwards

-- | Always use a downward stem.
stemDown :: StemType
stemDown = const downwards

-- | Flip the default stem direction.
stemFlip :: StemType
stemFlip = not

-- | Amount to add to stem length. May be negative.
type AdjustStem = Double

-- | Returns the default direction for the given note heads.
stemDirection :: [NoteHeadPosition] -> Direction
stemDirection x = upwards
-- TODO see Tyboni


-- | Number of flags on a stem.
--   This is the number of flags to actually engrave, irrespective of beams and crossbeams.
type Flags = Int

-- | (internal)
flagsFromIndex :: Int -> Flags
flagsFromIndex x = max 0 (x - 2)


-- | Number of crossbeams.
--   This is the number of crossbeams to actually engrave, irrespective of flags and beams.
type CrossBeams = Int

--
-- Dots
--

-- | Number of dots.
type Dots = Int


-- | Returns the rest typically used to indicate the given note value.
restFromNoteValue :: NoteValue -> Rest
restFromNoteValue x = r where (r, h, f, d) = fromNoteValue x

-- | Returns the note head typically used to indicate the given note value.
noteHeadFromNoteValue :: NoteValue -> NoteHead
noteHeadFromNoteValue x = h where (r, h, f, d) = fromNoteValue x

-- | Returns the number of flags typically used to indicate the given note value.
flagsFromNoteValue :: NoteValue -> Flags
flagsFromNoteValue x = f where (r, h, f, d) = fromNoteValue x

-- | Returns the number of dotrs typically used to indicate the given note value.
dotsFromNoteValue :: NoteValue -> Dots
dotsFromNoteValue x = d where (r, h, f, d) = fromNoteValue x

-- | Returns the rest, note head, number of flags and number of dots typically used to indicate
--   the given note value.
fromNoteValue :: NoteValue -> (Rest, NoteHead, Flags, Dots)
fromNoteValue x = (restFromIndex val, noteHeadFromIndex val, flagsFromIndex val, dots')
    where
        (val, dots) = properFraction . negate . logBase 2 $ x
        dots' = case dots of
            0.0 -> 0
            _   -> 1
-- TODO something more sophisticated for multiple dots etc.




--
-- Accidentals
--

data Accidental
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Symbolic Accidental where
    symbol DoubleFlat   =  (baseMusicFont, "\186")
    symbol Flat         =  (baseMusicFont, "b")
    symbol Natural      =  (baseMusicFont, "n")
    symbol Sharp        =  (baseMusicFont, "#")
    symbol DoubleSharp  =  (baseMusicFont, "\192")

-- | Minimum space required above the given accidental (Tyboni p 24).
minSpaceAbove :: Accidental -> HalfSpaces
minSpaceAbove DoubleFlat   =  2
minSpaceAbove Flat         =  2
minSpaceAbove Natural      =  3
minSpaceAbove Sharp        =  3
minSpaceAbove DoubleSharp  =  2

-- | Minimum space required below the given accidental (Tyboni p 24).
minSpaceBelow :: Accidental -> HalfSpaces
minSpaceBelow DoubleFlat   =  3
minSpaceBelow Flat         =  3
minSpaceBelow Natural      =  3
minSpaceBelow Sharp        =  3
minSpaceBelow DoubleSharp  =  2

--
-- Articulation
--

-- | Articulation mark.
--
--   These are always drawn in a particular order, represented by the 'Enum' and 'Bounded' instances.
--   The 'minBound' value should be closest to the chord and the 'maxBound' value farthest away from
--   the chord.
data Articulation
    = Fermata
    | Downbow
    | Upbow
    | Plus
    | Circle
    | Marcato
    | Accent
    | Tenuto
    | MoltoStaccato
    | Staccato
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Symbolic Articulation where
    symbol Fermata   =   (baseMusicFont, "u")
    symbol Plus      =   (baseMusicFont, "+")
    symbol Circle    =   (baseMusicFont, "o")
    symbol Marcato   =   (baseMusicFont, "v")
    symbol Accent    =   (baseMusicFont, ">")
    symbol Tenuto    =   (baseMusicFont, "-")
    symbol Staccato  =   (baseMusicFont, ".")


-- | Whether the accidental should always be drawn above the chord.
alwaysAbove :: Articulation -> Bool
alwaysAbove Fermata   =   True
alwaysAbove Plus      =   True
alwaysAbove Circle    =   True
alwaysAbove Marcato   =   True
alwaysAbove Accent    =   False
alwaysAbove Tenuto    =   False
alwaysAbove Staccato  =   False


--
-- Vertical lines
--

data VerticalLine
    = Arpeggio
    deriving (Eq, Show)


--
-- Ledger lines
--

-- | Number of long and short ledger lines, and their offset from the middle line.
--
--   Short ledger lines is used for single notes, while longer lines are used for primes and seconds.
type LongShortOffset = (Int, Int, HalfSpaces)

type LedgerLinesAbove = LongShortOffset
type LedgerLinesBelow = LongShortOffset

-- | A description of ledger lines to engrave for a particular chord.
newtype LedgerLines = LedgerLines { getLedgerLines :: (LedgerLinesAbove, LedgerLinesBelow) }
    deriving (Eq, Show)

-- | Monoid over ledger lines. This can be used to join ledger lines required by different chords.
--
--   Note that `mappend` raises an error if called on ledger lines with different offsets, as joining
--   ledger lines from different types of staves makes no sense.
instance Monoid LedgerLines where
    mempty  =  LedgerLines (none, none) where none = (0, 0, 0)
    LedgerLines x `mappend` LedgerLines y  =  LedgerLines $ prod2 apLines apLines x y
        where
            apLines         =  prod3 max max apOffset
            x `apOffset` y  =  if (x == y) then x else error "LedgerLines.mappend: unequal offset"


-- | Returns the ledger lines required by a given set of note heads. The stem direction is necessary to
--   determine the placement of second and prime intervals, which in turn influences the length of lines.
ledgerLines :: Direction -> [NoteHeadPosition] -> LedgerLines
ledgerLines = ledgerLines' 5

-- | Returns the ledger lines required by a given set of note heads. This version works for any
--   number of staff lines (but see note about `mappend` above).
ledgerLines' :: StaffLines -> Direction -> [NoteHeadPosition] -> LedgerLines
ledgerLines' s d p = LedgerLines $ (ledgerLinesAbove s d p, ledgerLinesBelow s d p)

ledgerLinesAbove staffLines direction positions =
    (0, shortAbove, firstAbove)
    where
        shortAbove  =
            truncate
                . maybe 0 (\p -> (p - firstAbove + 2) / 2)
                . fmap maximum . nonEmpty
                . filter (> firstAbove) $ positions
        firstAbove   =  fromIntegral staffLines + 1

ledgerLinesBelow staffLines direction positions =
    (0, shortBelow, firstBelow)
    where
        shortBelow  =
            truncate
                . maybe 0 (negate . \p -> (p - firstBelow - 2) / 2)
                . fmap minimum . nonEmpty
                . filter (< firstBelow) $ positions
        firstBelow  =  (negate . fromIntegral $ staffLines) - 1

-- | Engraves ledger lines, with the origin in at the default note column in the middle line
engraveLedgerLines :: LedgerLines -> Engraving
engraveLedgerLines ( LedgerLines ( (longAbove, shortAbove, offsetAbove),
                                   (longBelow, shortBelow, offsetBelow)) ) =
    aboveE <> belowE
    where
        aboveE   = moveHalfSpacesUp offsetAbove . catUp   . replicate shortAbove $ ledgerE
        belowE   = moveHalfSpacesUp offsetBelow . catDown . replicate shortBelow $ ledgerE

        ledgerE  =  lineE <> spaceE
        lineE    =  style $ hrule width where { style = lineWidth ledgerLineWeight }
        spaceE   =  spaceRect width space

        width    =  2 * space


--
-- Chords
--

data Stem =
    Stem { stemType   :: StemType,
           adjustStem :: AdjustStem,
           flags      :: Flags,
           crossBeams :: CrossBeams }

data Note =
    Note { noteHeadPosition :: NoteHeadPosition,
           noteHead         :: NoteHead,
           accidental       :: Accidental }

data Chord =
    Chord { notes :: [Note],
            rest  :: Rest,
            dots  :: Dots,
            stem  :: Stem,
            articulations :: [Articulation],
            verticalLines :: [VerticalLine] }

-- TODO reimplement in terms of engraveChord

-- | Engraves a single note.
engraveNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
engraveNote pos dir nh =
    moveHalfSpacesUp pos $ headE <> stemE <> spaceE
    where
        spaceE  =  spaceRect (fst . unr2 $ noteHeadOffset) space
        headE   =  position $ engraveSymbolFloating (symbol nh)
            where { position = translate (0.5 *^ noteHeadOffset) }

        stemE   =  if (hasStem nh) then stemE' else mempty
        stemE'  =  moveOriginBy noteStemOffset . style $ rect noteStemWidth noteStemHeight
            where { style = lineColor black . fillColor black }

        noteStemHeight  =  space * 3.5 - noteStemShortenAtOuterNote

        noteHeadOffset  =  symbolSpacer (symbol nh)
        noteStemOffset  =  r2 $ (negate `onlyIf` (const dir)) (- (getX $ noteHeadOffset / 2) - noteStemInset,
                                                               space * 3.5 / 2 + (noteStemShortenAtOuterNote / 2))

-- | Engraves a rest.
engraveRest :: Rest -> Engraving
engraveRest = undefined

-- | Engraves a chord. The origin will be at the middle line, at the standard note column.
engraveChord :: Chord -> Engraving
engraveChord = undefined


notePositions :: Chord -> [R2]
notePositions = undefined

highestNotePosition :: Chord -> R2
highestNotePosition = last . notePositions

lowestNotePosition :: Chord -> R2
lowestNotePosition = head . notePositions

stemRootPosition :: Chord -> R2
stemRootPosition = undefined

stemTopPosition :: Chord -> R2
stemTopPosition = undefined

slurAboveAnchor :: Chord -> R2
slurAboveAnchor = undefined

slurBelowAnchor :: Chord -> R2
slurBelowAnchor = undefined

tieAboveAnchor :: Chord -> R2
tieAboveAnchor = undefined

tieBelowAnchor :: Chord -> R2
tieBelowAnchor = undefined

tupletAboveAnchor :: Chord -> R2
tupletAboveAnchor = undefined

tupletBelowAnchor :: Chord -> R2
tupletBelowAnchor = undefined

glissandoBeforeAnchor :: Chord -> R2
glissandoBeforeAnchor = undefined

glissandoAfterAnchor :: Chord -> R2
glissandoAfterAnchor = undefined



