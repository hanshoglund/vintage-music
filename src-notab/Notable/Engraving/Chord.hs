
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleContexts #-}

-- | This module provides engraving of rests and notes, along with associated objects such as accidentals,
--   dots, stems, flags, articulations and so on. Note that the term chord is used to indicate a set of note
--   heads and associated objects, which is referred to as a rest if empty.
--
--   Objects that span multiple chords, including beams, ties and slurs are engraved separately, but this
--   module provides anchor points that indicate how to connect such objects to chords.
--
module Notable.Engraving.Chord
(
-- * Basic components
-- ** Rests
    Rest(..),
    restSymbol,

-- ** Note heads
    NoteHead(..),
    NoteHeadPos,
    noteHeadSymbol,
    noteHeadNeedsStem,
    separeteNoteHeads,
    partitionSmallIntervals,

-- ** Stems
    FlipStem,
    stemUp,
    stemDown,
    stemFlip,
    AdjustStem,
    Flags,
    CrossBeams,

-- * Additional components
-- ** Dots
    Dots,

-- ** Accidentals
    Accidental(..),
    accidentalSymbol,

-- ** Articulation
    Articulation(..),
    articulationSymbol,

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
    engraveNote,
    engraveRest,
    engraveChord,
    engraveChord',

-- ** Positions
    notePositions,
    highestNotePosition,
    lowestNotePosition,
    stemRootPosition,
    stemTopPosition,
)
where

import Data.Tuple (swap)
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

data Rest
    = WholeNoteRest
    | HalfNoteRest
    | QuarterNoteRest
    | EightNoteRest
    | SixteenthNoteRest
    | ThirtySecondNoteRest
    deriving (Show, Eq, Ord, Enum)

-- | Returns the symbol for a given accidentalSymbol.
restSymbol :: Rest -> Symbol
restSymbol WholeNoteRest         =  (baseMusicFont, "\183")
restSymbol HalfNoteRest          =  (baseMusicFont, "\238")
restSymbol QuarterNoteRest       =  (baseMusicFont, "\206")
restSymbol EightNoteRest         =  (baseMusicFont, "\228")
restSymbol SixteenthNoteRest     =  (baseMusicFont, "\197")
restSymbol ThirtySecondNoteRest  =  (baseMusicFont, "\168")


--
-- Note heads
--

-- | Represents a note head symbol.
data NoteHead
    = Unfilled
    | Filled
    | Whole
    | Brevis
    | Diamond
    | Square
    | Cross
    deriving (Show, Eq)

-- | Position of a note head, offset from middle line.
type NoteHeadPos = HalfSpaces

-- | Returns the symbol for a given note head.
noteHeadSymbol :: NoteHead -> Symbol
noteHeadSymbol Filled    =  (specialMusicFont, "f")
noteHeadSymbol Unfilled  =  (specialMusicFont, "F")
noteHeadSymbol Whole     =  (baseMusicFont, "w")
noteHeadSymbol Brevis    =  (baseMusicFont, "W")

-- | Whether a given notehead should be engraved with a stem or not.
noteHeadNeedsStem :: NoteHead -> Bool
noteHeadNeedsStem Unfilled  =  True
noteHeadNeedsStem Filled    =  True
noteHeadNeedsStem Whole     =  False
noteHeadNeedsStem Brevis    =  False


-- | Separate small intervals from others.
--
partitionSmallIntervals :: Direction -> [NoteHeadPos] -> ([(NoteHeadPos, NoteHeadPos)], [NoteHeadPos])
partitionSmallIntervals direction positions =
    partitioner collides positions
    where
        collides x y     =  y - x < 2
        partitioner
            | direction  =  partition2
            | otherwise  =  reversePartition2

-- | Separates note heads to be engraved to the left and right of the stem respectively.
--
separeteNoteHeads :: Direction -> [NoteHeadPos] -> ([NoteHeadPos], [NoteHeadPos])
separeteNoteHeads d = separeteNoteHeads' d . assertNoPrimes . Data.List.sort
    where
        -- Sanity check as we do not handle primes yet
        assertNoPrimes xs
            | null (filter2 (==) xs)  =  xs
            | otherwise               =  error "assertNoPrimes"

        separeteNoteHeads' direction positions
            | direction  =  ( lowers `merge` others, uppers )
            | otherwise  =  ( lowers, others `merge` uppers )
                where
                    (pairs, others)  =  partitionSmallIntervals direction positions
                    (lowers, uppers) =  unzip pairs

--
-- Dots
--

-- | Number of dots.
type Dots = Int


--
-- Accidentals
--

data Accidental
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp
    deriving (Show, Eq)

-- | Returns the symbol for a given accidentalSymbol.
accidentalSymbol :: Accidental -> Symbol
accidentalSymbol DoubleFlat   =  (baseMusicFont, "\186")
accidentalSymbol Flat         =  (baseMusicFont, "b")
accidentalSymbol Natural      =  (baseMusicFont, "n")
accidentalSymbol Sharp        =  (baseMusicFont, "#")
accidentalSymbol DoubleSharp  =  (baseMusicFont, "\192")


--
-- Stems
--

-- | Whether the stem should be flipped.
type FlipStem = (Direction -> Direction)

stemUp   :: FlipStem
stemDown :: FlipStem
stemFlip :: FlipStem
stemUp   = const upwards
stemDown = const downwards
stemFlip = not

-- | Amount to add to stem length.
type AdjustStem = Double

-- | Number of flags.
type Flags = Int

--
-- Crossbeams
--

-- | Number of crossbeams.
type CrossBeams = Int


--
-- Articulation
--

data Articulation
    = Fermata
    | Staccato
    | Tenuto
    | Marcato
    | Accent
    | Circle
    | Plus
    deriving (Show, Eq, Ord, Enum)

-- | Returns the symbol for a given accidental.
articulationSymbol :: Articulation -> Symbol
articulationSymbol Fermata   =   (baseMusicFont, "u")
articulationSymbol Staccato  =   (baseMusicFont, ".")
articulationSymbol Tenuto    =   (baseMusicFont, "-")
articulationSymbol Marcato   =   (baseMusicFont, "v")
articulationSymbol Accent    =   (baseMusicFont, ">")
articulationSymbol Circle    =   (baseMusicFont, "o")
articulationSymbol Plus      =   (baseMusicFont, "+")


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
--   Short ledger lines is used for single notes, while longer lines are used for primes and seconds.
--
type LongShortOffset = (Int, Int, HalfSpaces)

type LedgerLinesAbove = LongShortOffset
type LedgerLinesBelow = LongShortOffset

-- | A description of ledger lines to engrave for a particular chord.
newtype LedgerLines = LedgerLines { getLedgerLines :: (LedgerLinesAbove, LedgerLinesBelow) }
    deriving (Eq, Show)

-- | Monoid over ledger lines. This can be used to join ledger lines required by different chords.
--
--   Note that `mappend` raises an error if called on ledger lines with different offsets, as joining ledger
--   lines from different types of staves makes no sense.
instance Monoid LedgerLines where
    mempty  =  LedgerLines (none, none) where none = (0, 0, 0)
    LedgerLines x `mappend` LedgerLines y  =  LedgerLines $ prod2 apLines apLines x y
        where
            apLines         =  prod3 max max apOffset
            x `apOffset` y  =  if (x == y) then x
                else error "LedgerLines.mappend: unequal offset"


-- | Returns the ledger lines required by a given set of note heads. The stem direction is necessary to
--   determine the placement of second and prime intervals, which in turn influences the length of lines.
ledgerLines :: Direction -> [NoteHeadPos] -> LedgerLines
ledgerLines = ledgerLines' 5

-- | Returns the ledger lines required by a given set of note heads. This version works for any number of
--   staff lines (but see note about `mappend` above).
ledgerLines' :: StaffLines -> Direction -> [NoteHeadPos] -> LedgerLines
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

-- | Engraves ledger lines, with the origin in at the default note column in the middle line.

engraveLedgerLines :: LedgerLines -> Engraving
engraveLedgerLines (LedgerLines ((la, sa, oa), (lb, sb, ob))) = 
    mempty
        <> (cat unitY          . replicate sa $ ledgerE) # translate (r2 (0, oa * halfSpace))
        <> (cat (negate unitY) . replicate sb $ ledgerE) # translate (r2 (0, ob * halfSpace))
    where
        ledgerE  =  lineE <> spaceE
        lineE    =  hrule width # lineWidth ledgerLineWeight
        spaceE   =  spaceRect width space
        width    =  2 * space          
        
        
--
-- Chords
--

-- TODO rewrite as part of engraveChord

-- | Engraves a single note.
engraveNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
engraveNote pos dir nh =
    moveToPosition pos $
        mempty
        <> headE
        <> stemE
        <> spaceE
    where
        spaceE  =  spaceRect (fst . unr2 $ noteHeadOffset) space
        headE   =  engraveSymbol (noteHeadSymbol nh) # translate (0.5 *^ noteHeadOffset)

        stemE   =  if (noteHeadNeedsStem nh) then stemE' else mempty
        stemE'  =  rect noteStemWidth noteStemHeight
                       # lc black
                       # fc black
                       # moveOriginBy noteStemOffset

        noteStemHeight  =  space * 3.5 - noteStemShortenAtOuterNote

        noteHeadOffset  =  symbolSpacer (noteHeadSymbol nh)
        noteStemOffset  =  r2 $ (negate `onlyIf` (const dir)) (- (fst . unr2 $ noteHeadOffset / 2) - noteStemInset,
                                                               space * 3.5 / 2 + (noteStemShortenAtOuterNote / 2))


-- | Engraves a rest.
engraveRest :: 
    Rest
    -> Engraving    
engraveRest = undefined           

-- | Engraves a chord. The origin will be at the middle line, at the default note column.
engraveChord :: 
    [(NoteHead, NoteHeadPos, Accidental)]
    -> Engraving    
engraveChord = undefined           

-- | Engraves a chord. The origin will be at the middle line, at the default note column.
engraveChord' :: 
    Rest
    -> [(NoteHead, NoteHeadPos, Accidental)]
    -> FlipStem 
    -> AdjustStem
    -> Flags
    -> CrossBeams
    -> Dots
    -> [Articulation]
    -> [VerticalLine]
    -> Engraving
engraveChord' = undefined


notePositions :: a -> [R2]
notePositions         = undefined

highestNotePosition :: a -> R2
highestNotePosition   = undefined

lowestNotePosition :: a -> R2
lowestNotePosition    = undefined

stemRootPosition :: a -> R2
stemRootPosition      = undefined

stemTopPosition :: a -> R2
stemTopPosition       = undefined
