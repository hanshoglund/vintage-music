
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleContexts #-}

-- | Low-level engraving of rests, notes, along with associated objects such as stems, dots, accidentals,
--   articulations and so on. Note that the term chord is used to indicate a set of note heads and
--   associated objects, which is referred to as a rest if empty.
--
--   Objects that span multiple chords, including beams, ties and slurs are engraved separately, but this
--   module provides anchor points that indicate how to connect such objects to chords.
--
module Music.Notable.Engraving.Chord
(
-- * Basic components

-- ** Rests
    Rest(..),
    engraveRest,

-- ** Note heads
    NoteHead(..),
    NoteHeadPosition,
    hasStem,
    engraveNoteHead,
    engraveLeftNoteHeadColumn,
    engraveRightNoteHeadColumn,
    separateNoteHeads,
    separateNoteHeadsWith,
    engraveNoteHeads,
    engraveRestOrNoteHeads,

-- ** Dots
    Dots,
    engraveDots,

-- ** Stems
    StemDirection(..),
    StemAdjustment,
    stemUp,
    stemDown,
    defaultStem,
    flipStem,
    stemLength,
    stemXOffset,
    stemYOffset,

-- *** Flags
    Flags,
    engraveFlags,

-- *** Cross beams
    CrossBeams,
    engraveCrossBeams,

-- *** Engraving
    engraveStem, 
    engraveStem',

-- ** Conversion from note values
    noteHeadFromNoteValue,
    flagsFromNoteValue,
    restFromNoteValue,
    dotsFromNoteValue,
    fromNoteValue,


-- * Additional components
-- ** Accidentals
    Accidental(..),
    AccidentalPosition(..),
    minSpaceAbove,
    minSpaceBelow,
    engraveAccidental,
    engraveAccidentals,

-- ** Articulation
    Articulation(..),
    alwaysAbove,
    engraveArticulation,
    engraveArticulations,

-- ** Vertical lines
    VerticalLine(..),
    engraveVerticalLines,

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
    -- splitNote,
    -- splitNotes,
    -- getNotePositions,
    -- getNoteHeads,
    -- getNoteAccidentals,

-- ** Positioning
    -- notePositions,
    -- highestNotePosition,
    -- lowestNotePosition,
    -- stemRootPosition,
    -- stemTopPosition,
    -- 
    -- slurAboveAnchor,
    -- slurBelowAnchor,
    -- tieAboveAnchor,
    -- tieBelowAnchor,
    -- tupletAboveAnchor,
    -- tupletBelowAnchor,
    -- glissandoBeforeAnchor,
    -- glissandoAfterAnchor,

-- ** Engraving
    engraveNote,
    engraveChord,
)
where

import Data.Convert
import Data.Ord ( comparing )
import Data.Trivial
import Data.Tuple ( swap )
import qualified Data.List

import Music.Util
import Music.Util.List

import Music.Notable.Core
import Music.Notable.Core.Symbols
import Music.Notable.Core.Diagrams


--
-- Constants
--

-- | Thickness of stems.
stemWeight :: Double
stemWeight = convert $ Spaces 0.12

-- | Move stem inwards by this amount.
stemInset :: Double
stemInset = convert $ Spaces 0.032

-- | Shorten stem at outer note by this amount.
stemShortenAtOuterNote :: Double
stemShortenAtOuterNote = convert $ Spaces 0.1

-- | Space between rightmost accidental column and leftmost note column.
accidentalOffset :: Double
accidentalOffset = convert $ Spaces 0.4

-- | Space between accidental columns.
accidentalColumnOffset :: Double
accidentalColumnOffset = convert $ Spaces 0.3

-- | Thickness of ledger lines.
ledgerLineWeight :: Double
ledgerLineWeight = convert $ Spaces 0.14



--
-- Rests
--

-- | Represents a rest.
--   Typically, a rest is engraved when the set of notes in a chord is empty.
data Rest
    = BrevisRest
    | WholeNoteRest
    | HalfNoteRest
    | QuarterNoteRest
    | EightNoteRest
    | SixteenthNoteRest
    | ThirtySecondNoteRest
    deriving (Eq, Show, Ord, Enum, Bounded)

-- | (internal)
restFromIndex :: Int -> Rest
restFromIndex x = toEnum (x + 1)

instance Symbolic Rest where
    symbol WholeNoteRest          =  (baseMusicFont, "\xd3")
    symbol HalfNoteRest           =  (baseMusicFont, "\x2211")
    symbol QuarterNoteRest        =  (baseMusicFont, "\x152")
    symbol EightNoteRest          =  (baseMusicFont, "\x2030")
    symbol SixteenthNoteRest      =  (baseMusicFont, "\x2248")
    symbol ThirtySecondNoteRest   =  (baseMusicFont, "\xae")

-- | Engraves a rest.
--   The local origin will to the left, at position zero.
engraveRest :: Rest -> Engraving
engraveRest = engraveSymbol . symbol


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
    | CrossNoteHead
    | CircledCrossNoteHead
    | UnfilledSquareNoteHead
    | FilledSquareNoteHead
    deriving (Eq, Show)

--   I don't want to make it an instance of Enum, as the index only make sense for "standard"
--   notes such as brevis, whole, filled etc.
noteHeadFromIndex x
    | x  <= -1   =  BrevisNoteHead
    | x  <=  0   =  WholeNoteHead
    | x  <=  1   =  UnfilledNoteHead
    | otherwise  =  FilledNoteHead

instance Symbolic NoteHead where
    symbol BrevisNoteHead          =  (baseMusicFont, "W")
    symbol WholeNoteHead           =  (baseMusicFont, "w")
    symbol UnfilledNoteHead        =  (specialMusicFont, "F")
    symbol FilledNoteHead          =  (specialMusicFont, "f")
    symbol DiamondNoteHead         =  (baseMusicFont, "O")
    symbol CrossNoteHead           =  (baseMusicFont, "\220")
    symbol CircledCrossNoteHead    =  (specialMusicFont, "\89") -- or 88 or 90
    symbol UnfilledSquareNoteHead  =  (specialMusicFont, "\41")
    symbol FilledSquareNoteHead    =  (specialMusicFont, "\54")


-- | Vertical position of a note head, offset from middle line.
type NoteHeadPosition = HalfSpaces

-- | Whether a given notehead should be engraved with a stem or not.
hasStem :: NoteHead -> Bool
hasStem BrevisNoteHead    =  False
hasStem WholeNoteHead     =  False
hasStem UnfilledNoteHead  =  True
hasStem FilledNoteHead    =  True



-- | Separate small intervals (i.e. primes and seconds) from others.
--   Such intervals must be engraved on opposite sides of the stem to avoid collisions.
partitionNoteHeads :: Direction -> [NoteHeadPosition] -> ([(NoteHeadPosition, NoteHeadPosition)], [NoteHeadPosition])
partitionNoteHeads stemDir positions =
    partitioner collides positions
    where
        collides x y     =  absDif x y < 2
        partitioner
            | isUp   stemDir  =  partition2
            | isDown stemDir  =  reversePartition2

-- | Separates note heads to be engraved to the left and right of the stem respectively.
separateNoteHeads :: Direction -> [NoteHeadPosition] -> ([NoteHeadPosition], [NoteHeadPosition])
separateNoteHeads d = separateNoteHeads' d . {-assertNoPrimes . -}Data.List.sort

-- Sanity check as we do not handle primes yet
assertNoPrimes xs
    | null (filter2 (==) xs)  =  xs
    | otherwise               =  error "assertNoPrimes"

separateNoteHeads' stemDir positions
    | isUp   stemDir  =  ( lowers `merge` others, uppers )
    | isDown stemDir  =  ( lowers, others `merge` uppers )
        where
            (pairs, others)  =  partitionNoteHeads stemDir positions
            (lowers, uppers) =  unzip pairs

-- | Like 'separateNoteHeads', but passing an arbitrary data type along.
separateNoteHeadsWith :: Direction -> [(NoteHeadPosition, b)] -> ([(NoteHeadPosition, b)], [(NoteHeadPosition, b)])
separateNoteHeadsWith stemDir noteHeads =
    mergeZip leftPoses heads rightPoses
    where
        noteHeads'  =  sortBy (comparing fst) noteHeads
        (poses, heads)           =  unzip noteHeads'
        (leftPoses, rightPoses)  =  separateNoteHeads stemDir poses


-- | Engraves a single note head.
--   The local origin will be at the left of the note at position zero.
engraveNoteHead :: NoteHeadPosition -> NoteHead -> Engraving
engraveNoteHead pos noteHead =
    moveHalfSpacesUp pos $ engraveSymbol (symbol noteHead)

-- | Engraves the given set of note heads in the left column.
engraveLeftNoteHeadColumn :: [(NoteHeadPosition, NoteHead)] -> Engraving
engraveLeftNoteHeadColumn = engraveNoteHeadColumn True

-- | Engraves the given set of note heads in the right column.
engraveRightNoteHeadColumn :: [(NoteHeadPosition, NoteHead)] -> Engraving
engraveRightNoteHeadColumn = engraveNoteHeadColumn False

engraveNoteHeadColumn isLeft notes =
    mconcat . fmap (\(p, n) -> align $ engraveNoteHead p n) $ notes
        where
            align = if isLeft then alignR else alignL

-- | Engraves the given set of note heads.
--
--   This function will partition the note heads into a main column and a side column, to avoid colliding
--   seconds and primes whenever possible. The side column is to the right if the stem direction is up, and to
--   the left if the stem direction is down. The local origin will be in the main column at position zero.
engraveNoteHeads :: Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveNoteHeads stemDir = snd . engraveNoteHeads' stemDir

-- Returns the x offset of the main column along with the engraving
-- This is required by stemXOffset
engraveNoteHeads' :: Direction -> [(NoteHeadPosition, NoteHead)] -> (Double, Engraving)
engraveNoteHeads' stemDir notes = (mainColumnXOffset, position $ leftColumn `leftTo` rightColumn)
    where
        (leftNotes, rightNotes)  =  separateNoteHeadsWith stemDir notes
        
        leftColumn   =  engraveLeftNoteHeadColumn leftNotes
        rightColumn  =  engraveRightNoteHeadColumn rightNotes
        mainColumn   =  if (isUp stemDir) then leftColumn else rightColumn

        mainColumnXOffset  =  negateIfDown stemDir (width mainColumn / 2 - stemInset)
        position           =  translate (r2 (mainColumnXOffset, 0))

-- | Like 'engraveRest' if the given set of notes is empty, or 'engraveNoteHeads' otherwise.
engraveRestOrNoteHeads :: Rest -> Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveRestOrNoteHeads rest stemDir noteHeads
    | null noteHeads  =  engraveRest rest
    | otherwise       =  engraveNoteHeads stemDir noteHeads


--
-- Dots
--

-- | Number of dots.
type Dots = Int

-- | Engraves the dots required for the given note heads.
--   The local origin will be in the middle, at position zero.
engraveDots :: Dots -> [NoteHeadPosition] -> Engraving
engraveDots = undefined -- TODO



--
-- Stems
--

-- TODO change to (Chord -> D -> D) and compose with defaultStem ??
newtype StemDirection = StemDirection { getStemDirection :: Direction -> Direction }
    deriving (Eq, Show)

-- | Always use an upward stem.
stemUp :: StemDirection
stemUp = StemDirection $ const up

-- | Always use a downward stem.
stemDown :: StemDirection
stemDown = StemDirection $ const down

-- | Use the default stem direction.
defaultStem :: StemDirection
defaultStem = StemDirection id

-- | Flip the default stem direction.
flipStem :: StemDirection
flipStem = StemDirection $ Direction . not . getDirection

stemLength :: [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemLength notes
    | length notes == 0  =  error "stemLength: Note list is empty"
    | otherwise          =  octave + (highest - lowest)
    where
        highest  =  maximum $ map (fst) notes
        lowest   =  minimum $ map (fst) notes

stemXOffset :: Direction -> [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemXOffset stemDir = convert . (subtract inset) . fst . engraveNoteHeads' stemDir
    where
        inset = negateIfDown stemDir stemInset

stemYOffset :: Direction -> [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemYOffset stemDir notes
    | length notes == 0  =  error "stemYOffset: Note list is empty"
    | otherwise          =  0 + (negateIfDown stemDir $ stemLength notes) / 2 + outerNote
    where
        outerNote = if (isUp stemDir) then lowest else highest
        highest  =  maximum $ map (fst) notes
        lowest   =  minimum $ map (fst) notes

-- | Engraves a stem.
--   The local origin will be in the middle column, at position zero.
engraveStem :: Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveStem stemDir notes =
    pos $ style $ rect stemWeight (convert $ stemLength notes)
    where
        pos    =  translate (r2 (convert $ posX, convert $ posY))
        posX   =  stemXOffset stemDir notes
        posY   =  stemYOffset stemDir notes
        style  =  fillColor black . lineWidth 0

-- | Engraves a stem.
--   The local origin will be in the middle column, at position zero.
engraveStem' :: StemAdjustment -> StemDirection -> Flags -> CrossBeams -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveStem' = undefined



-- | Amount to add to stem length. May be negative.
type StemAdjustment = Double

-- | Returns the default direction for the given note heads.
defaultStemDirection :: [NoteHeadPosition] -> Direction
defaultStemDirection [] = down
defaultStemDirection xs
    | (m > 0)    =  down
    | otherwise  =  up
    where
        m = mean . map signum $ xs


-- | Number of flags on a stem.
--   This is the number of flags to actually engrave, irrespective of beams and crossbeams.
type Flags = Int

flagsFromIndex :: Int -> Flags
flagsFromIndex x = max 0 (x - 2)

engraveFlags :: Flags -> Engraving
engraveFlags = undefined -- TODO


-- | Number of crossbeams.
--   This is the number of crossbeams to actually engrave, irrespective of flags and beams.
type CrossBeams = Int


-- | Engraves a set of crossbeams.
--   The local origin will be in the middle of the lines.
engraveCrossBeams :: CrossBeams -> Engraving
engraveCrossBeams = undefined -- TODO




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

-- TODO support any number of dots (see notes in Music.Notable.Core)

-- | Returns the rest, note head, number of flags and number of dots typically used to indicate
--   the given note value.
fromNoteValue :: NoteValue -> (Rest, NoteHead, Flags, Dots)
fromNoteValue x = (restFromIndex val, noteHeadFromIndex val, flagsFromIndex val, dots')
    where
        (val, dots) = properFraction . negate . logBase 2 $ x
        dots' = case dots of
            0.0 -> 0
            _   -> 1



--
-- Accidentals
--

data Accidental
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp
    deriving (Eq, Show, Ord, Bounded, Enum)

-- | Position that the accidental will indicate, offset from middle line.
type AccidentalPosition = HalfSpaces

instance Symbolic Accidental where
    symbol DoubleFlat   =  (baseMusicFont, "\x222b")
    symbol Flat         =  (baseMusicFont, "b")
    symbol Natural      =  (baseMusicFont, "n")
    symbol Sharp        =  (baseMusicFont, "#")
    symbol DoubleSharp  =  (baseMusicFont, "\x2039")

-- | Minimum space required above the given accidental (Tyboni p 24).
minSpaceAbove :: Accidental -> HalfSpaces
minSpaceAbove DoubleFlat   =  3
minSpaceAbove Flat         =  3
minSpaceAbove Natural      =  3
minSpaceAbove Sharp        =  3
minSpaceAbove DoubleSharp  =  1

-- | Minimum space required below the given accidental (Tyboni p 24).
minSpaceBelow :: Accidental -> HalfSpaces
minSpaceBelow DoubleFlat   =  2
minSpaceBelow Flat         =  2
minSpaceBelow Natural      =  3
minSpaceBelow Sharp        =  3
minSpaceBelow DoubleSharp  =  1


-- | Engraves an accidental.
--   The local origin will be to the left at the line indicated by the accidental.
engraveAccidental :: Accidental -> Engraving
engraveAccidental = engraveSymbol . symbol

-- | Column of accidentals, numbered from right to left. 
--   The column closest to the note heads has number zero. 
type AccidentalColumn = Int

-- | Separate accidentals into columns to engrave.
--   The first list is the rightmost column.
separateAccidentals :: [(AccidentalPosition, Accidental)] -> [[(AccidentalPosition, Accidental)]]
separateAccidentals as =
    [ discardColumn . filterColumn x $ as' | x <- [0 .. columns] ]
    where
        as' = distributeAccidentals as
        columns = maximumWith (-1) $ fmap getAccCol as'
        filterColumn x = filter ((==) x . getAccCol)
        discardColumn = fmap (\(c, p, a) -> (p, a))

distributeAccidentals :: [(AccidentalPosition, Accidental)] -> [(AccidentalColumn, AccidentalPosition, Accidental)]
distributeAccidentals = distributeAccidentals' . sortBy (comparing fst)

distributeAccidentals' = foldr addColumn []

-- | Fold over a list of accidentals, 
addColumn (position, accidental) as = (column, position, accidental) : as
    where
        column = findColumn 0 (position, accidental) as 

-- | The expression `findColumn col a as` returns a column greater than or equal to `col` in
--   which there is space available for accidental `a`, given a set of previous accidentals `as`.
findColumn col a as 
    | spaceAvailable col a as  =  col
    | otherwise                =  findColumn (succ col) a as

-- | The expression `spaceAvailable column a as` determines whether the accidental `a` can 
--   fit into the column `col`, given a set of previous accidentals `as`. 
spaceAvailable column (position, accidental) as 
    | null notesInColumn  =  True
    | otherwise           =  upperNoteBound >= lowerNoteBound 
    where
        notesInColumn   =  filter (\a -> getAccCol a == column) as
        upperNote       =  head notesInColumn
        upperNoteBound  =  getAccPos upperNote - minSpaceBelow (getAccAcc upperNote)
        lowerNoteBound  =  position + minSpaceAbove accidental
    
getAccCol = fst3
getAccPos = snd3
getAccAcc = trd3                                


ac = Prelude.reverse . map (\a -> (getAccCol a, getHalfSpaces $ getAccPos a, getAccAcc a))


engraveAccidentalColumn :: [(AccidentalPosition, Accidental)] -> Engraving
engraveAccidentalColumn = mconcat . fmap (\(p, a) -> moveHalfSpacesUp p $ engraveAccidental a)

engraveAccidentals :: [(AccidentalPosition, Accidental)] -> Engraving
engraveAccidentals = 
    catLeft . fmap engraveAccidentalColumn . separateAccidentals


-- start at top
-- if collision, move to next column


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
    | Staccato
    deriving (Eq, Show, Ord, Bounded, Enum)

instance Symbolic Articulation where
    symbol Fermata   =   (baseMusicFont, "U")
    symbol Downbow   =   (baseMusicFont, "^")
    symbol Upbow     =   (baseMusicFont, "V")
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

-- | Engraves an articulation mark.
engraveArticulation :: Articulation -> Engraving
engraveArticulation = engraveSymbol . symbol

-- | Engraves a set of articulation marks.
-- 
--   The accidentals will be drawn beginning slightly above or below the given position and continue upwards or
--   downwards depending on the direction value. The boolean values indicate whether space should be left for
--   ties and slurs respectively.
-- 
--   The local origin will be in the middle, at position zero.
engraveArticulations :: Direction -> NoteHeadPosition -> Bool -> Bool -> [Articulation] -> Engraving
engraveArticulations = undefined -- TODO


--
-- Vertical lines
--

data VerticalLine
    = Arpeggio
    deriving (Eq, Show)

-- | Engraves a set of vertical lines.
-- 
--   The local origin will be in the middle, at position zero.
engraveVerticalLines :: (NoteHeadPosition, NoteHeadPosition) -> [VerticalLine] -> Engraving
engraveVerticalLines = undefined -- TODO

--
-- Ledger lines
--

-- | Number of long and short ledger lines, and their offset from the middle line.
--
--   Short ledger lines is used for single notes, while longer lines are used for primes and seconds.
type LongShortOffset = (Int, Int, HalfSpaces)

-- | Ledger lines to draw above the staff.
type LedgerLinesAbove = LongShortOffset

-- | Ledger lines to draw below the staff.
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

ledgerLinesAbove staffLines stemDir positions =
    (0, shortAbove, firstAbove)
    where
        shortAbove  =
            truncate
                . maybe 0 (\p -> (p - firstAbove + 2) / 2)
                . fmap maximum . nonEmpty
                . filter (> firstAbove) $ positions
        firstAbove   =  fromIntegral staffLines + 1

ledgerLinesBelow staffLines stemDir positions =
    (0, shortBelow, firstBelow)
    where
        shortBelow  =
            truncate
                . maybe 0 (negate . \p -> (p - firstBelow - 2) / 2)
                . fmap minimum . nonEmpty
                . filter (< firstBelow) $ positions
        firstBelow  =  (negate . fromIntegral $ staffLines) - 1

-- | Engraves ledger lines.
--   The origin will be in the middle, at position zero.
engraveLedgerLines :: LedgerLines -> Engraving
engraveLedgerLines ( LedgerLines ( (longAbove, shortAbove, offsetAbove),
                                   (longBelow, shortBelow, offsetBelow)) ) =
    aboveE <> belowE
    where
        aboveE   = moveHalfSpacesUp offsetAbove . catUp   . replicate shortAbove $ ledgerE
        belowE   = moveHalfSpacesUp offsetBelow . catDown . replicate shortBelow $ ledgerE

        ledgerE  =  lineE <> spaceE
        lineE    =  style $ hrule width where { style = lineWidth ledgerLineWeight }
        spaceE   =  spaceRect width (convert space)

        width    =  convert (2 * space) :: Double


--
-- Chords
--

data Stem =
    Stem { -- | Direction of stem.
           stemDirection   :: StemDirection,
           -- | Adjustment for stem.
           stemAdjustment :: StemAdjustment,
           -- | Flags to draw on stem.
           flags      :: Flags,
           -- | Cross beams to draw on stem.
           crossBeams :: CrossBeams }
    deriving (Eq, Show)

data Note =
    Note { -- | Position of note head.
           noteHeadPosition :: NoteHeadPosition,
           -- | Type of note head.
           noteHead         :: NoteHead,
           -- | Accidental to draw for this note head (irrespective of previous accidentals in the same bar).
           accidental       :: Maybe Accidental }
    deriving (Eq, Show)

data Chord =
    Chord { -- | List of notes.
            notes         :: [Note],
            -- | Rest to display if the note list is empty.
            rest          :: Rest,
            -- | Number of dots.
            dots          :: Dots,
            -- | Properties of stem (if present).
            stem          :: Stem,
            -- | Whether the note is to be tied (affects placement of slurs and articulations).
            tied          :: Bool,
            -- | Whether the note is to be slurred (affects placement of articulations).
            slurred       :: Bool,
            -- | List of articulations.
            articulations :: [Articulation],
            -- | List of vertical lines.
            verticalLines :: [VerticalLine] }
    deriving (Eq, Show)

instance Trivial Stem where
    trivial =
        Stem { stemDirection = defaultStem,
               stemAdjustment = 0,
               flags = 0,
               crossBeams = 0 }

instance Trivial Note where
    trivial =
        Note { noteHeadPosition = 0,
               noteHead = WholeNoteHead,
               accidental = Nothing }

instance Trivial Chord where
    trivial =
        Chord { notes = [],
                rest = WholeNoteRest,
                dots = 0,
                stem = trivial,
                tied = False,
                slurred = False,
                articulations = [],
                verticalLines = [] }

splitNotes :: [Note] -> ([(NoteHeadPosition, NoteHead)], [Maybe Accidental])
splitNotes = unzip . map splitNote
    where
        splitNote (Note p h a) = ((p, h), a)

getNotes :: [Note] -> [(NoteHeadPosition, NoteHead)]
getNotes = fst . splitNotes

getNotePositions :: [Note] -> [NoteHeadPosition]
getNotePositions = map fst . getNotes

getNoteHeads :: [Note] -> [NoteHead]
getNoteHeads = map snd . getNotes

getNoteAccidentals :: [Note] -> [(AccidentalPosition, Accidental)]
getNoteAccidentals ns =
    removeNothingRight $ ps `zip` as
    where          
        ps = getNotePositions $ ns
        as = snd . splitNotes $ ns



--
-- Positioning
--

notePositions :: Chord -> [R2]
notePositions = map (\y -> r2 (0, y)) . map convert . getNotePositions . notes

highestNotePosition :: Chord -> R2
highestNotePosition = last . notePositions

lowestNotePosition :: Chord -> R2
lowestNotePosition = head . notePositions

-- | Lowest note head with x-offset for stems.
stemRootPosition :: Chord -> R2
stemRootPosition = undefined

-- | Highest note head + x-offset for stems + octave + adjustment.
stemTopPosition :: Chord -> R2
stemTopPosition = undefined

-- Tricky, depends on accidentals.
slurAboveAnchor :: Chord -> R2
slurAboveAnchor = undefined

-- Tricky, depends on accidentals.
slurBelowAnchor :: Chord -> R2
slurBelowAnchor = undefined

-- Tricky, depends on accidentals.
tieAboveAnchor :: Chord -> R2
tieAboveAnchor = undefined

-- Tricky, depends on accidentals.
tieBelowAnchor :: Chord -> R2
tieBelowAnchor = undefined

-- Tricky, depends on accidentals.
tupletAboveAnchor :: Chord -> R2
tupletAboveAnchor = undefined

-- Tricky, depends on accidentals.
tupletBelowAnchor :: Chord -> R2
tupletBelowAnchor = undefined

-- Depends on stem direction.
glissandoBeforeAnchor :: Chord -> R2
glissandoBeforeAnchor = undefined

-- Depends on stem direction.
glissandoAfterAnchor :: Chord -> R2
glissandoAfterAnchor = undefined



-- | Engraves a single note.
engraveNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
-- engraveNote pos dir noteHead =
    -- moveHalfSpacesUp pos $ headE <> stemE <> spaceE
    -- where
    --     spaceE  =  spaceRect (fst . unr2 $ noteHeadSpace) (convert space)
    -- 
    --     headE   =  position $ engraveSymbolFloating (symbol noteHead)
    --         where { position = translate (r2 (0.5 * getX noteHeadSpace,0)) }
    -- 
    --     stemE   =  if (hasStem noteHead) then stemE' else mempty
    --     stemE'  =  moveOriginBy noteStemOffset . style $ rect stemWeight noteStemHeight
    --         where { style = lineColor black . fillColor black }
    -- 
    --     noteStemHeight  =  convert (space * 3.5) - stemShortenAtOuterNote
    -- 
    --     noteHeadSpace   =  symbolSpacer (symbol noteHead)
    --     noteStemOffset  =  r2 . negateIfDown dir $ (noteStemOffsetX, noteStemOffsetY)
    --     noteStemOffsetX =  (getX $ noteHeadSpace / 2) + stemInset
    --     noteStemOffsetY =  negate $ convert space * 3.5 / 2 + (stemShortenAtOuterNote / 2)
engraveNote = engraveNote'

engraveNote' :: HalfSpaces -> Direction -> NoteHead -> Engraving
engraveNote' pos stemDir noteHead =
    engraveChord chord
    where
        note = trivial { noteHeadPosition = pos, noteHead = noteHead }
        chord = trivial { notes = [note] }



-- | Engraves a chord. The origin will be in the main column at the middle line.
--   (see 'engraveNoteHeads' for an explanation of columns).

-- PRECOND none
engraveChord :: Chord -> Engraving
engraveChord chord = mempty
    <> engraveRestOrNoteHeads restN directionN notesN

    -- TODO use richer version of engraveStem instead of these three, and only if there is actually a stem
    <> engraveStem directionN notesN
    -- <> engraveFlags (flags (stem chord))
    -- <> engraveCrossBeams (crossBeams (stem chord))

    -- <> engraveDots dotsN positionsN
    -- <> engraveAccidentals accidentalsN
    -- <> engraveArticulations directionN outerNoteN (tied chord) (slurred chord) (articulations chord)
    -- <> engraveVerticalLines (topNoteN, bottomNoteN) (verticalLines chord)
    <> engraveLedgerLines ledgerLinesN
    where                               
        directionN    =  (getStemDirection . stemDirection . stem $ chord) . defaultStemDirection . getNotePositions . notes $ chord
        restN         =  rest chord
        notesN        =  getNotes (notes chord)
        accidentalsN  =  getNoteAccidentals (notes chord)                 
        positionsN    =  getNotePositions (notes chord)
        dotsN         =  dots chord
        ledgerLinesN  =  ledgerLines directionN positionsN
        topNoteN      =  maximum positionsN
        bottomNoteN   =  minimum positionsN
        outerNoteN    =  if isUp directionN then topNoteN else bottomNoteN
                                                                                  