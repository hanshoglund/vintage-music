
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
    -- partitionNoteHeads,
    -- separateNoteHeads,
    -- separateNoteHeadsWith,
    engraveNoteHead,
    engraveLeftNoteHeadColumn,
    engraveRightNoteHeadColumn,
    engraveNoteHeads,
    engraveRestOrNoteHeads,

-- ** Stems
    AdjustStem,
    engraveStem,
    stemLength,
    stemXOffset,
    stemYOffset,
    
    -- stemUp,
    -- stemDown,
    -- stemFlip,
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
    engraveAccidental,

-- ** Articulation
    Articulation(..),
    alwaysAbove,
    engraveArticulation,

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

import Data.Convert
import Data.Ord ( comparing )
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

stemWeight :: Double
stemWeight = 0.025

stemInset :: Double
stemInset = 0.013

stemShortenAtOuterNote :: Double
stemShortenAtOuterNote = convert $ Spaces 0.1

-- | Thickness of ledger lines.
ledgerLineWeight :: Double
ledgerLineWeight = 0.035



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
    deriving (Show, Eq, Ord, Enum, Bounded)

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
    deriving (Show, Eq)

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
        collides x y     =  y - x < 2
        partitioner
            | isUp   stemDir  =  partition2
            | isDown stemDir  =  reversePartition2

-- | Separates note heads to be engraved to the left and right of the stem respectively.
separateNoteHeads :: Direction -> [NoteHeadPosition] -> ([NoteHeadPosition], [NoteHeadPosition])
separateNoteHeads d = separateNoteHeads' d . {-assertNoPrimes . -}Data.List.sort
    where
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
        noteHeads'  =  Data.List.sortBy (comparing fst) noteHeads
        (poses, heads)           =  unzip noteHeads'
        (leftPoses, rightPoses)  =  separateNoteHeads stemDir poses


-- | Engraves a single note head.
--   The origin will be at the left of the note at position 0.
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
--   This function will partition the note heads into a main column and a side column, to avoid
--   colliding seconds and primes whenever possible. The side column is to the right if the stem
--   direction is up, and to the left if the stem direction is down. The origin will be in the main
--   column at position 0.
engraveNoteHeads :: Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveNoteHeads stemDir = snd . engraveNoteHeads' stemDir

-- Returns the x offset of the main column along with the engraving
-- This is required by stemXOffset
engraveNoteHeads' :: Direction -> [(NoteHeadPosition, NoteHead)] -> (Double, Engraving)
engraveNoteHeads' stemDir notes =
    (mainColumnXOffset, position $ leftColumn `leftTo` rightColumn)
    where
        (leftNotes, rightNotes)  =  separateNoteHeadsWith stemDir notes
        leftColumn   =  engraveLeftNoteHeadColumn leftNotes
        rightColumn  =  engraveRightNoteHeadColumn rightNotes        
        mainColumn = if (isUp stemDir) then leftColumn else rightColumn 

        mainColumnXOffset  =  negateIfDown stemDir (width mainColumn / 2)
        position           =  translate (r2 (mainColumnXOffset, 0))

-- | This function invokes 'engraveRest' if the given set of notes is empty 'engraveNoteHeads'
--   otherwise.
engraveRestOrNoteHeads :: Rest -> Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveRestOrNoteHeads rest stemDir noteHeads
    | null noteHeads  =  engraveRest rest
    | otherwise       =  engraveNoteHeads stemDir noteHeads



--
-- Stems
--

-- -- | Whether the stem should be flipped.
-- type StemType = (Direction -> Direction)
--
-- -- | Always use an upward stem.
-- stemUp :: StemType
-- stemUp = const up
--
-- -- | Always use a downward stem.
-- stemDown :: StemType
-- stemDown = const down
--
-- -- | Flip the default stem direction.
-- stemFlip :: StemType
-- stemFlip = Direction . not . getDirection

-- FIXME set line color to transparent and increase stemWeight
engraveStem :: Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveStem stemDir notes =
    pos $ style $ rect stemWeight (convert $ stemLength notes)
    where                                                       
        pos    =  translate (r2 (convert $ posX, convert $ posY))
        posX   =  stemXOffset stemDir notes
        posY   =  stemYOffset stemDir notes
        style  =  fillColor black

stemLength :: [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemLength notes 
    | length notes == 0  =  error "stemLength: Note list is empty"
    | otherwise       =  octave + (highest - lowest)
    where
        highest  =  maximum $ map (fst) notes
        lowest   =  minimum $ map (fst) notes

stemXOffset :: Direction -> [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemXOffset stemDir = convert . fst . engraveNoteHeads' stemDir

stemYOffset :: Direction -> [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemYOffset stemDir notes 
    | length notes == 0  =  error "stemYOffset: Note list is empty"
    | otherwise          =  0 + (negateIfDown stemDir $ stemLength notes) / 2 + outerNote
    where
        outerNote = if (isUp stemDir) then lowest else highest
        highest  =  maximum $ map (fst) notes
        lowest   =  minimum $ map (fst) notes


-- | Amount to add to stem length. May be negative.
type AdjustStem = Double

-- | Returns the default direction for the given note heads.
stemDirection :: [NoteHeadPosition] -> Direction
stemDirection x = up
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
    symbol DoubleFlat   =  (baseMusicFont, "\x222b")
    symbol Flat         =  (baseMusicFont, "b")
    symbol Natural      =  (baseMusicFont, "n")
    symbol Sharp        =  (baseMusicFont, "#")
    symbol DoubleSharp  =  (baseMusicFont, "\x2039")

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

-- | Engraves a rest.
engraveAccidental :: Accidental -> Engraving
engraveAccidental = engraveSymbol . symbol


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
    deriving (Show, Eq, Ord, Bounded, Enum)

instance Symbolic Articulation where
    symbol Fermata   =   (baseMusicFont, "u")
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
        spaceE   =  spaceRect width (convert space)

        width    =  convert (2 * space) :: Double


--
-- Chords
--

data Stem =
    Stem { stemType   :: Direction,
           adjustStem :: AdjustStem,
           flags      :: Flags,
           crossBeams :: CrossBeams }

data Note =
    Note { noteHeadPosition :: NoteHeadPosition,
           noteHead         :: NoteHead,
           accidental       :: Accidental }

data Chord =
    Chord { notes         :: [Note],
            rest          :: Rest,
            dots          :: Dots,
            stem          :: Stem,
            articulations :: [Articulation],
            verticalLines :: [VerticalLine] }

splitNote :: Note -> ((NoteHeadPosition, NoteHead), Accidental)
splitNote (Note p h a) = ((p, h), a)

splitNotes :: [Note] -> ([(NoteHeadPosition, NoteHead)], [Accidental])
splitNotes = unzip . map splitNote

getNotePositions :: [Note] -> [NoteHeadPosition]
getNotePositions = map fst . fst . splitNotes

getNoteHeads :: [Note] -> [NoteHead]
getNoteHeads = map snd . fst . splitNotes

getNoteAccidentals :: [Note] -> [Accidental]
getNoteAccidentals = snd . splitNotes


-- | Engraves a chord. The origin will be at the middle line, at the standard note column.
engraveChord :: Chord -> Engraving
engraveChord = undefined


notePositions :: Chord -> [R2]
notePositions = map (\y -> r2 (0, y)) . map convert . getNotePositions . notes

highestNotePosition :: Chord -> R2
highestNotePosition = last . notePositions

lowestNotePosition :: Chord -> R2
lowestNotePosition = head . notePositions

-- | Lowest note head with x-offset for stems.
stemRootPosition :: Chord -> R2
stemRootPosition = undefined

-- | Highest note head + x-offset for stems + octave + adjustment.
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



-- TODO reimplement in terms of engraveChord

-- | Engraves a single note.
engraveNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
engraveNote pos dir noteHead =
    moveHalfSpacesUp pos $ headE <> stemE <> spaceE
    where
        spaceE  =  spaceRect (fst . unr2 $ noteHeadSpace) (convert space)
        
        headE   =  position $ engraveSymbolFloating (symbol noteHead)
            where { position = translate (r2 (0.5 * getX noteHeadSpace,0)) }

        stemE   =  if (hasStem noteHead) then stemE' else mempty
        stemE'  =  moveOriginBy noteStemOffset . style $ rect stemWeight noteStemHeight
            where { style = lineColor black . fillColor black }

        noteStemHeight  =  convert (space * 3.5) - stemShortenAtOuterNote

        noteHeadSpace   =  symbolSpacer (symbol noteHead)
        noteStemOffset  =  r2 . negateIfDown dir $ (noteStemOffsetX, noteStemOffsetY)
        noteStemOffsetX =  (getX $ noteHeadSpace / 2) + stemInset
        noteStemOffsetY =  negate $ convert space * 3.5 / 2 + (stemShortenAtOuterNote / 2)      