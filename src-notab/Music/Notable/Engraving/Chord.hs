
{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleContexts #-}

-- | This module handles engraving of rests and notes, along with associated objects such as stems, dots,
--   accidentals, articulations and so on. Note that the term chord is used to indicate a set of note heads,
--   which is referred to as a rest if empty.
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
    -- partitionNoteHeads,
    -- separateNoteHeads,
    -- separateNoteHeadsWith,
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
    stemHorizOffset,
    stemVertOffset,

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
    -- separateAccidentals,
    engraveAccidental,      
    engraveAccidentalColumn,
    engraveAccidentals,

-- ** Articulation
    Articulation(..),
    alwaysAbove,
    engraveArticulation,
    engraveArticulations,

-- ** Vertical lines
    VerticalLine(..),
    ArticulationPlacement(..),
    engraveVerticalLines,

-- * Ledger lines
    LongLedgers,
    ShortLedgers,
    LedgersAbove,
    LedgersBelow,
    Ledgers(..),
    ledgers,
    ledgers',
    engraveLedgers,


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
kStemWeight        :: Double
-- | Amount of space to move stems inward by.
kStemInset         :: Double
-- | Amount of space to shorten stem by at the outer note.
kStemCap           :: Double
-- | Thickness of ledger lines.
kLedgerLineWeight  :: Double

kStemWeight        = convert $ Spaces 0.12
kStemInset         = convert $ Spaces 0.032
kStemCap           = convert $ Spaces 0.1
kLedgerLineWeight  = convert $ Spaces 0.14


-- | Amount of space of space to add to the right of vertical lines.
kVerticalLineSpace :: Double
-- | Amount of space to add to the right of accidentals.
kAccidentalSpace   :: Double
-- | Amount of space to add to the left of dots.
kDotSpace          :: Double

kVerticalLineSpace = convert $ Spaces 0
kAccidentalSpace   = convert $ Spaces 0.3
kDotSpace          = convert $ Spaces 0.3



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

instance Symbolic Rest where
    symbol WholeNoteRest          =  (baseMusicFont, "\xd3")
    symbol HalfNoteRest           =  (baseMusicFont, "\x2211")
    symbol QuarterNoteRest        =  (baseMusicFont, "\x152")
    symbol EightNoteRest          =  (baseMusicFont, "\x2030")
    symbol SixteenthNoteRest      =  (baseMusicFont, "\x2248")
    symbol ThirtySecondNoteRest   =  (baseMusicFont, "\xae")

restFromIndex :: Int -> Rest
restFromIndex x = toEnum (x + 1)

-- | Engraves a rest.
--   The local origin will in the middle, at position zero.
engraveRest :: Rest -> Engraving
engraveRest = centerX . engraveSymbol . symbol



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

--   I don't want to make it an instance of Enum, as the index only make sense for "standard"
--   notes such as brevis, whole, filled etc.
noteHeadFromIndex x
    | x  <= -1  = BrevisNoteHead
    | x  <=  0  = WholeNoteHead
    | x  <=  1  = UnfilledNoteHead
    | otherwise = FilledNoteHead


-- | Vertical position of a note head, offset from middle line.
type NoteHeadPosition = HalfSpaces

-- | Whether a given notehead should be engraved with a stem or not.
hasStem :: NoteHead -> Bool
hasStem BrevisNoteHead          = False
hasStem WholeNoteHead           = False
hasStem UnfilledNoteHead        = True
hasStem FilledNoteHead          = True
hasStem DiamondNoteHead         = False
hasStem CrossNoteHead           = False
hasStem CircledCrossNoteHead    = False
hasStem UnfilledSquareNoteHead  = False
hasStem FilledSquareNoteHead    = False


-- | Separate small intervals (i.e. primes and seconds) from others.
--   Such intervals must be engraved on opposite sides of the stem to avoid collisions.
partitionNoteHeads :: Direction 
                   -> [NoteHeadPosition] 
                   -> ([(NoteHeadPosition, NoteHeadPosition)], [NoteHeadPosition])
partitionNoteHeads stemDir positions =
    partitioner collides positions
    where
        collides x y = x `absoluteDifference` y < 2
        partitioner
            | isUp   stemDir = partition2
            | isDown stemDir = reversePartition2

-- | Separates note heads to be engraved to the left and right of the stem respectively.
separateNoteHeads :: Direction 
                  -> [NoteHeadPosition] 
                  -> ([NoteHeadPosition], [NoteHeadPosition])
separateNoteHeads d = separateNoteHeads' d . Data.List.sort

separateNoteHeads' stemDir positions
    | isUp   stemDir = (ls `merge` xs, us)
    | isDown stemDir = (ls, xs `merge` us)
        where
            (ps, xs) = partitionNoteHeads stemDir positions
            (ls, us) = unzip ps

-- | Like 'separateNoteHeads', but passing an arbitrary data type along.
separateNoteHeadsWith :: Direction 
                      -> [(NoteHeadPosition, b)] 
                      -> ([(NoteHeadPosition, b)], [(NoteHeadPosition, b)])
separateNoteHeadsWith stemDir noteHeads = mergeZip leftPoses heads rightPoses
    where
        noteHeads'               =  sortBy (comparing fst) noteHeads
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

engraveNoteHeadColumn :: Bool -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveNoteHeadColumn isLeft notes =
    mconcat . fmap (\(p, n) -> align $ engraveNoteHead p n) $ notes
        where
            align = if isLeft then alignR else alignL

-- Returns the x offset of the main column along with the engraving
-- This is required by stemHorizOffset
engraveNoteHeads' :: Direction -> [(NoteHeadPosition, NoteHead)] -> (Double, Engraving)
engraveNoteHeads' stemDir notes = (x, pos $ lc `leftTo` rc)
    where
        (lc, rc) =  mapPair l r $ separateNoteHeadsWith stemDir notes
            where { l = engraveLeftNoteHeadColumn
                  ; r = engraveRightNoteHeadColumn }
        mc = if (isUp stemDir) then lc else rc
        x   =  negateIfDown stemDir (width mc / 2)
        pos =  translateX x

-- | Engraves the given set of note heads.
--
--   This function will partition the note heads into a main column and a side column, to avoid colliding
--   seconds and primes whenever possible. The side column is to the right if the stem direction is up, and to
--   the left if the stem direction is down. The local origin will be in the main column at position zero.
engraveNoteHeads :: Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveNoteHeads stemDir = snd . engraveNoteHeads' stemDir

-- | Like 'engraveRest' if the given set of notes is empty, or 'engraveNoteHeads' otherwise.
engraveRestOrNoteHeads :: Rest -> Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveRestOrNoteHeads rest stemDir noteHeads
    | null noteHeads = engraveRest rest
    | otherwise      = engraveNoteHeads stemDir noteHeads



--
-- Dots
--

-- | Number of dots.
type Dots = Int

-- | Engraves a single dot.
--   The local origin will be in the middle, at position zero.
engraveDot :: NoteHeadPosition -> Engraving
engraveDot pos = p . s $ dotE <> spaceE
    where                              
        dotE    =  circle (convert $ halfSpace * 0.33)
        spaceE  =  spaceRect (convert space * 0.6) (convert space)
        p  =  moveHalfSpacesUp pos
        s  =  lineWidth 0 . fillColor black

-- | Where to draw the dot for a notehead at this position.
dotPosition :: NoteHeadPosition -> NoteHeadPosition
dotPosition x = r (x / 2) * 2 + 1
    where
        r = fromIntegral . floor

-- | Engraves the dots required for the given note heads.
--   The local origin will be at the leftmost dot, at position zero.
engraveDots :: Dots -> [NoteHeadPosition] -> Engraving
engraveDots dots ps =
    catRight $ replicate dots col
    where
        col  =  mconcat $ fmap engraveDot $ ps'
        ps'  =  Data.List.nub . map dotPosition $ ps 



--
-- Stems
--

-- TODO change to (Chord -> D -> D) and compose with defaultStem ?
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

stemLength :: Direction -> [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemLength stemDir notes
    | length notes == 0  =  error "stemLength: Note list is empty"
    | otherwise          =  octave + (hi - lo)
    where
        hi = maximum $ map (fst) notes
        lo = minimum $ map (fst) notes

stemHorizOffset :: Direction -> [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemHorizOffset stemDir = convert . (subtract inset) . fst . engraveNoteHeads' stemDir
    where
        inset = negateIfDown stemDir kStemInset

stemVertOffset :: Direction -> [(NoteHeadPosition, NoteHead)] -> HalfSpaces
stemVertOffset stemDir notes
    | null notes  =  error "stemVertOffset: Note list is empty"
    | otherwise   =  0 + (negateIfDown stemDir $ stemLength stemDir notes) / 2 + outerNote
    where
        outerNote  =  if (isUp stemDir) then lo else hi
        hi  =  maximum $ map (fst) notes
        lo  =  minimum $ map (fst) notes

-- | Engraves a stem.
--   The local origin will be in the middle column, at position zero.
engraveStem :: Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveStem stemDir notes =
    pos $ style $ rect kStemWeight (convert len)
    where
        len    =  stemLength stemDir notes
        pos    =  translate . r2 . mapPair convert convert 
                      $ 
                  (stemHorizOffset stemDir notes, stemVertOffset stemDir notes)
        style  =  fillColor black . lineWidth 0

-- | Engraves a stem.
--   The local origin will be in the middle column, at position zero.
engraveStem' :: StemAdjustment -> StemDirection -> Flags -> CrossBeams -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveStem' = undefined



-- | Amount to add to stem length. May be negative.
type StemAdjustment = Double

-- | Returns the default direction for the given note heads.
defaultStemDirection :: [NoteHeadPosition] -> Direction
defaultStemDirection xs
    | null xs    =  error "defaultStemDirection: Note list is empty"
    | (m >= 0)   =  down
    | otherwise  =  up
    where
        m = mean . map signum $ xs


-- | Number of flags on a stem.
--   This is the number of flags to actually engrave, irrespective of beams and crossbeams.
type Flags = Int

flagsFromIndex :: Int -> Flags
flagsFromIndex x = max 0 (x - 2)

engraveFlags :: Flags -> Engraving
engraveFlags = mempty -- TODO


-- | Number of crossbeams.
--   This is the number of crossbeams to actually engrave, irrespective of flags and beams.
type CrossBeams = Int


-- | Engraves a set of crossbeams.
--   The local origin will be in the middle of the lines.
engraveCrossBeams :: CrossBeams -> Engraving
engraveCrossBeams = mempty -- TODO




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

accidentalSpace DoubleFlat   =  (3, 2)
accidentalSpace Flat         =  (3, 2)
accidentalSpace Natural      =  (3, 3)
accidentalSpace Sharp        =  (3, 3)
accidentalSpace DoubleSharp  =  (1, 1)

-- | Minimum space required above the given accidental.
minSpaceAbove :: Accidental -> HalfSpaces
minSpaceAbove = fst . accidentalSpace

-- | Minimum space required below the given accidental.
minSpaceBelow :: Accidental -> HalfSpaces
minSpaceBelow = snd . accidentalSpace

-- | Engraves an accidental.
--   The local origin will be to the left at the line indicated by the accidental.
engraveAccidental :: Accidental -> Engraving
engraveAccidental = engraveSymbol . symbol

-- | Engraves a column of accidentals.
--   The local origin will be to the right at position zero.
engraveAccidentalColumn :: [(AccidentalPosition, Accidental)] -> Engraving
engraveAccidentalColumn = mconcat . fmap (\(p, a) -> moveHalfSpacesUp p $ alignR $ engraveAccidental a)

-- | Column of accidentals, numbered from right to left. 
--   The column closest to the note heads has number zero. 
type AccidentalColumn = Int


distributeAccidentals :: [(AccidentalPosition, Accidental)] 
                      -> [(AccidentalColumn, AccidentalPosition, Accidental)]
distributeAccidentals = d . sortBy (comparing fst)
    where
        d = foldr addColumn []

addColumn :: (AccidentalPosition, Accidental)
          -> [(AccidentalColumn, AccidentalPosition, Accidental)]
          -> [(AccidentalColumn, AccidentalPosition, Accidental)]
addColumn (position, accidental) as = (column, position, accidental) : as
    where
        column = findColumn 0 (position, accidental) as 

-- | The expression `findColumn col a as` returns a column greater than or equal to `col` in
--   which there is space available for accidental `a`, given a set of previous accidentals `as`.
findColumn :: AccidentalColumn 
           -> (AccidentalPosition, Accidental) 
           -> [(AccidentalColumn, AccidentalPosition, Accidental)] 
           -> AccidentalColumn
findColumn col a as 
    | spaceAvailable col a as  =  col
    | otherwise                =  findColumn (succ col) a as

-- | The expression `spaceAvailable column a as` determines whether the accidental `a` can 
--   fit into the column `col`, given a set of previous accidentals `as`. 
spaceAvailable :: AccidentalColumn 
               -> (AccidentalPosition, Accidental) 
               -> [(AccidentalColumn, AccidentalPosition, Accidental)] 
               -> Bool
spaceAvailable column (position, accidental) as 
    | null notesInColumn  =  True
    | otherwise           =  upperNoteBound >= lowerNoteBound 
    where
        notesInColumn   =  filter (\a -> fst3 a == column) as
        upperNote       =  head notesInColumn
        upperNoteBound  =  position' -  minSpaceBelow accidental'
        lowerNoteBound  =  position  +  minSpaceAbove accidental
        position'       =  snd3 upperNote                  
        accidental'     =  trd3 upperNote    
    
-- | Separate accidentals into columns, from left to right.
separateAccidentals :: [(AccidentalPosition, Accidental)] -> [[(AccidentalPosition, Accidental)]]
separateAccidentals as = 
    do  { x <- [0 .. columns]
        ; return . discardColumn . filterColumn x $ as'
        }
    where
        as' = distributeAccidentals as
        columns = maximumWith (-1) $ fmap fst3 as'
        filterColumn x = filter ((==) x . fst3)
        discardColumn = fmap (\(c, p, a) -> (p, a))

-- | Engraves a column of accidentals.
--   The local origin will be to the right of the rightmost column at position zero.
engraveAccidentals :: [(AccidentalPosition, Accidental)] -> Engraving
engraveAccidentals = catLeft . fmap engraveAccidentalColumn . separateAccidentals



--
-- Articulation
--

-- | Articulation mark.
--
--   These are always drawn in a particular order, represented by the 'Enum' and 'Bounded' instances.
--   The 'minBound' value should be closest to the chord and the 'maxBound' value farthest away from
--   the chord.
data Articulation
    = Staccato
    | Tenuto
    | Accent
    | Marcato
    | Circle
    | Plus
    | Upbow
    | Downbow
    | Fermata
    deriving (Eq, Show, Ord, Bounded, Enum)

instance Symbolic Articulation where
    symbol Staccato  =   (baseMusicFont, ".")
    symbol Tenuto    =   (baseMusicFont, "-")
    symbol Accent    =   (baseMusicFont, ">")
    symbol Marcato   =   (baseMusicFont, "v")
    symbol Circle    =   (baseMusicFont, "o")
    symbol Plus      =   (baseMusicFont, "+")
    symbol Upbow     =   (baseMusicFont, "V")
    symbol Downbow   =   (baseMusicFont, "^")
    symbol Fermata   =   (baseMusicFont, "U")

data ArticulationPlacement
    = Articulation Articulation
    | Legato
    deriving (Eq, Show)

instance Ord ArticulationPlacement where        
    Articulation x <= Articulation y  =  x <= y
    Legato         <= Articulation y  =  alwaysAbove y
    Articulation x <= Legato          =  not (alwaysAbove x)
    Legato         <= Legato          =  True

-- | Whether the accidental should always be drawn above the staff lines.
alwaysAbove :: Articulation -> Bool
alwaysAbove = oneOf [Fermata, Plus, Circle]

-- | Engraves an articulation mark.
engraveArticulation :: Articulation -> Engraving
engraveArticulation = centerX . engraveSymbol . symbol

aboveArticulationPoses :: NoteHeadPosition -> [NoteHeadPosition]
aboveArticulationPoses = undefined

-- as long as we are inside the staff lines, increase 1, then increase 0.5
articulationPoses :: StaffLines -> Direction -> NoteHeadPosition -> [NoteHeadPosition]
articulationPoses = undefined


-- | Engraves a set of articulation marks.
-- 
--   The accidentals will be drawn beginning slightly above or below the given position and continue upwards or
--   downwards depending on the direction value. The boolean values indicate whether space should be left for
--   ties and slurs respectively.
-- 
--   The local origin will be in the middle, at position zero.
engraveArticulations :: Direction -> NoteHeadPosition -> Bool -> Bool -> [Articulation] -> Engraving
engraveArticulations stemDir outerNote tied slurred articulations = mempty -- TODO



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
engraveVerticalLines notes lines = mempty -- TODO



--
-- Ledger lines
--

-- | Long ledger lines, used for primes and seconds.
type LongLedgers = Int

-- | Short ledger lines, used for single notes.
type ShortLedgers = Int

-- | Number of ledger lines to draw above the staff, and their offset from the middle line.
type LedgersAbove = (LongLedgers, ShortLedgers, HalfSpaces)

-- | Number of ledger lines to draw above the staff, and their offset from the middle line.
type LedgersBelow = (LongLedgers, ShortLedgers, HalfSpaces)

-- | A description of ledger lines to engrave for a particular chord.
newtype Ledgers = Ledgers { getLedgers :: (LedgersAbove, LedgersBelow) }
    deriving (Eq, Show)

-- | Monoid over ledger lines. This can be used to join ledger lines required by different chords.
--
--   Note that `mappend` raises an error if called on ledger lines with different offsets, as joining
--   ledger lines from different types of staves makes no sense.
instance Monoid Ledgers where
    mempty  =  Ledgers (none, none) where none = (0, 0, 0)
    Ledgers x `mappend` Ledgers y  =  Ledgers $ prod2 apLines apLines x y
        where
            apLines         =  prod3 max max apOffset
            x `apOffset` y  =  if (x == y) then x else error "Ledgers.mappend: unequal offset"


-- | Returns the ledger lines required by a given set of note heads. The stem direction is necessary to
--   determine the placement of second and prime intervals, which in turn influences the length of lines.
ledgers :: Direction -> [NoteHeadPosition] -> Ledgers
ledgers = ledgers' 5

-- | Returns the ledger lines required by a given set of note heads. This version works for any
--   number of staff lines (but see note about `mappend` above).
ledgers' :: StaffLines -> Direction -> [NoteHeadPosition] -> Ledgers
ledgers' s d p = Ledgers $ (ledgersAbove s d p, ledgersBelow s d p)

ledgersAbove :: StaffLines -> Direction -> [NoteHeadPosition] -> LedgersAbove
ledgersAbove staffLines stemDir positions = (0, s, th + 1)
    where
        s = ifNonEmpty sl 0 . filter (> th) $ positions
        sl ps = truncate $ (maximum ps - th + 1) / 2
        th = fromIntegral staffLines

ledgersBelow :: StaffLines -> Direction -> [NoteHeadPosition] -> LedgersBelow
ledgersBelow staffLines stemDir positions = (0, s, th - 1)
    where
        s = ifNonEmpty sl 0 . filter (< th) $ positions
        sl ps = truncate . negate $ (minimum ps - th - 1) / 2
        th = negate . fromIntegral $ staffLines
       

ledger :: Spaces -> Engraving
ledger width = lineE <> spaceE
    where
        lineE  =  style $ hrule w where { style = lineWidth kLedgerLineWeight }
        spaceE =  spaceRect w h
        w  =  convert width
        h  =  convert space

shortLedger :: Engraving
shortLedger = ledger 2

longLedger :: Engraving
longLedger = ledger 3


-- | Engraves ledger lines.
--   The origin will be in the middle, at position zero.
engraveLedgers :: Ledgers -> Engraving
engraveLedgers ( Ledgers ( (longAbove, shortAbove, offsetAbove),
                           (longBelow, shortBelow, offsetBelow)) ) =
    aboveE <> belowE
    where
        aboveE   = moveHalfSpacesUp offsetAbove . catUp   . replicate shortAbove $ shortLedger
        belowE   = moveHalfSpacesUp offsetBelow . catDown . replicate shortBelow $ shortLedger




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
        Stem { stemDirection = defaultStem,
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


-- | Engraves a single note.
engraveNote :: Direction -> NoteHeadPosition -> NoteHead -> Engraving
engraveNote stemDir pos noteHead =
    engraveChord chord
    where
        note = trivial { noteHeadPosition = pos, noteHead = noteHead }
        stem = trivial { stemDirection = if isUp stemDir then stemUp else stemDown }
        chord = trivial { notes = [note], stem = stem }

-- | Engraves a chord. The origin will be in the main column at the middle line.
--   (see 'engraveNoteHeads' for an explanation of columns).
engraveChord :: Chord -> Engraving
engraveChord chord =
    -- `leftTo` engraveVerticalLines (topNoteN, bottomNoteN) (verticalLines chord)
    -- `leftTo` spaceX kVerticalLineSpace    
    
    (alignR $ engraveAccidentals accidentalsN `leftTo` spaceX kAccidentalSpace)
    `leftTo` (mempty
        <> engraveRestOrNoteHeads restN directionN notesN
        <> (if hasStemN then engraveStem directionN notesN else mempty)
        <> engraveArticulations directionN outerNoteN (tied chord) (slurred chord) (articulations chord)
        <> (emptyEnvelope `withEnvelope` engraveLedgers ledgersN)
        )
    `leftTo` spaceX kDotSpace    
    `leftTo` engraveDots dotsN positionsN
    where                               
        directionN    =  (getStemDirection . stemDirection . stem $ chord) . defaultStemDirection . getNotePositions . notes $ chord
    
        restN         =  rest chord
        notesN        =  getNotes (notes chord)
        accidentalsN  =  getNoteAccidentals (notes chord)                 
        positionsN    =  getNotePositions (notes chord)
    
        hasStemN      =  any hasStem (getNoteHeads . notes $ chord)
        dotsN         =  dots chord
    
        ledgersN      =  ledgers directionN positionsN
    
        topNoteN      =  maximum positionsN
        bottomNoteN   =  minimum positionsN
        outerNoteN    =  if isUp directionN then topNoteN else bottomNoteN
                                                                                  

                                                                                  
-- --
-- -- Positioning
-- --
-- 
-- notePositions :: Chord -> [R2]
-- notePositions = map (\y -> r2 (0, y)) . map convert . getNotePositions . notes
-- 
-- highestNotePosition :: Chord -> R2
-- highestNotePosition = last . notePositions
-- 
-- lowestNotePosition :: Chord -> R2
-- lowestNotePosition = head . notePositions
-- 
-- -- | Lowest note head with x-offset for stems.
-- stemRootPosition :: Chord -> R2
-- stemRootPosition = undefined
-- 
-- -- | Highest note head + x-offset for stems + octave + adjustment.
-- stemTopPosition :: Chord -> R2
-- stemTopPosition = undefined
-- 
-- -- Tricky, depends on accidentals.
-- slurAboveAnchor :: Chord -> R2
-- slurAboveAnchor = undefined
-- 
-- -- Tricky, depends on accidentals.
-- slurBelowAnchor :: Chord -> R2
-- slurBelowAnchor = undefined
-- 
-- -- Tricky, depends on accidentals.
-- tieAboveAnchor :: Chord -> R2
-- tieAboveAnchor = undefined
-- 
-- -- Tricky, depends on accidentals.
-- tieBelowAnchor :: Chord -> R2
-- tieBelowAnchor = undefined
-- 
-- -- Tricky, depends on accidentals.
-- tupletAboveAnchor :: Chord -> R2
-- tupletAboveAnchor = undefined
-- 
-- -- Tricky, depends on accidentals.
-- tupletBelowAnchor :: Chord -> R2
-- tupletBelowAnchor = undefined
-- 
-- -- Depends on stem direction.
-- glissandoBeforeAnchor :: Chord -> R2
-- glissandoBeforeAnchor = undefined
-- 
-- -- Depends on stem direction.
-- glissandoAfterAnchor :: Chord -> R2
-- glissandoAfterAnchor = undefined
-- 
