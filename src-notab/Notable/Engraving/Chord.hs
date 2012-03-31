
{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

-- | Chord-level engraving.
module Notable.Engraving.Chord
(
-- * Accidentals
AccidentalType(..),
Accidental(..),
--accidentalSymbol,

-- * Note heads
NoteHead(..),
NoteHeadPos,
noteHeadSymbol,
noteHeadNeedsStem,
separeteNoteHeads,
partitionSeconds,

-- * Ledger lines
Ledgers,
ledgers,
standardLedgers,
engraveLedgers,

-- * Chords
Dots,
FlipStem,
AdjustStem,
-- Flags,
-- CrossBeams
engraveNote,
engraveChord,
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
-- Accidentals
--

data AccidentalType 
    = Flat 
    | Natural 
    | Sharp

data Accidental 
    = NoAccidental 
    | NormalAccidental AccidentalType 
    | RedundantAccidental AccidentalType

-- TODO accidentalSymbol




--
-- Note heads
--

-- | Represents a note head symbol.
data NoteHead 
    = Unfilled
    | Filled 
    | Whole 
    | Brevis

type NoteHeadPos = HalfSpaces

noteHeadSymbol :: NoteHead -> Symbol
-- noteHeadSymbol Filled    =  (baseMusicFont, [toEnum 0x0153])
-- noteHeadSymbol Unfilled  =  (baseMusicFont, [toEnum 0x02d9])
noteHeadSymbol Filled    =  (specialMusicFont, "f")
noteHeadSymbol Unfilled  =  (specialMusicFont, "F")
noteHeadSymbol Whole     =  (baseMusicFont, "w")
noteHeadSymbol Brevis    =  (baseMusicFont, "W")

-- | Whether a given notehead should be drawn with a stem or not.
noteHeadNeedsStem :: NoteHead -> Bool
noteHeadNeedsStem Unfilled  =  True
noteHeadNeedsStem Filled    =  True
noteHeadNeedsStem Whole     =  False
noteHeadNeedsStem Brevis    =  False


-- | Separate notes involved in seconds from other notes.
--
--   Each given position will only be returned once, so the following holds for all @d@ and @ps@:
--
--   > ps = sort (uppers ++ lowers ++ others)
--   >     where
--   >         (seconds, others)  =  partitionSeconds d ps
--   >         (uppers, lowers)   =  unzip seconds
--       
--   Search always start from the outermost note, so in cases like [1,2,3] would be groped
--   as @[(1,2)] [3]@ for an upward stem and [(2,3)] [1] for a downward stem.
--
partitionSeconds :: Direction -> [NoteHeadPos] -> ([(NoteHeadPos, NoteHeadPos)], [NoteHeadPos])
partitionSeconds direction positions =
    partitioner collides positions
    where
        collides x y     =  y - x < 2
        partitioner
            | direction  =  partition2
            | otherwise  =  reversePartition2

-- | Separates note heads to be drawn to the left and right of the stem respectively.
--
--   Seconds are partitioned so that the lower note heads goes to the right of the stem, 
--   while all other notes are put on the default side of the note. The same layout 
--   is used whether the chord actually has a stem or not. (Tyboni II.1 p 91)
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
                    (pairs, others)  =  partitionSeconds direction positions
                    (lowers, uppers) =  unzip pairs


--
-- Ledger lines
--

-- | Number of ledger lines to draw for a particular chord. 
--   Specifically, @((longAbove, shortAbove), (longBelow, shortBelow))@.

type Ledgers = ((Int, Int), (Int, Int))


-- | Returns the type of ledger lines to draw above respectively below the ordinary staff lines.
--
--   @ledgers staffLines direction positions@ is the ledger lines needed for drawing the given
--   position on the given number of staff lines with the given stem direction.

ledgers :: Int -> Direction -> [NoteHeadPos] -> Ledgers
ledgers staffLines direction positions = 
    ((0, shortAbove), (0, shortBelow)) 
    where              
        shortAbove          =  truncate . maybe 0 (\p -> (p - firstLedgerLineAbove + 1) / 2) $ shortAbovePos
        shortAbovePos       =  fmap maximum . nonEmpty $ shortLinesAbovePos
        shortLinesAbovePos  =  filter (> firstLedgerLineAbove) $ positions

        shortBelow          =  truncate . maybe 0 (negate . \p -> (p - firstLedgerLineBelow - 1) / 2) $ shortBelowPos
        shortBelowPos       =  fmap minimum . nonEmpty $ shortLinesBelowPos
        shortLinesBelowPos  =  filter (< firstLedgerLineBelow) $ positions

        -- TODO find long lines as well    
                                        
        firstLedgerLineAbove  =  fromIntegral staffLines
        firstLedgerLineBelow  =  negate . fromIntegral $ staffLines

-- | Returns the type of ledger lines to draw above respectively below the ordinary staff lines
--   for a standard system.                                          
--
standardLedgers :: Direction -> [NoteHeadPos] -> Ledgers
standardLedgers = ledgers 5

engraveLedgers :: Ledgers -> Engraving
engraveLedgers ((la, sa), (lb, sb)) =
       (cat unitY          . replicate sa $ ledgerE) # translate (r2 (0,  3 * space))
    <> (cat (negate unitY) . replicate sb $ ledgerE) # translate (r2 (0, -3 * space))
    where  
        ledgerE = lineE <> spaceE
        lineE  = hrule (2 * space) # lineWidth ledgerLineWeight
        spaceE = spaceRect (2 * space) space


--
-- Chords
--

type Dots = Int
type FlipStem = Bool
type AdjustStem = Double
-- TODO type Flags = Int
-- TODO type CrossBeams = Int




-- | Engraves a chord.
--
--   


-- NOTE origin in middle of "correct" note column, at system line 0

engraveChord :: [(NoteHead, NoteHeadPos, Accidental)] -> Dots -> FlipStem -> AdjustStem -> Engraving
engraveChord = undefined

engraveNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
engraveNote pos dir nh = 
    moveToPosition pos $ 
        mempty
        <> noteHead 
        <> noteStem
        <> spacer
    where
        spacer     =  spaceRect (fst . unr2 $ noteHeadOffset) space
        noteHead   =  engraveSymbol (noteHeadSymbol nh) # translate (0.5 *^ noteHeadOffset)

        noteStem   =  if (noteHeadNeedsStem nh) then noteStem' else mempty
        noteStem'  =  rect noteStemWidth noteStemHeight 
                      # lc black
                      # fc black 
                      # moveOriginBy noteStemOffset
        
        noteStemHeight  =  space * 3.5 - noteStemShortenAtOuterNote

        noteHeadOffset  =  symbolSpacer (noteHeadSymbol nh)
        noteStemOffset  =  r2 $ (negate `onlyIf` (const dir)) (- (fst . unr2 $ noteHeadOffset / 2) - noteStemInset, 
                                                               space * 3.5 / 2 + (noteStemShortenAtOuterNote / 2))



