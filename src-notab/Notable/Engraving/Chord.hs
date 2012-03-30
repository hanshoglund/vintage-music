
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

-- | Separates note heads to be drawn to the left and right of the stem respectively.
--
--   Seconds are partitioned so that the lower note heads goes to the right of the stem, 
--   while all other notes are put on the default side of the note. The same layout 
--   is used whether the chord actually has a stem or not. (Tyboni II.1 p 91)
--
separeteNoteHeads :: (Ord a, Num a) => Direction -> [a] -> ([a], [a])
separeteNoteHeads d = separeteNoteHeads' d . assertNoPrimes . Data.List.sort
    where
        -- Sanity check as we do not handle primes yet
        assertNoPrimes xs 
            | null (filter2 (==) xs)  =  xs
            | otherwise               =  error "assertNoPrimes"
        
        separeteNoteHeads' direction positions 
            | direction  =  ( lowers `merge` others, uppers ) 
            | otherwise  =  ( lowers, uppers `merge` others )
                where 
                    (pairs, others)  =  partitioner collides positions
                    (lowers, uppers) =  unzip pairs
                    collides x y     =  y - x < 2
                    partitioner
                        | direction  =  partition2
                        | otherwise  =  reversePartition2





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



