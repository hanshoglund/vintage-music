
{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

-- | Chord-level engraving.
module Notable.Engraving.Chord
(
NoteHead(..),
NoteHeadPos,
AccidentalType(..),
Accidental(..),
Dots,
FlipStem,
renderChord,
separeteNoteHeads,
assertNoPrimes,
renderNote,
hasStem,
noteSymbol,
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


-- | Represents a note head symbol.
data NoteHead 
    = Unfilled
    | Filled 
    | Whole 
    | Brevis

type NoteHeadPos = HalfSpaces

data AccidentalType 
    = Flat 
    | Natural 
    | Sharp

data Accidental 
    = NoAccidental 
    | NormalAccidental AccidentalType 
    | RedundantAccidental AccidentalType

type Dots = Int

type FlipStem = Bool

-- TODO cross-beams
-- TODO chord vertical lines

renderChord :: [(NoteHead, NoteHeadPos, Accidental)] -> Dots -> FlipStem -> Engraving
renderChord = undefined

-- | Separates note heads to be drawn to the left or the right of the stem.
--
--   Seconds are always partitioned so that the lower note heads goes to the right 
--   of the stem, while other notes are put on the default side of the note.

separeteNoteHeads :: Direction -> [Int] -> ([Int], [Int])
separeteNoteHeads d = separeteNoteHeads' d . assertNoPrimes . Data.List.sort

separeteNoteHeads' direction positions 
    | direction == upwards    =  (lowers `merge` others, uppers)
    | direction == downwards  =  (lowers, uppers `merge` others)
    where 
        (pairs, others)  =  partitioner collides positions
        (lowers, uppers) =  unzip pairs
        partitioner      =  if direction then partition2 else reversePartition2
        collides x y     =  y - x < 2

-- | Sanity check as we do not handle primes yet
assertNoPrimes :: [Int] -> [Int]
assertNoPrimes xs 
    | null (filter2 (==) xs)  =  xs
    | otherwise               =  error "assertNoPrimes"



renderNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
renderNote pos dir nh = 
    moveToPosition pos $ 
        mempty
        <> noteHead 
        <> noteStem
        <> spacer
    where
        spacer     =  spaceRect (fst . unr2 $ noteHeadOffset) space
        noteHead   =  baselineText noteGlyph # font noteFont # translate (0.5 *^ noteHeadOffset)

        noteStem   =  if (hasStem nh) then noteStem' else mempty
        noteStem'  =  rect noteStemWidth noteStemHeight 
                      # lc black
                      # fc black 
                      # moveOriginBy noteStemOffset
        
        noteStemHeight  =  space * 3.5 - noteStemShortenAtOuterNote

        noteHeadOffset  =  noteHeadAdjustment nh
        noteStemOffset  =  r2 $ negateIf (const dir) (- (fst . unr2 $ noteHeadOffset / 2) - noteStemInset, 
                                                      space * 3.5 / 2 + (noteStemShortenAtOuterNote / 2))
        
        (noteFont, noteGlyph)  =  noteSymbol nh

noteHeadAdjustment Filled   = r2 (-0.3, 0)
noteHeadAdjustment Unfilled = r2 (-0.3, 0)
noteHeadAdjustment Whole    = r2 (-0.43, 0)
noteHeadAdjustment Brevis   = r2 (-0.65, 0)

-- | Whether a given notehead should be drawn with a stem or not.
hasStem :: NoteHead -> Bool
hasStem Unfilled  =  True
hasStem Filled    =  True
hasStem Whole     =  False
hasStem Brevis    =  False

noteSymbol :: NoteHead -> Symbol
noteSymbol Filled    =  (specialMusicFont, "f")
noteSymbol Unfilled  =  (specialMusicFont, "F")
noteSymbol Whole     =  (baseMusicFont, "w")
noteSymbol Brevis    =  (baseMusicFont, "W")

                                               