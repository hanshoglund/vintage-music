{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

module Notable.Engraving.Chord
where

import Music.Util
    
import Notable.Core
import Notable.Core.Diagrams
import Notable.Core.Glyphs

noteStemWidth               =  0.025
noteStemInset               =  0.013
noteStemShortenAtOuterNote  =  0.1 * space

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
        noteStemOffset  =  r2 $ negateUnless dir (- (fst . unr2 $ noteHeadOffset / 2) - noteStemInset, 
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

                                               