{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

module Notable.Engraving.Chord
where

import Music.Util
    
import Notable.Core
import Notable.Core.Diagrams
import Notable.Core.Glyphs


renderNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
renderNote pos dir nh = 
    translate (r2 (0, space * pos / 2)) $ mempty
    <> noteHead 
    <> noteStem
    <> spacer
    where
        spacer     =  spaceRect (fst . unr2 $ noteHeadOffset) space
        noteHead   =  baselineText noteGlyph # font noteFont # translate (0.5 *^ noteHeadOffset)

        noteStem   =  if (hasStem nh) then noteStem' else mempty
        noteStem'  =  rect noteStemWidth noteStemHeight 
                      # fc black 
                      # moveOriginBy noteStemOffset
        
        noteStemWidth   =  0.025
        noteStemHeight  =  space * 3.5

        noteHeadOffset  =  noteHeadAdjustment nh
        noteStemOffset  =  r2 $ negateUnless dir (- fst . unr2 $ noteHeadOffset / 2, space * 3.5 / 2)
        
        (noteFont, noteGlyph)  =  noteSymbol nh

spaceRect x y = rect x y # fc blue # opacity 0.0

noteHeadAdjustment Filled   = r2 (-0.3, 0)
noteHeadAdjustment Unfilled = r2 (-0.3, 0)
noteHeadAdjustment Whole    = r2 (-0.43, 0)
noteHeadAdjustment Brevis   = r2 (-0.65, 0)

-- | Represents a note head symbol.
data NoteHead 
    = Unfilled
    | Filled 
    | Whole 
    | Brevis

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

                                               