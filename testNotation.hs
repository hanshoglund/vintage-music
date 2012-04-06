
{-# LANGUAGE
    TypeFamilies,
    RankNTypes,
    MultiParamTypeClasses,
    TypeSynonymInstances,
    NoMonomorphismRestriction,
    FlexibleContexts #-}

import Data.Convert
import Data.Indexed
import Data.Ord ( comparing )
import qualified Data.List

import Music
import Music.Inspect
import Music.Render
import Music.Render.Graphics
import Music.Util.List

import Music.Notable.Core
import Music.Notable.Core.Symbols
import Music.Notable.Spacing
import Music.Notable.Core.Diagrams
import Music.Notable.Engraving.Chord
import Music.Notable.Engraving.Staff



-- Instance so we can use 'draw'
instance Render Notation Graphic where
    render = Graphic . renderN
renderN :: Notation -> Engraving

renderN _ = mempty
    -- <> allE
--    <> arcE
    <> chord2E

chord2E = mempty
    <> noteLines # scaleX 10
    <> engraveNoteHeads2 up
        [
            (2, UnfilledSquareNoteHead),
            (1, FilledSquareNoteHead),
            (0, DiamondNoteHead),
            (-7, FilledNoteHead),
            (-6, FilledNoteHead),
            (-12, UnfilledNoteHead)
        ]
    <> engraveLedgerLines (ledgerLines up [2,-12])
    <> (engraveNoteHeads2 down
        [
            (2, UnfilledSquareNoteHead),
            (1, FilledSquareNoteHead),
            (0, DiamondNoteHead),
            (-7, FilledNoteHead),
            (-6, FilledNoteHead),
            (-12, UnfilledNoteHead)
        ]
        <> engraveLedgerLines (ledgerLines up [2,-12])
        ) # translate (r2 (1, 0))


type Dup a = (a, a)

engraveNoteHead :: NoteHeadPosition -> NoteHead -> Engraving
engraveNoteHead pos nh =
    moveHalfSpacesUp pos $ position $ engraveSymbolFloating (symbol nh)
        where { position = translate (0.5 *^ symbolSpacer (symbol nh)) }

engraveNoteHeads2 :: Direction -> [(NoteHeadPosition, NoteHead)] -> Engraving
engraveNoteHeads2 stemDir noteHeads =
    let (poses, heads)          = unzip (Data.List.sortBy (comparing fst) noteHeads)
        (leftPoses, rightPoses) = separateNoteHeads stemDir poses
        (lefts, rights)         = mergeZip leftPoses heads rightPoses
        leftSide  = mconcat . fmap (\(p,n) -> engraveNoteHead p n) $ lefts
        rightSide = mconcat . fmap (\(p,n) -> engraveNoteHead p n) $ rights
      in leftSide <> translate (r2 (0.3, 0)) rightSide
-- FIXME engraveNoteHeads should have standard column as origin!


arcE = mempty
    <> noteLines # scaleX 4
    <> hc2 # rotate ((0) :: Deg) # translate (r2 (0, 1.5 * convert space))
--    <> (lw 0.05 $ hc)  # rotate ((-12) :: Deg) # translate (r2 (0, -1))
    <> engraveNote 0 up UnfilledNoteHead # translate (r2 (-1, 0))
    <> engraveNote (-0) down UnfilledNoteHead # translate (r2 (1, 0))
    where
        hc2 = caligraphy 10 $ hc
        hc = scaleY 0.34 $ lw 0.04 $ stroke $ arc (0.45 * tau :: Rad) (0.05 * tau :: Rad)

-- TODO this does not scale properly, i fear we need to render two close arcs and fill the space inbetween
caligraphy x = id
--caligraphy x = scaleX (1/x) . freeze . scaleX x


ledgersE = mempty
    <> noteLines # scaleX 4
    <> trebleClef # translate (r2 (-2,0))
    <> engraveNote 10 down UnfilledNoteHead
    <> engraveNote (-9) up UnfilledNoteHead
    <> engraveLedgerLines (ledgerLines up [-9,2,3,10])



allE = rotate (10 :: Deg) clefE `above` (scale (1/4) . freeze) chordE `above` chordE `above` ledgersE

chordE = mempty
    <> noteLines # scaleX 15
    <> (
        altoClef
        `leftTo` strutX 0.5
        `leftTo` engraveNote 1 down UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 2 down UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 0 down UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-1) up UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-5) up UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-3) up FilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 0 up WholeNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 3 up BrevisNoteHead
    ) # translate (r2 (-7.2, 0))


clefE = mempty
    <> noteLines # scaleX 15
    <> (
        mempty
        `leftTo` frenchClef
        `leftTo` strutX 0.5
        `leftTo` trebleClef
        `leftTo` strutX 0.5
        `leftTo` sopranoClef
        `leftTo` strutX 0.5
        `leftTo` mezzoSopranoClef
        `leftTo` strutX 0.5
        `leftTo` altoClef
        `leftTo` strutX 0.5
        `leftTo` tenorClef
        `leftTo` strutX 0.5
        `leftTo` baritoneClef
        `leftTo` strutX 0.5
        `leftTo` bassClef
        `leftTo` strutX 0.5
        `leftTo` subBassClef
       ) # translate (r2 (-5, 0))






