
{-# LANGUAGE
    MultiParamTypeClasses,
    NoMonomorphismRestriction #-}

import Data.Convert
import Data.Indexed
import Data.Trivial

import Music
import Music.Inspect
import Music.Render.Graphics
import Music.Util
import Music.Util.List

import Music.Notable.Core
import Music.Notable.Core.Diagrams
import Music.Notable.Engraving






-- | Column of accidentals, numbered from right to left. 
--   The column closest to the note heads has number zero. 
type AccidentalColumn = Int

-- TODO assumes the list is sorted by index in increasing order 

separateAccidentals :: [(AccidentalPosition, Accidental)] -> [(AccidentalColumn, AccidentalPosition, Accidental)]
separateAccidentals = foldr addColumn []

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
        





renderN :: Notation -> Engraving
renderN _ = mempty
    <> allE
    `below` (scale 1.2 . center 6 $ minorE)
    `below` chord2E

minorE = mempty
    <> (alignL $ noteLines # scaleX 12) 
    <> (catRight $ map (engraveNoteHead 0) $ (++ [DiamondNoteHead, CrossNoteHead, CircledCrossNoteHead, UnfilledSquareNoteHead, FilledSquareNoteHead]) $ map (noteHeadFromNoteValue) [1,1/2,1/4,1/8,1/16])
    `leftTo` strutX 1
    `leftTo` (catRight $ map (engraveRest . restFromNoteValue) [1,1/2,1/4,1/8,1/16])
    `leftTo` strutX 1
    `leftTo` (catRight $ map engraveAccidental $ enumFromTo minBound maxBound)
    `leftTo` strutX 1
    `leftTo` (catRight $ map engraveArticulation $ enumFromTo minBound maxBound)

chord2E = mempty
    <> noteLines # scaleX 10
    <> drawNotes up
    <> drawNotes down # translate (r2 (3, 0))
    where
        drawNotes stemDir = mempty
            <> engraveStem stemDir notes
            <> engraveNoteHeads stemDir notes
            <> engraveLedgerLines (ledgerLines stemDir (map fst notes))
        notes = 
            [
                (2, UnfilledSquareNoteHead),
                (1, FilledSquareNoteHead),
                (0, DiamondNoteHead),
                (-7, FilledNoteHead),
                (-6, FilledNoteHead),
                (-12, UnfilledNoteHead)
            ]
             
---------




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
    <> engraveClef trebleClef # translate (r2 (-2,0))
    <> engraveNote 10 down UnfilledNoteHead
    <> engraveNote (-9) up UnfilledNoteHead
    <> engraveLedgerLines (ledgerLines up [-9,2,3,10])



allE = rotate (10 :: Deg) clefE `above` (scale (1/4) . freeze) chordE `above` chordE `above` ledgersE

chordE = mempty
    <> noteLines # scaleX 15
    <> (         engraveClef altoClef
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
        `leftTo` engraveClef frenchClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef trebleClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef sopranoClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef mezzoSopranoClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef altoClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef tenorClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef baritoneClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef bassClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef subBassClef
       ) # translate (r2 (-5, 0))




-- Instance so we can use 'draw'
instance Render Notation Graphic where
    render = Graphic . renderN


