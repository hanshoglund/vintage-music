
{-# LANGUAGE  
    TypeFamilies,
    RankNTypes,  
    MultiParamTypeClasses,
    TypeSynonymInstances,
    NoMonomorphismRestriction,
    FlexibleContexts #-}

import Music
import Music.Inspect
import Music.Render
import Music.Render.Graphics
import Notable.Core
import Notable.Spacing
import Notable.Core.Diagrams
import Notable.Engraving.Chord
import Notable.Engraving.Staff
import Music.Util.List

import Data.Indexed



-- Instance so we can use 'draw'
instance Render Notation Graphic where
    render = Graphic . renderN
renderN :: Notation -> Engraving

renderN _ = allE <> arcE


arcE = mempty
    <> noteLines # scaleX 4
    <> hc2 # rotate ((0) :: Deg) # translate (r2 (0, 1.5 * space))
--    <> (lw 0.05 $ hc)  # rotate ((-12) :: Deg) # translate (r2 (0, -1))
    <> engraveNote 0 False UnfilledNoteHead # translate (r2 (-1, 0))
    <> engraveNote (-0) False UnfilledNoteHead # translate (r2 (1, 0))
    where                                                          
        hc2 = caligraphy 10 $ hc
        hc = scaleY 0.34 $ lw 0.04 $ stroke $ arc (0.45 * tau :: Rad) (0.05 * tau :: Rad)

-- TODO this does not scale properly, i fear we need to render two close arcs and fill the space inbetween 
caligraphy x = id
--caligraphy x = scaleX (1/x) . freeze . scaleX x 


ledgersE = mempty
    <> noteLines # scaleX 4
    <> trebleClef # translate (r2 (-2,0))
    <> engraveNote 10 False UnfilledNoteHead
    <> engraveNote (-9) True UnfilledNoteHead
    <> engraveLedgerLines (ledgerLines True [-9,2,3,10])



allE = rotate (10 :: Deg) clefE `above` (scale (1/4) . freeze) chordE `above` chordE `above` ledgersE

chordE = mempty
    <> noteLines # scaleX 15
    <> (
        altoClef
        `leftTo` strutX 0.5
        `leftTo` engraveNote 1 False UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 2 False UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 0 False UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-1) True UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-5) True UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-3) True FilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 0 True WholeNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 3 True BrevisNoteHead
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






