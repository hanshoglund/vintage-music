
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




-- Instance so we can use 'draw'
instance Render Notation Graphic where
    render = Graphic . renderN
renderN :: Notation -> Engraving

renderN _ = allE



ledgersE = mempty
    <> noteLines # scaleX 4
    <> trebleClef # translate (r2 (-2,0))
    <> engraveNote 10 False Unfilled
    <> engraveNote (-9) True Unfilled
    <> engraveLedgerLines (ledgerLines True [-9,2,3,10])



allE = rotate (10 :: Deg) clefE `above` (scale (1/4) . freeze) chordE `above` chordE `above` ledgersE

chordE = mempty
    <> noteLines # scaleX 15
    <> (
        altoClef
        `leftTo` strutX 0.5
        `leftTo` engraveNote 1 False Unfilled
        `leftTo` strutX 1
        `leftTo` engraveNote 2 False Unfilled
        `leftTo` strutX 1
        `leftTo` engraveNote 0 False Unfilled
        `leftTo` strutX 1
        `leftTo` engraveNote (-1) True Unfilled
        `leftTo` strutX 1
        `leftTo` engraveNote (-5) True Unfilled
        `leftTo` strutX 1
        `leftTo` engraveNote (-3) True Filled
        `leftTo` strutX 1
        `leftTo` engraveNote 0 True Whole
        `leftTo` strutX 1
        `leftTo` engraveNote 3 True Brevis
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






