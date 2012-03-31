
{-# LANGUAGE  
    TypeFamilies,
    RankNTypes,  
    MultiParamTypeClasses,
    NoMonomorphismRestriction,
    FlexibleContexts #-}

import Music
import Music.Inspect
import Music.Render
import Music.Render.Graphics
import Notable.Core
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
    <> noteLines 4
    <> trebleClef # translate (r2 (-2,0))
    <> engraveNote 6 False Unfilled
    <> engraveNote (-25) True Unfilled
    <> engraveLedgerLines ((0,3),(0,10))




allE = rotate (10 :: Deg) clefE === (scale (1/4) . freeze) chordE === chordE === ledgersE

chordE = mempty
    <> noteLines 15
    <> (
        altoClef
        =>= strutX 0.5
        =>= engraveNote 1 False Unfilled
        =>= strutX 1
        =>= engraveNote 2 False Unfilled
        =>= strutX 1
        =>= engraveNote 0 False Unfilled
        =>= strutX 1
        =>= engraveNote (-1) True Unfilled
        =>= strutX 1
        =>= engraveNote (-5) True Unfilled
        =>= strutX 1
        =>= engraveNote (-3) True Filled
        =>= strutX 1
        =>= engraveNote 0 True Whole
        =>= strutX 1
        =>= engraveNote 3 True Brevis
    ) # translate (r2 (-7.2, 0))


clefE = mempty
    <> noteLines 15
    <> (
        mempty
        =>= frenchClef
        =>= strutX 0.5
        =>= trebleClef
        =>= strutX 0.5
        =>= sopranoClef
        =>= strutX 0.5
        =>= mezzoSopranoClef
        =>= strutX 0.5
        =>= altoClef
        =>= strutX 0.5
        =>= tenorClef
        =>= strutX 0.5
        =>= baritoneClef
        =>= strutX 0.5
        =>= bassClef
        =>= strutX 0.5
        =>= subBassClef
       ) # translate (r2 (-5, 0)) 






