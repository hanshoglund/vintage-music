
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


-- Instance so we can use 'draw'
instance Render Notation Graphic where
    render = Graphic . renderNot
renderNot :: Notation -> Engraving
renderNot x = rotate (10 :: Deg) (clefE x) === (scale (1/4) . freeze) (chordE x) === chordE x

chordE _ = mempty
    <> standardNoteLines 15
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


clefE _ = mempty
    <> standardNoteLines 15
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

