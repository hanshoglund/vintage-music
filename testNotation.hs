
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
renderNot x = clefE x === chordE x

chordE _ = mempty
    <> standardNoteLines 15
    <> (
        altoClef
        =>= strutX 0.5
        =>= renderNote 1 False Unfilled
        =>= strutX 1
        =>= renderNote 2 False Unfilled
        =>= strutX 1
        =>= renderNote 0 False Unfilled
        =>= strutX 1
        =>= renderNote (-1) True Unfilled
        =>= strutX 1
        =>= renderNote (-5) True Unfilled
        =>= strutX 1
        =>= renderNote (-3) True Filled
        =>= strutX 1
        =>= renderNote 0 True Whole
        =>= strutX 1
        =>= renderNote 3 True Brevis
    ) # translate (r2 (-7.2, 0))


clefE _ = mempty
    <> standardNoteLines 15
    <> (
        mempty
        =>= frenchClef
        =>= trebleClef
        =>= sopranoClef
        =>= mezzoSopranoClef
        =>= altoClef
        =>= tenorClef
        =>= baritoneClef
        =>= bassClef
        =>= subBassClef
       ) 

