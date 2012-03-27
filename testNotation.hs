
{-# LANGUAGE  
    TypeFamilies,
    RankNTypes,  
    MultiParamTypeClasses,
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
    render = Graphic . clefs



       -- # translate (r2 (-5,0))
    -- <> doubleBarLine # translate (r2 (-2,0))
    -- <> renderNote 0 False Filled
    -- <> renderNote (-3) True Brevis  # translate (r2 (2, 0))
    -- <> renderNote (-1) True Whole   # translate (r2 (3, 0))
    -- <> renderNote 1 downwards Unfilled   # translate (r2 (4, 0))
    -- <> renderNote 3 downwards Filled     # translate (r2 (5, 0))


clefs _ = mempty
    <> standardNoteLines 15
    <> (
        frenchClef
        =>= trebleClef
        =>= sopranoClef
        =>= mezzoSopranoClef
        =>= altoClef
        =>= tenorClef
        =>= baritoneClef
        =>= bassClef
        =>= subBassClef
       ) 

