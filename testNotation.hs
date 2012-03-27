
{-# LANGUAGE 
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    MultiParamTypeClasses,
    RankNTypes,
    NoMonomorphismRestriction #-}

import Music                  
import Music.Inspect
import Music.Render
import Music.Render.Graphics
import Notable.Core
import Notable.Core.Diagrams
import Notable.Engraving.Chord
import Notable.Engraving.Staff


renderNotation :: Notation -> Engraving
renderNotation x = mempty
    <> noteLines # showOrigin
    <> renderNote 0 False Filled
    <> renderNote (-3) True Brevis  # translate (r2 (2, 0))
    <> renderNote (-1) True Whole   # translate (r2 (3, 0))
    <> renderNote 1 True Unfilled   # translate (r2 (4, 0))
    <> renderNote 3 True Filled     # translate (r2 (5, 0))

instance Render Notation Graphic where
    render = Graphic . renderNotation
