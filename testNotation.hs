
{-# LANGUAGE
    TypeFamilies,
    RankNTypes,
    MultiParamTypeClasses,
    TypeSynonymInstances,
    NoMonomorphismRestriction,
    FlexibleContexts #-}

import Numeric(showHex)
import Data.Convert
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

import Graphics.SVGFonts.ReadFont (textSVG,textSVG_, outlMap, TextOpts(..))

-- Instance so we can use 'draw'
instance Render Notation Graphic where
    render = Graphic . renderN
renderN :: Notation -> Engraving

fdo2 = outlMap $ "/Users/hans/Downloads/Helsinki/fonts/Helsinki.svg"
text' t = stroke (textSVG_ with { txt = t, textHeight = 3, fdo = fdo2}) # lineWidth 0 # fillColor black
engraveSymbolFloating' (font', glyph) = font font' $ text' glyph


helChars =
    [ "\xa5;"        
    , "\xe01e;"      
    , "\x25ca;"      
    , "\x2260;"      
    , "\x221e;"      
    , "\x221a;"      
    , "\x220f;"      
    , "\x2202;"      
    , "\x2122;"      
    , "\x2044;"      
    , "\x2026;"      
    , "\x3c0;"       
    , "\x3a9;"       
    , "\x394;"       
    , "\x192;"       
    , "\x131;"       
    , "\xff;"        
    , " "             
    , "\x22;"        
    , "#"             
    , "%"             
    , "\x26;"        
    , "'"             
    , "*"             
    , "+"             
    , ","             
    , "-"             
    , "."             
    , "/"             
    , "0"             
    , "1"             
    , "2"             
    , "3"             
    , "4"             
    , "5"             
    , "6"             
    , "7"             
    , "8"             
    , "9"             
    , "\x3e;"        
    , "?"             
    , "B"             
    , "C"             
    , "E"             
    , "H"             
    , "J"             
    , "K"             
    , "M"             
    , "O"             
    , "Q"             
    , "R"             
    , "T"             
    , "U"             
    , "W"             
    , "X"             
    , "]"             
    , "^"             
    , "b"             
    , "c"             
    , "e"             
    , "h"             
    , "j"             
    , "k"             
    , "m"             
    , "n"             
    , "o"             
    , "q"             
    , "r"             
    , "u"             
    , "v"             
    , "w"             
    , "x"             
    , "~"             
    , "\xb0;"        
    , "\xae;"        
    , "\xc6;"        
    , "\x2264;"      
    , "\x2265;"      
    , "\xb5;"        
    , "\x2211;"      
    , "\x222b;"      
    , "\xbf;"        
    , "\x2248;"      
    , "\x152;"       
    , "\x153;"       
    , "\x2018;"      
    , "\x2019;"      
    , "\xf7;"        
    , "\x178;"       
    , "\x20ac;"      
    , "\x2039;"      
    , "\x203a;"      
    , "\xfb01;"      
    , "\x201a;"      
    , "\x2030;"      
    , "\xc2;"        
    , "\xc1;"        
    , "\xd3;"        
    , "\xd4;"        
    , "\xd9;"        
    , "\x2d9;"       
    , "\xb8;"        
    , "\x2dd;" 
    ]


renderN _ = mempty
    -- <> allE
    -- <> arcE # translate (r2 (0,3.5))
    -- <> chord2E # translate (r2 (0,7))
--    <> symbolsE                 
    <> noteLines # scaleX 30 
    <> engraveRest WholeNoteRest
    <> engraveAccidental DoubleSharp
    <> engraveArticulation Fermata
    <> rect 1 1

--    <> rect 4 4
--    <> engraveSymbolFloating ("Helsinki", "\xd9")
    -- `leftTo` symbolImage "quarter"
    -- `leftTo` symbolImage "eigth"
    -- `leftTo` symbolImage "sixteenth"
    where
        -- symbolImage name = scale 0.43 $ image ("/Users/hans/Documents/Kod/hs/Music/symbols/" ++ name ++ ".png") 4 4

symbolsE = catDown . map (catRight) $ rows
    where
        cells = map cell helChars
        rows = divide 16 cells
        cell x = rect 4 4
           <> translate (r2 (-1.5,0.5))   (engraveSymbolFloating ("Arial", (flip showHex "") . fromEnum $ head x)) 
            -- <> translate (r2 (-1.5,-1))  (engraveSymbolFloating ("Arial", [toEnum x])) 
            <> translate (r2 (1,1)) (engraveSymbolFloating ("Helsinki", x))
--            <> translate (r2 (1,-1))  (engraveSymbolFloating ("Helsinki Special", [toEnum x]))

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






