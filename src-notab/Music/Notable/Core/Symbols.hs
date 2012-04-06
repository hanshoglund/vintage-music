
-- | Notable symbols are simply represented as a pair of font and glyph.
module Music.Notable.Core.Symbols
(
    Font,
    Glyph,
    Symbol,
    Symbolic(..),
    baseMusicFont,
    specialMusicFont,
    symbolSpacer,
    symbolOffset
)
where

import Music.Notable.Core.Diagrams

class Symbolic a where
    symbol :: a -> Symbol

type Font   = String
type Glyph  = String
type Symbol = (Font, Glyph)

baseMusicFont :: Font
baseMusicFont    = "Helsinki"

specialMusicFont :: Font
specialMusicFont = "Helsinki Special"


-- symbol WholeNoteHeadNoteRest  =  (baseMusicFont, "\xd3")
-- symbol HalfNoteRest           =  (baseMusicFont, "\x2211") 
-- symbol QuarterNoteRest        =  (baseMusicFont, "\x152")
-- symbol EightNoteRest          =  (baseMusicFont, "\x2030")
-- symbol SixteenthNoteRest      =  (baseMusicFont, "\x2248")
-- symbol ThirtySecondNoteRest   =  (baseMusicFont, "\xae")

--
-- Spacing data
--

-- | Spacer for this symbol (offset from lower left to upper right corner of bounding rectangle).
symbolSpacer :: Symbol -> R2

-- Clefs
symbolSpacer ("Helsinki",         "&")     =  r2 (0.72,   1.95)
symbolSpacer ("Helsinki",         "B")     =  r2 (0.72,   1)
symbolSpacer ("Helsinki",         "?")     =  r2 (0.72,   1)

-- Rests
symbolSpacer ("Helsinki",         "\xd3")    =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "\x2211")  =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "\x152")   =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "\x2030")  =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "\x2248")  =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "\xae")    =  r2 (0.3, 0.3)

-- Note heads
symbolSpacer ("Helsinki Special", "f")     =  r2 (-0.305, 0.25)
symbolSpacer ("Helsinki Special", "F")     =  r2 (-0.308, 0.25)
symbolSpacer ("Helsinki",         "\207")  =  r2 (-0.305, 0.25)
symbolSpacer ("Helsinki",         "\250")  =  r2 (-0.308, 0.25)
symbolSpacer ("Helsinki",         "w")     =  r2 (-0.43,  0.25)
symbolSpacer ("Helsinki",         "W")     =  r2 (-0.65,  0.25)
symbolSpacer ("Helsinki",         "O")     =  r2 (-0.305, 0.25)
symbolSpacer ("Helsinki",         "\220")  =  r2 (-0.305, 0.25)
symbolSpacer ("Helsinki Special", "\89")   =  r2 (-0.305, 0.25)
symbolSpacer ("Helsinki Special", "\41")   =  r2 (-0.26, 0.25)
symbolSpacer ("Helsinki Special", "\54")   =  r2 (-0.26, 0.25)

-- Accidentals
symbolSpacer ("Helsinki",         "\x222b")  =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "b")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "n")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "#")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "\x2039")  =  r2 (0.3, 0.3)

-- Articulations
symbolSpacer ("Helsinki",         "U")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "u")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "+")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "o")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "v")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         ">")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         "-")     =  r2 (0.3, 0.3)
symbolSpacer ("Helsinki",         ".")     =  r2 (0.3, 0.3)




-- | Offset from origo to lower left corner of bounding rectangle.
symbolOffset :: Symbol -> R2

-- Clefs
symbolOffset ("Helsinki",         "&")     =  r2 (0.33,-0.18) ^+^ r2 (0, 0.5)
symbolOffset ("Helsinki",         "B")     =  r2 (0.34, 0)
symbolOffset ("Helsinki",         "?")     =  r2 (0.34, 0)

-- Rests
symbolOffset ("Helsinki",         "\xd3")    =  r2 (0.3, 0.3)
symbolOffset ("Helsinki",         "\x2211")  =  r2 (0.3, 0.3)
symbolOffset ("Helsinki",         "\x152")   =  r2 (0.3, 0.3)
symbolOffset ("Helsinki",         "\x2030")  =  r2 (0.3, 0.3)
symbolOffset ("Helsinki",         "\x2248")  =  r2 (0.3, 0.3)
symbolOffset ("Helsinki",         "\xae")    =  r2 (0.3, 0.3)

-- Note heads
symbolOffset ("Helsinki Special", "f")     =  r2 (0.305/2, 0)
symbolOffset ("Helsinki Special", "F")     =  r2 (0.305/2, 0)
symbolOffset ("Helsinki",         "\207")  =  r2 (0.305/2, 0)
symbolOffset ("Helsinki",         "\250")  =  r2 (0.308/2, 0)
symbolOffset ("Helsinki",         "w")     =  r2 (0.43/2, 0)
symbolOffset ("Helsinki",         "W")     =  r2 (0.65/2, 0)
symbolOffset ("Helsinki",         "O")     =  r2 (0.305/2, 0)
symbolOffset ("Helsinki",         "\220")  =  r2 (0.305/2, 0)
symbolOffset ("Helsinki Special", "\89")   =  r2 (0.305/2, 0)
symbolOffset ("Helsinki Special", "\41")   =  r2 (0.26/2, 0)
symbolOffset ("Helsinki Special", "\54")   =  r2 (0.26/2, 0)

-- Accidentals
symbolOffset ("Helsinki",         "\x222b")  =  r2 (0, 0)
symbolOffset ("Helsinki",         "b")       =  r2 (0, 0)
symbolOffset ("Helsinki",         "n")       =  r2 (0, 0)
symbolOffset ("Helsinki",         "#")       =  r2 (0, 0)
symbolOffset ("Helsinki",         "\x2039")  =  r2 (0, 0)

-- Articulations
symbolOffset ("Helsinki",         "U")     =  r2 (0, 0)
symbolOffset ("Helsinki",         "u")     =  r2 (0, 0)
symbolOffset ("Helsinki",         "+")     =  r2 (0, 0)
symbolOffset ("Helsinki",         "o")     =  r2 (0, 0)
symbolOffset ("Helsinki",         "v")     =  r2 (0, 0)
symbolOffset ("Helsinki",         ">")     =  r2 (0, 0)
symbolOffset ("Helsinki",         "-")     =  r2 (0, 0)
symbolOffset ("Helsinki",         ".")     =  r2 (0, 0)











-- Opus offsets
-- Clefs
-- symbolOffset ("Opus",             "&")     =  r2 (0.33,-0.18) ^+^ r2 (0, 0.5)
-- symbolOffset ("Opus",             "B")     =  r2 (0.34, 0)
-- symbolOffset ("Opus",             "?")     =  r2 (0.34, 0)

-- Opus spacer
-- Clefs
-- symbolSpacer ("Opus",             "&")     =  r2 (0.72, 1.95)
-- symbolSpacer ("Opus",             "B")     =  r2 (0.72, 1)
-- symbolSpacer ("Opus",             "?")     =  r2 (0.72, 1)

-- Note heads
-- symbolSpacer ("Opus Special",     "f")     =  r2 (-0.36, 0.25)
-- symbolSpacer ("Opus Special",     "F")     =  r2 (-0.36, 0.25)
-- symbolSpacer ("Opus",             "\207")  =  r2 (-0.36, 0.25)
-- symbolSpacer ("Opus",             "\250")  =  r2 (-0.36, 0.25)
-- symbolSpacer ("Opus",             "w")     =  r2 (-0.46, 0.25)
-- symbolSpacer ("Opus",             "W")     =  r2 (-0.64, 0.25)



