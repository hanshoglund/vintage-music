
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


-- | Spacer for this symbol (offset from lower left to upper right corner of bounding rectangle).
--   Has to be entered manually at the moment.
symbolSpacer :: Symbol -> R2

-- symbolSpacer ("Modusnotes", x) = symbolSpacer ("Helsinki", x)

-- symbolSpacer ("Petrucci", "\339")  =  r2 (-0.36, 0)
-- symbolSpacer ("Petrucci", "\729")  =  r2 (-0.36, 0)
-- symbolSpacer ("Petrucci", x) = symbolSpacer ("Helsinki", x)

-- symbolSpacer ("Feta", x) = symbolSpacer ("Helsinki", x)

symbolSpacer ("Opus Special",     "f")     =  r2 (-0.36, 0)
symbolSpacer ("Opus Special",     "F")     =  r2 (-0.36, 0)
symbolSpacer ("Opus",             "\207")  =  r2 (-0.36, 0)
symbolSpacer ("Opus",             "\250")  =  r2 (-0.36, 0)
symbolSpacer ("Opus",             "w")     =  r2 (-0.46, 0)
symbolSpacer ("Opus",             "W")     =  r2 (-0.64, 0)
symbolSpacer ("Opus",             "&")     =  r2 (0.72, 1.95) -- should Y be 1 as this is a clef?
symbolSpacer ("Opus",             "B")     =  r2 (0.72, 1)
symbolSpacer ("Opus",             "?")     =  r2 (0.72, 1)

symbolSpacer ("Helsinki Special", "f")     =  r2 (-0.305, 0)
symbolSpacer ("Helsinki Special", "F")     =  r2 (-0.308, 0)
symbolSpacer ("Helsinki",         "\207")  =  r2 (-0.305, 0)
symbolSpacer ("Helsinki",         "\250")  =  r2 (-0.308, 0)
symbolSpacer ("Helsinki",         "w")     =  r2 (-0.43, 0)
symbolSpacer ("Helsinki",         "W")     =  r2 (-0.65, 0)
symbolSpacer ("Helsinki",         "&")     =  r2 (0.72, 1.95) -- should Y be 1 as this is a clef?
symbolSpacer ("Helsinki",         "B")     =  r2 (0.72, 1)
symbolSpacer ("Helsinki",         "?")     =  r2 (0.72, 1)



-- | Offset from origo to lower left corner of bounding rectangle.
symbolOffset :: Symbol -> R2

-- symbolOffset ("Modusnotes", x) = symbolOffset ("Helsinki", x)
-- symbolOffset ("Petrucci", x) = symbolOffset ("Helsinki", x)
-- symbolOffset ("Feta", x) = symbolOffset ("Helsinki", x)


symbolOffset ("Helsinki",         "&")     =  r2 (0.33,-0.18) ^+^ r2 (0, 0.5)
symbolOffset ("Helsinki",         "B")     =  r2 (0.34, 0)
symbolOffset ("Helsinki",         "?")     =  r2 (0.34, 0)

symbolOffset ("Opus",             "&")     =  r2 (0.33,-0.18) ^+^ r2 (0, 0.5)
symbolOffset ("Opus",             "B")     =  r2 (0.34, 0)
symbolOffset ("Opus",             "?")     =  r2 (0.34, 0)


