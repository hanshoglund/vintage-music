
-- | Notable symbols are simply represented as a pair of font and glyph.
module Notable.Core.Symbols
(
Font,
Glyph,
Symbol,
baseMusicFont,
specialMusicFont
)
where

import Notable.Core.Diagrams

type Font   = String
type Glyph  = String
type Symbol = (Font, Glyph)

baseMusicFont :: Font
baseMusicFont    = "Helsinki"

specialMusicFont :: Font
specialMusicFont = "Helsinki Special"
