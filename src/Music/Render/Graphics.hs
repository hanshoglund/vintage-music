{-|
    Module      :  Music.Render.Graphics
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Music.Render.Graphics
(
    Graphic,
    renderScore,
    writePdf
)
where

{-# LANGUAGE
    FlexibleContexts,
    TypeFamilies,
    NoMonomorphismRestriction #-}

import Prelude hiding (reverse)
import Diagrams.Prelude hiding ((|||), (===), render)
import qualified Diagrams.Prelude as D
import Diagrams.Backend.Cairo

import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector (unitX, unitY)
import Diagrams.Combinators
import Data.VectorSpace

import Music.Time
import Music.Time.Score
import Music.Internal.Time.Score (Score(..))

import Music.Time.Event
import qualified Music.Time.EventList as EventList
import Music.Utilities
import Data.Colour (withOpacity)
import Data.Colour.SRGB

-- | Opaque type representing a graphic representation.
newtype Graphic = Graphic (Diagram Cairo R2)


t2d :: Time t => t -> Double
t2d = time2Double

vert  = beside (negateV unitY)
horiz = append unitX


-- | Renders the given score as a graphic.
renderScore :: (Show a, Time t) => Score t a -> Graphic
renderScore s = let (d, s') = renderScore' 0 s in Graphic s'

renderScore' t (RestS d)      =  (d,
    if (d == 0)
        then mempty
        else moveOriginBy ((t2d d) * (-1), 0) ({-strutX-}hrule $ t2d d * 2))

renderScore' t (NoteS d x)    =  (d,
    if (d == 0)
        then mempty
        else
           moveOriginBy ((t2d d) * (-1), 0)
            (text (show x) # font "Gill Sans" # fc white <> (scaleX (t2d d) (square 2 # fcA (sRGB24read "465FBD" `withOpacity` 0.6)))))

renderScore' t (ParS x y)     =
    let (dx, sx) = renderScore' t x
        (dy, sy) = renderScore' t y
                              in (dx `max` dy,
    sx `vert` sy)

renderScore' t (SeqS x y)     =
    let (dx, sx) = renderScore' t x
        (dy, sy) = renderScore' (t + dx) y
                              in (dx + dy,
    sx `horiz` sy)


--
-- Imperative test stuff
--

-- | Writes the given graphic to a Pdf file.
writePdf :: FilePath -> Graphic -> IO ()
writePdf file (Graphic diagram) = do
    fst $ renderDia Cairo ( CairoOptions file $ PDF (500, 500) ) diagram
    return ()
