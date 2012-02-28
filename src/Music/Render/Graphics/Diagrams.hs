{-|
    Module      :  Music.Render.Graphics.Diagrams
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Music.Render.Graphics.Diagrams
-- (
-- )
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
import Music.Time.Event
import qualified Music.Time.EventList as EventList
import Music.Utilities

type Graphic = Diagram Cairo R2
                             
-- ver = beside (negateV unitY)
-- hor = beside unitX

phrase = line [60, 65, 67, 60, 55, 62] :: Score Double Int

test :: Score Double Int
test =  
        lineStretch (zip (map cos [0..20]) [0..20])
    ||| lineStretch (zip (map sin [0..20]) [0..20])


--     line [1..3] ||| line [1..2] ||| reverse (line [1..3])
-- 
-- 
-- --    lineStretch (zip (map cos [1..10]) [0..10])
-- --    ||| lineStretch (zip (map sin [1..10]) [0..10])
--     -- note 3
    ||| rest pi >>> note 5
    ||| arpeggio 0.1 [0..5]
    ||| line [1..10]
    ||| line [10..16]
    ||| reverse (line [11,12,13,14] ||| line [22,23])
    ||| stretch 1.3 (line [5,6,7])
    ||| note 1
    ||| stretch 2 (note 2)  
    ||| concatSeq [line [1,2,3], chord [4,5], line [6,7,8]]
    ||| note 1
    ||| concatSeq [line [1,2,3], chord [4,5], line [6,7,8]]
--    loop phrase ||| (stretch 1.01 $ loop phrase)

t2d :: Time t => t -> Double
t2d = time2Double

horiz = append (negateV unitY)
vert = append unitX
--orig  = showOrigin


renderScore :: (Show a, Time t) => Score t a -> Graphic
renderScore s = let (d, s') = renderScore' 0 s in s'

renderScore' t (RestS d)      =  (d, 
    if (d == 0) 
        then mempty 
        else moveOriginBy ((t2d d) * (-1), 0) (strutX $ t2d d * 2)
    # id)

renderScore' t (NoteS d x)    =  (d,                  
    if (d == 0)
        then mempty
        else
           moveOriginBy ((t2d d) * (-1), 0) 
            (text (show x) <> (scaleX (t2d d) (square 2))) 
    # id)

renderScore' t (ParS x y)     =
    let (dx, sx) = renderScore' t x
        (dy, sy) = renderScore' t y
                              in (dx `max` dy, 
    sx `horiz` sy 
    # id)

renderScore' t (SeqS x y)     =
    let (dx, sx) = renderScore' t x
        (dy, sy) = renderScore' (t + dx) y
                              in (dx + dy, 
    sx `vert` sy 
    # id)



-- renderScoreGraphic :: Time t => Score t a -> Graphic
-- renderScoreGraphic =
--     mconcat . map renderEvent . EventList.events . render
--
-- renderEvent (Event t d x) =
--     square (t2d d) # alignX (t2d t)
--
-- Imperative test stuff
--

writeGraphics :: FilePath -> Graphic -> IO ()
writeGraphics file diagram = do
    fst $ renderDia Cairo ( CairoOptions file $ PDF (500, 500) ) diagram
    return ()

main = do
    writeGraphics "test.pdf" (renderScore test)
    openFile "test.pdf"
    return ()