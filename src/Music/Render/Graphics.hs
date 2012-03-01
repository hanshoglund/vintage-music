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
    TypeFamilies,
    NoMonoMorphismRestriction #-}

import Prelude hiding ( reverse )

import Data.Colour ( withOpacity )
import Data.Colour.SRGB ( sRGB24read )

import Diagrams.Prelude hiding ( (|||), (===), render )
import Diagrams.Backend.Cairo

import Music.Time
import Music.Time.Score
import Music.Internal.Time.Score ( foldScore )


-- | Opaque type representing a graphic representation.
newtype Graphic = Graphic (Diagram Cairo R2)


-- | Renders the given score as a graphic.
renderScore :: (Show a, Time t) => Score t a -> Graphic
renderScore = Graphic 
            . foldScore (\t d   -> renderRest d)
                        (\t d x -> renderNote d x)
                        (\t x y -> renderPar x y)
                        (\t x y -> renderSeq x y)
    where
        renderRest d   | d == 0     =  mempty
                       | otherwise  =  moveOriginBy (negate (t2d d), 0) (renderEmpty (t2d d * 2))

        renderNote d x | d == 0     =  mempty
                       | otherwise  =  moveOriginBy (negate (t2d d), 0) (renderText x <> renderBox (t2d d))

        renderPar     =  beside (negateV unitY)
        renderSeq     =  append unitX
        renderEmpty   =  strutX
        renderText x  =  text (show x) # font "Gill Sans"
                                       # fc white
        renderBox d   =  scaleX d . fcA boxColor $ square 2
        boxColor      =  sRGB24read "465FBD" `withOpacity` 0.6



t2d :: Time t => t -> Double
t2d   = time2Double



-- Imperative test stuff

-- | Writes the given graphic to a Pdf file.
writePdf :: FilePath -> Graphic -> IO ()
writePdf file (Graphic diagram) = do
    fst $ renderDia Cairo ( CairoOptions file $ PDF (500, 500) ) diagram
    return ()
