{-|
    Module      :  Music.Render.Graphics
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances #-}

module Music.Render.Graphics
(
    Graphic(..),
    writeGraphics
)
where

import Prelude hiding ( reverse )

import Data.Monoid
import Data.Colour ( withOpacity )
import Data.Colour.SRGB ( sRGB24read )
import Data.Convert

import Diagrams.Prelude hiding ( Render, Time, render )

import Diagrams.Backend.Cairo

-- Needed due to GHC 7.0.x bug, see haddocks for Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

import Music.Time
import Music.Render ( Render(..) )
import Music.Internal.Time.Score


-- | A graphic representation.
newtype Graphic = Graphic (Diagram Cairo R2)

-- | Writes the given graphic representation to a file.
writeGraphics :: FilePath -> Graphic -> IO ()
writeGraphics file (Graphic diagram) = do
    fst $ renderDia Cairo (CairoOptions file (Width 250) PDF) diagram
    fst $ renderDia Cairo (CairoOptions "test.ps" (Width 250) PS) diagram
    return ()



--
-- Score rendering
--

instance (Time t, Show a) => Render (Score t a) Graphic where
    render = renderGraphics

-- instance (Time t, Show a) => Render (EventList t a) Graphic where
--     render = renderGraphics . unrenderScore

renderGraphics :: (Time t, Show a) => Score t a -> Graphic
renderGraphics = Graphic 
        . foldScore (\t d   -> renderRest d)
                    (\t d x -> renderNote d x)
                    (\t x y -> renderPar x y)
                    (\t x y -> renderSeq x y)
        . normalizeDuration
    where
        renderRest d   | d == 0     =  mempty
                       | otherwise  =  moveOriginBy (r2 (negate (t2d d), 0)) (renderEmpty (t2d d * 2))

        renderNote d x | d == 0     =  mempty
                       | otherwise  =  moveOriginBy (r2 (negate (t2d d), 0)) (renderText x <> renderBox (t2d d))

        renderPar     =  beside (negateV unitY)
        renderSeq     =  {-append-}beside unitX
        renderEmpty   =  strutX
        renderText x  =  text (show x) # font "Gill Sans"
                                       # fc white
        renderBox d   =  scaleX d . fcA boxColor $ square 2
        boxColor      =  sRGB24read "465FBD" `withOpacity` 0.6    
        
        t2d           = timeToDouble