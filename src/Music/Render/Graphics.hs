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

import Diagrams.Prelude hiding ( Render, render )
import Diagrams.Backend.Cairo


-- | A graphic representation.
newtype Graphic = Graphic (Diagram Cairo R2)

-- | Writes the given graphic representation to a file.
writeGraphics :: FilePath -> Graphic -> IO ()
writeGraphics file (Graphic diagram) = do
    fst $ renderDia Cairo ( CairoOptions file $ PDF (500, 500) ) diagram
    return ()


