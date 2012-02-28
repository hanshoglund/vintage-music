{-|
    Module      :  Music.Render.Graphics.Diagrams
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Music.Render.Graphics.Diagrams where

{-# LANGUAGE 
    NoMonomorphismRestriction #-}

import Diagrams.Prelude hiding (width, height)
import Diagrams.Backend.Cairo

import Music.Utilities









-- 
-- Imperative test stuff
--

writeGraphics :: FilePath -> Diagram Cairo R2 -> IO ()
writeGraphics file diagram = do 
    fst $ renderDia Cairo ( CairoOptions file $ PDF (500, 500) ) diagram
    return ()

example :: Diagram Cairo R2                        
example = circle 1 <> centerX (text "hans")

main = do
    writeGraphics "test.pdf" example
    openFile "test.pdf"
    return ()