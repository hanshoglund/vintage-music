{-|
    Module      :  Music.Inspect
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleContexts #-}

module Music.Inspect
(
    inspect,
    play,
    draw,
    audify,
)
where

import Data.Convert
import Music.Render.Midi
import Music.Render.Graphics
import Music.Util.System

inspect :: (Render a Graphic, Render a Midi) => a -> IO ()
play    :: Render a Midi => a    -> IO ()
draw    :: Render a Graphic => a -> IO ()

inspect score = do
    play score
    draw score

play score = do
    writeMidi    "test.mid" (render score)
    openMidiFile "test.mid"
    return ()

draw score = do
    writeGraphics "test.pdf" (render score)
    openFile      "test.pdf"
    return ()

audify score = do
    writeMidi "test.mid" (render score)
    exportMidiFileToAudio "test.mid"