
{-|
    Module      :  Music.Util.System
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable

    System utilities.
-}

module Music.Util.System where
       
import System.Posix.Process


-- | Execute a system command.
execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
       
-- | Open the given file using the default application.
openFile :: FilePath -> IO ()
openFile path = execute "open" [path]
        
-- | Open the given file using the given application.
openFileWith :: FilePath -> FilePath -> IO ()
openFileWith application path = execute "open" [path, "-a", application]

-- | Open the given Midi file using Timidity.
openMidiFile   :: FilePath -> IO ()
openMidiFile name = execute "timidity" ["--quiet", name]

-- | Open the given Midi file using Sibelius.
exportMidiFile :: FilePath -> IO ()
exportMidiFile    = openFileWith "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6"

-- | Audify the given Midi file using Timidity.
exportMidiFileToAudio :: FilePath -> IO ()
exportMidiFileToAudio path = execute "timidity" ["-Ow", path]