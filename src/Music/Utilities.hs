                                                            
module Music.Utilities where
       
import System.Posix.Process


openMidiFile name = execute "timidity" [name]
exportMidiFile    = openFileWith "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6"


execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
       
-- | (Mac OS X) Open the given file using the default application.
openFile :: FilePath -> IO ()
openFile path = execute "open" [path]
        
-- | (Mac OS X) Open the given file using the given application.
openFileWith :: FilePath -> FilePath -> IO ()
openFileWith application path = execute "open" [path, "-a", application]
