                                                            
module Music.Utilities where
       
import System.Posix.Process


execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
       
-- | (Mac OS X) Open the given file using the default application.
openFile :: FilePath -> IO ()
openFile path = do
    forkProcess $ executeFile "open" True [path] Nothing
    return ()
        
-- | (Mac OS X) Open the given file using the given application.
openFileWith :: FilePath -> FilePath -> IO ()
openFileWith application path = do
    forkProcess $ executeFile "open" True [path, "-a", application] Nothing
    return ()