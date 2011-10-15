                                                            
module Music.Utilities where
       
import System.Posix.Process
       
-- | (Mac OS X) Open the given file using the default application.
openFile :: FilePath -> IO ()
openFile path = 
    executeFile "open" True [path] Nothing
    
-- | (Mac OS X) Open the given file using the given application.
openFileWith :: FilePath -> FilePath -> IO ()
openFileWith path application = 
    executeFile "open" True [path, "-a", application] Nothing