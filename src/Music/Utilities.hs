                                                            
module Music.Utilities (openFile) where
       
import System.Posix.Process

openFile :: FilePath -> IO ()
openFile path = 
    executeFile "open" True [path] Nothing