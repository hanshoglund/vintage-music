
{-|
    Defines a generic model of music.
    Typical models can be exported, played etc.
-}
module Music.Model where



class Model m where
    
class Play p where
    play  :: p -> IO ()
    view  :: p -> IO ()
    write :: FilePath -> p -> IO ()