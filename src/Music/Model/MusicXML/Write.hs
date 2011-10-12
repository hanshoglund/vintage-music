{-# LANGUAGE RankNTypes #-}

module Music.Model.MusicXML.Write 
where

import Text.XML.HXT.Core
 
import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout
 

class ReadXml c where
    readXml   :: (ArrowXml a, ArrowList a') => 
        a n XmlTree -> a' n' c

class WriteXml b where
    writeXml :: ArrowXml a => b -> a n XmlTree






instance WriteXml () where
    writeXml () = mkelem "nil" [] []



   
putXml :: WriteXml a => a -> IO ()
putXml xml = 
    do  runX (
                root [] [writeXml xml] 
                >>> 
                writeDocument [withIndent True] ""
             )
        return ()
