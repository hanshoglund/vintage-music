{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module Music.Model.MusicXML.Write
(
  ArrowXml
, XmlTree
, ReadXml(..)
, WriteXml(..)

, mkelem
, aelem
, selem
, eelem
, root

, attr
, attrs
, stringAttrs
, showAttrs
, writeXmlAttrs

, txt
, blb

, catA
, this
, none
, (<+>)

, putXml 
, printXml
) 
where

import Control.Arrow
import Text.XML.HXT.Core
 
import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout
 

class ReadXml c where
    readXml :: (ArrowXml a, ArrowList a') => 
        a n XmlTree -> a' n' c

class WriteXml b where
    writeXml :: ArrowXml a => b -> a n XmlTree


instance WriteXml () where
    writeXml x = none

instance WriteXml Int where
    writeXml x = txt (show x)

instance WriteXml String where
    writeXml x = txt x

instance WriteXml a => WriteXml (Maybe a) where
    writeXml (Just a)  = writeXml a 
    writeXml (Nothing) = none
    
instance (WriteXml a, WriteXml b) => WriteXml (Either a b) where
    writeXml (Left x)  = writeXml x
    writeXml (Right x) = writeXml x

instance WriteXml a => WriteXml [a] where
    writeXml [] = none
    writeXml xs = catA (map writeXml xs)

instance WriteXml TODO where 
    writeXml = error "TODO is not implemented"


attrs :: ArrowXml a => [(String, a n XmlTree)] -> a n XmlTree
attrs = attrs' id

stringAttrs :: ArrowXml a => [(String, String)] -> a n XmlTree
stringAttrs = attrs' txt

showAttrs :: (ArrowXml a, Show s) => [(String, s)] -> a n XmlTree
showAttrs = attrs' (txt . show)

writeXmlAttrs :: (ArrowXml a, WriteXml b) => [(String, b)] -> a n XmlTree
writeXmlAttrs = attrs' writeXml

attrs' :: ArrowXml a => (t -> a b XmlTree) -> [(String, t)] -> a b XmlTree
attrs' f = catA . map (\(k, v) -> attr k (f v))


   
putXml :: WriteXml a => a -> IO ()
putXml = printXml . writeXml

printXml xml = 
    do  runX (
                root [] [xml] 
                -- >>> 
                -- addDoctypeDecl "score-partwise" "" ""
                >>>
                writeDocument [withIndent True] ""
             )
        return ()
                    


-- foo x = writeXml [Just ""] <+> writeXml [Left "", Right ()]
