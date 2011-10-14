{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module Music.Model.MusicXML.Write
(
  ArrowXml
, XmlTree
, ReadXml(..)
, WriteXml(..)
, Trivial(..)

, element
-- , mkelem
-- , aelem
-- , selem
-- , eelem
-- , root

, attr
, attrs
, stringAttrs
, showAttrs
, writeXmlAttrs

-- , txt
-- , blb

-- , catA
-- , this
-- , none
, (<+>)

, putXml 
, printXml
) 
where

import Data.Monoid                    

import Control.Arrow
import Text.XML.HXT.Core
 
import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout
 

-- | The class of types that can be read from an XML representation.
class ReadXml c where
    readXml :: (ArrowXml a, ArrowList a') => 
        a n XmlTree -> a' n' c

-- | The class of types that can be written to an XML representation.
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

instance WriteXml a => WriteXml [a] where
    writeXml [] = none
    writeXml xs = catA (map writeXml xs)
    
instance (WriteXml a, WriteXml b) => WriteXml (a, b) where
    writeXml (a, b) = writeXml a <+> writeXml b

instance (WriteXml a, WriteXml b) => WriteXml (Either a b) where
    writeXml (Left x)  = writeXml x
    writeXml (Right x) = writeXml x
                       
instance WriteXml TODO where 
    writeXml = error "TODO is not implemented"



-- | A class for types with a sensible default value.     
class Trivial a where
    trivial :: a
    always  :: Maybe a -> a
    force   :: (a -> b) -> Maybe a -> b
    forceLeft  :: Either a b -> a
    forceRight :: Either b a -> a
    
    always (Nothing)  = trivial
    always (Just x)   = x 
    force f (Nothing) = f trivial
    force f (Just x)  = f x
    forceLeft (Left x)   = x
    forceRight (Right x) = x
    forceLeft _          = trivial
    forceLeft _          = trivial

instance Trivial () where
    trivial = ()

instance Trivial [a] where
    trivial = []

instance Trivial (Maybe a) where
    trivial = Nothing       

-- instance (Trivial a) => Trivial (Either a b) where
--     trivial = Left trivial
                       
instance Trivial TODO where 
    trivial = error "TODO is not implemented"
 
element :: ArrowXml a => String -> [a n XmlTree] -> [a n XmlTree] -> a n XmlTree
element = mkelem

attrs         :: ArrowXml a               => [(String, a n XmlTree)] -> a n XmlTree
stringAttrs   :: ArrowXml a               => [(String, String)]      -> a n XmlTree
showAttrs     :: (ArrowXml a, Show s)     => [(String, s)]           -> a n XmlTree
writeXmlAttrs :: (ArrowXml a, WriteXml b) => [(String, b)]           -> a n XmlTree
attrs'        :: ArrowXml a               => (t -> a b XmlTree) -> [(String, t)] -> a b XmlTree

attrs         = attrs' id
stringAttrs   = attrs' txt
showAttrs     = attrs' (txt . show)
writeXmlAttrs = attrs' writeXml
attrs' f      = catA . map (\(k, v) -> attr k (f v))


   
putXml :: WriteXml a => a -> IO ()
putXml = printXml . writeXml

printXml xml = 
    do  putStrLn ""
        runX (
                root [] [xml] 
                -- >>> 
                -- addDoctypeDecl "score-partwise" "" ""
                >>>
                writeDocument [withIndent True] ""
             )
        return ()
                    


-- foo x = writeXml [Just ""] <+> writeXml [Left "", Right ()]
