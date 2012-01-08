{-# LANGUAGE MultiParamTypeClasses #-}

module Music.Model.Standard.Phrasing
(
  Phrase(..)  
, Phrased(..)
, Arpeggio(..)
, Arpeggiated(..)
)
where
 
import Music.Model.Standard
    
data Phrase a 
    = Legato a
    | Staccato a

newtype (Sequential s) => Phrased s a = Phrased { runPhrased :: s (Phrase a) }

data Arpeggio a
    = Arpeggio a
    | NoArpeggio a

newtype (Parallel p) => Arpeggiated p a = Arpeggiated { runArpeggiated :: p (Arpeggio a) }
