
{-|
    Implements MusicXML 2.0.

    Names and documentation from the MusicXML XSD, see <http://www.recordare.com/musicxml/specification>.
-}
module Music.Model.MusicXML
(
  module Music.Model.MusicXML.Base
, module Music.Model.MusicXML.Note
, module Music.Model.MusicXML.Score
, module Music.Model.MusicXML.Write
)

where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Articulations
import Music.Model.MusicXML.Attributes
import Music.Model.MusicXML.Harmony
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Note
import Music.Model.MusicXML.Opus
import Music.Model.MusicXML.Score
import Music.Model.MusicXML.Sound
import Music.Model.MusicXML.Text
import Music.Model.MusicXML.Tuplet
import Music.Model.MusicXML.Write


-- ------------------------------------------------------------------------- --
-- Test stuff, remove later

u = undefined  
n = Nothing

  
attributes' = DocumentAttributes { version = "1.0" }

work' = Work 
    { workTitle  = Just "Music" 
    , workNumber = Just "32"
    , opus = Nothing }

parts' = []

header' = ScoreHeader 
    { work = (Just work')
    , movementNumber = Nothing
    , movementTitle  = Nothing
    , identification = Nothing
    , defaults       = Nothing
    , credit         = []
    , partList       = parts' } 

score' = PartwiseScore (Just attributes') header' parts'




