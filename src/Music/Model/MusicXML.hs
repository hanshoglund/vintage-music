
{-|
    Implements MusicXML 2.0.
        
    XML elements are lifted to Haskell values. Optional and listed elements are represented using
    'Maybe', 'Either' and @[]@. Most types can be initiated using the "Data.Trival" class. This should 
    produce a sensible default element, such as a blank score, a note list without elements etc.
    
    Utility function are provided to manipulate the structured data. Generally, for a type @S@ with
    an element of type @T@, the following functions are provided:
    
    @
        t     :: S -> T
        setT  :: T -> S -> S
        withT :: (T -> T) -> S -> S
    @

    Names and documentation from the MusicXML XSD, see <http://www.recordare.com/musicxml/specification>.
-}
module Music.Model.MusicXML
(
-- * Basic module reexports

      module Music.Model.MusicXML.Base
    , module Music.Model.MusicXML.Note
    , module Music.Model.MusicXML.Score

-- * Score creation and manipulation
    , createPartwiseScore
    , createTimewiseScore
    , withHeader
    , setParts
    , setMeasures
    , withWork
    , setWork
    , setDefaults
    , setCredit
    , setTitle 

-- * Reading and writing
    , putXml

)

where

import Data.Trivial

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

-- | Create a single-voice partwise score with the given name.
createPartwiseScore :: String -> [Part] -> Score
createPartwiseScore title parts    = setParts parts . setTitle title $ trivial

-- | Create a single-voice partwise score with the given name.
createTimewiseScore :: String -> [Measure] -> Score
createTimewiseScore title measures = setMeasures measures . setTitle title $ trivial

withHeader  :: (ScoreHeader -> ScoreHeader) -> Score -> Score
withWork    :: (Work -> Work)               -> Score -> Score
setWork     :: Work                         -> Score -> Score
setDefaults :: Defaults                     -> Score -> Score
setCredit   :: [Credit]                     -> Score -> Score
setTitle    :: String                       -> Score -> Score
   

withHeader  f s = s { header   = f (header s) }
setParts    p s = s { parts    = p }
setMeasures p s = s { measures = p }

withWork    f = withHeader $ \h -> h { work      = Just (force f $ work h) }
setWork     x = withHeader $ \h -> h { work      = Just x }
setDefaults x = withHeader $ \h -> h { defaults  = Just x }
setCredit   x = withHeader $ \h -> h { credit    = x      }
setTitle    x = withWork   $ \w -> w { workTitle = Just x }



-- ------------------------------------------------------------------------- --
-- Test stuff, remove later

u = undefined  
n = Nothing


score1 :: Score
score1 = trivial
score2 = setTitle "Test" score1
