

{-|
    Implements The ABC Music standard 2.0 (December 2010).

    Retrieved from <http://abcnotation.com/wiki/abc:standard:v2.0>.
-}
module Music.Model.Abc
( 
-- * Information fields
  Field(..)

-- * Pitch and accidentals
, PitchName(..)
, OctaveBase
, OctaveModifier
, Accidental
, big
, small
, comma
, apostrophe
, sharp
, flat
, Pitch
, pitch
, normalize
-- * Rhythm
-- * Bars and voices
-- * File structure
)
where
    
import Data.Word 


-- --------------------------------------------------------------------------------

    
data Field = Author
           | Book
           | Composer
           | Discography
           | FileUrl
           | Group
           | History
           | Instruction
           | Key
           | UnitNoteLength
           | Meter
           | Macro
           | Notes
           | Origin
           | Parts
           | Tempo
           | Rhythm
           | Remark
           | Source
           | SymbolLine
           | Title
           | UserDefined
           | Voice
           | Words
           | ReferenceNumber
           | Transcriber
           deriving (Eq, Show)

-- ABC pitches use reduntant octave and accidental modifiers 
-- The default form LiteralPitch preserves the original representation
-- The normalized forms are more efficient
-- The MIDI form can not distinguish C# and Db

data Pitch = LiteralPitch      PitchName OctaveBase [OctaveModifier] [Accidental]
           | NzOctavePitch     PitchName Int [Accidental]
           | NzAccidentalPitch PitchName OctaveBase [OctaveModifier] Int
           | NzPitch           PitchName Int Int
           | MIDIPitch         Word8


data PitchName = C | D | E | F | G | A | B 
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

newtype OctaveBase     = Ob Bool deriving (Eq, Ord)
newtype OctaveModifier = Om Bool deriving (Eq, Ord)
newtype Accidental     = Ac Bool deriving (Eq, Ord)
    
big        = Ob True
small      = Ob False
comma      = Om True
apostrophe = Om False       
sharp      = Ac True
flat       = Ac False

pitch     :: PitchName -> OctaveBase -> [OctaveModifier] -> [Accidental] -> Pitch           
normalize :: Pitch -> Pitch

pitch = LiteralPitch              
normalize (LiteralPitch n ob om a) = undefined

-- --------------------------------------------------------------------------------

data Event = Note Pitch Int
           | Rest Int    
           
-- --------------------------------------------------------------------------------

type Header = [Field]
type Body   = Int -- TODO

data Tune = Tune Header Body

type AbcFile = [Tune]


