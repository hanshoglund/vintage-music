
-- ------------------------------------------------------------

{- |
   Module     : Music.Model.MusicXML.Score
   Copyright  : Copyright (C) 2012 Hans Höglund
   License    : MIT

   Maintainer : Hans Hoglund (hans@hanshoglund.se)
   Stability  : experimental
   Portability: non-portable
-}

-- ------------------------------------------------------------

module Music.Model.MusicXML.Note 
(
-- * Rhythm
      NoteType

-- * Pitch
    , Pitch(..)
    , DisplayStepOctave(..)
    , AccidentalValue(..)
    , Accidental(..)
    , AccidentalMark(..)
-- * Stems and ties
    , Stem(..)
    , Tie(..)
    , Tied(..)

-- * Noteheads
    , NoteHead(..)

-- * Notations
    , Slur(..)
    , Glissando(..)
    , Slide(..)
    , Notation(..)
    , OtherNotation(..)

-- * Note definition
    , FullNote(..)
    , NoteProperties(..)
    , Note(..)
)
where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Text
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Articulations
import Music.Model.MusicXML.Tuplet
import Music.Model.MusicXML.Sound
import Music.Model.MusicXML.Write


-- ------------------------------------------------------------

-- | Pitch is represented as a combination of the step of the diatonic scale, the chromatic
-- alteration, and the octave.
data Pitch = Pitch Step (Maybe Semitones) Octave

-- | The display-step-octave type contains the sequence of elements used by both the rest and
-- unpitched elements. This group is used to place rests and unpitched elements on the staff
-- without implying that these elements have pitch. Positioning follows the current clef. If
-- percussion clef is used, the display-step and display-octave elements are interpreted as if
-- in treble clef, with a G in octave 4 on line 2. If not present, the note is placed on the
-- middle line of the staff, generally used for one-line staffs.
data DisplayStepOctave = DisplayStepOctave Step Octave


-- | The accidental-value type represents notated accidentals supported by MusicXML. In the
--   MusicXML 2.0 DTD this was a string with values that could be included. The XSD strengthens the
--   data typing to an enumerated list.
data AccidentalValue 
    = Sharp
    | Natural
    | Flat
    | DoubleSharp
    | SharpSharp
    | FlatFlat
    | NaturalSharp
    | NaturalFlat
    | QuarterFlat
    | QuarterSharp
    | ThreeQuartersFlat
    | ThreeQuartersSharp
    deriving (Show, Eq, Enum)
  
-- | The accidental type represents actual notated accidentals. Editorial and cautionary indications
--   are indicated by attributes. Values for these attributes are False if not present. Specific graphic
--   display such as parentheses, brackets, and size are controlled by the level-display attribute
--   group.
data Accidental = Accidental { accidentalValue        :: AccidentalValue
                             , accidentalCautionary   :: Bool
                             , accidentalEditorial    :: Bool
                             , accidentalLevelDisplay :: Maybe LevelDisplay
                             , accidentalPrintStyle   :: Maybe PrintStyle }

instance Trivial Accidental where
    trivial = Accidental Natural False False Nothing Nothing

instance WriteXml Accidental where
    writeXml a = undefined


-- | An accidental-mark can be used as a separate notation or as part of an ornament. When used in an
-- ornament, position and placement are relative to the ornament, not relative to the note.
data AccidentalMark = AccidentalMark { accidentalMarkValue      :: AccidentalValue
                                     , accidentalMarkPrintStyle :: Maybe PrintStyle
                                     , accidentalMarkPlacement  :: Maybe Placement }

instance Trivial AccidentalMark where
    trivial = AccidentalMark Natural Nothing Nothing

instance WriteXml AccidentalMark where
    writeXml a = undefined
      

-- ------------------------------------------------------------

-- | The tie element indicates that a tie begins or ends with this note. The tie element
-- indicates sound; the tied element indicates notation.
data Tie = TODO

-- | The tied type represents the notated tie. The tie element represents the tie sound.
data Tied 
    = Tied
    { startStop   :: StartStop
    , numberLevel :: NumberLevel
    , lineType    :: LineType
    , position    :: Position
    , placement   :: Placement
    , orientation :: Orientation
    , bezier      :: Bezier
    , color       :: Color }


-- ------------------------------------------------------------

-- | Slur types are empty. Most slurs are represented with two elements: one with a start type, and
--   one with a stop type. Slurs can add more elements using a continue type. This is typically used to
--   specify the formatting of cross-system slurs, or to specify the shape of very complex slurs.

data Slur = Slur { slurType        :: StartStopContinue 
                 , slurNumber      :: NumberLevel
                 , slurLineType    :: Position 
                 , slurPlacement   :: Placement
                 , slurOrientation :: Orientation
                 , slurBezier      :: Bezier
                 , slurColor       :: Color }
                 
-- ------------------------------------------------------------

-- | Glissando and slide types both indicate rapidly moving from one pitch to the other so that
--   individual notes are not discerned. The distinction is similar to that between NIFF's glissando
--   and portamento elements. A glissando sounds the half notes in between the slide and defaults to a
--   wavy line. The optional text is printed alongside the line.

data Glissando = Glissando { glissandoValue      :: String 
                           , glissandoType       :: StartStop
                           , glissandoNumber     :: NumberLevel
                           , glissandoLineType   :: LineType
                           , glissandoPrintStyle :: PrintStyle }

-- ------------------------------------------------------------

data Slide = Slide { slideValue      :: String
                   , slideType       :: StartStop
                   , slideNumber     :: NumberLevel
                   , slideLineType   :: LineType
                   , slidePrintStyle :: PrintStyle 
                   , slideBendSound  :: BendSound }


-- ------------------------------------------------------------
-- ------------------------------------------------------------
-- ------------------------------------------------------------
-- ------------------------------------------------------------

-- | While using repeater beams was the original method for indicating tremolos, often playback and
--   display are not well-enough integrated in an application to make that feasible. The tremolo
--   ornament can be used to indicate either single-note or double-note tremolos. Single-note tremolos
--   use the single type, while double-note tremolos use the start and stop types. The default is
--   "single" for compatibility with Version 1.1. The text of the element indicates the number of
--   tremolo marks and is an integer from 0 to 6. Note that the number of attached beams is not
--   included in this value, but is represented separately using the beam element.

data Tremolo = Tremolo { tremoloStartStopSingle :: StartStopSingle
                       , tremoloTremoloMarks    :: TremoloMarks
                       , tremoloPrintStyle      :: PrintStyle
                       , tremoloPlacement       :: Placement }

-- ------------------------------------------------------------

-- | Notations refer to musical notations, not XML notations. Multiple notations are allowed in order
-- to represent multiple editorial levels. The set of notations may be refined and expanded over
-- time, especially to handle more instrument-specific technical notations.
data Notation 
    = TiedNotation            Tied
    | SlurNotation            Slur
    | TupletNotation          Tuplet
    
    | GlissandoNotation       Glissando
    | SlideNotation           Slide
    
    | OrnamentsNotation       Ornaments
    
    | TechnicalNotation       Technical
    | ArticulationsNotation   Articulations
    
    | DynamicsNotation        Dynamics
    | FermataNotation         Fermata
    | ArpeggiateNotation      Arpeggiate
    | NonArpeggiateNotation   NonArpeggiate
    | AccidentalMarkNotation  AccidentalMark
    | OtherNotationNotation   OtherNotation
      

-- | The other-notation type is used to define any notations not yet in the MusicXML format. This
-- allows extended representation, though without application interoperability. It handles notations
-- where more specific extension elements such as other-dynamics and other-technical are not
-- appropriate.
data OtherNotation = OtherNotation { otherNotationName        :: String 
                                   , otherNotationType        :: StartStopSingle
                                   , otherNotationLevel       :: NumberLevel
                                   , otherNotationPrintObject :: Bool
                                   , otherNotationPrintStyle  :: PrintStyle
                                   , otherNotationPlacement   :: Placement }


-- ------------------------------------------------------------

-- | Stems can be down, up, none, or double. For down and up stems, the position attributes can be
-- used to specify stem length. The relative values specify the end of the stem relative to the
-- program default. Default values specify an absolute end stem position. Negative values of
-- relative-y that would flip a stem instead of shortening it are ignored.
type Stem = TODO
{-
    <xs:complexType name="stem">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="stem-value">
                <xs:attributeGroup ref="y-position"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}          

-- ------------------------------------------------------------


-- | The full-note group is a sequence of the common note elements between cue/grace notes and
--   regular (full) notes: pitch, chord, and rest information, but not duration (cue and grace
--   notes do not have duration encoded). Unpitched elements are used for unpitched percussion,
--   speaking voice, and other musical elements lacking determinate pitch.

--   The isChord attribute indicates that this note is an additional chord tone with the preceding
--   note. The duration of this note can be no longer than the preceding note. In MuseData, a 
--   missing duration indicates the same length as the previous note, but the MusicXML format 
--   requires a duration for chord notes too.

-- TODO note that the Bool indicates whether it is a chord or not

data FullNote = 
    Pitched   Bool Pitch
    -- | The unpitched element indicates musical elements that are notated on the staff but lack
    -- definite pitch, such as unpitched percussion and speaking voice.
  | Unpitched Bool DisplayStepOctave 
    -- | The rest element indicates notated rests or silences. Rest are usually empty, but
    -- placement on the -- staff can be specified using display-step and display-octave elements.
  | Rest      Bool DisplayStepOctave

data NoteProperties = NoteProperties
    { instrument        :: Maybe Instrument 
    , noteType          :: Maybe NoteType 
    , dots              :: [EmptyPlacement] 
    , accidental        :: Maybe Accidental
    , timeModification  :: Maybe TimeModification
    , stem              :: Maybe Stem
    , noteHead          :: Maybe NoteHead
    , staff             :: Maybe Staff
    , beam              :: Maybe Beam
    , notations         :: [[Notation]]
    , lyric             :: [Lyric] }

-- | Notes are the most common type of MusicXML data. The MusicXML format keeps the MuseData
--   distinction between elements used for sound information and elements used for notation
--   information (e.g., tie is used for sound, tied for notation). Thus grace notes do not have a
--   duration element. Cue notes have a duration element, as do forward elements, but no tie
--   elements. Having these two types of information available can make interchange considerably
--   easier, as some programs handle one type of information much more readily than the other.
-- 
--   The dynamics and end-dynamics attributes correspond to MIDI 1.0's Note On and Note Off 
--   velocities, respectively. They are expressed in terms of percentages of the default 
--   forte value (90 for MIDI 1.0). The attack and release attributes are used to alter the
--   staring and stopping time of the note from when it would otherwise occur based on the
--   flow of durations - information that is specific to a performance. They are expressed
--   in terms of divisions, either positive or negative. A note that starts a tie should
--   not have a release attribute, and a note that stops a tie should not have an attack
--   attribute. If a note is played only one time through a repeat, the time-only attribute
--   shows which time to play the note. The pizzicato attribute is used when just this note
--   is sounded pizzicato, vs. the pizzicato element which changes overall playback between
--   pizzicato and arco.                                                       
--
--   One dot element is used for each dot of prolongation. The placement element is used to 
--   specify whether the dot should appear above or below the staff line. It is ignored for 
--   notes that appear on a staff space.
data Note 
    = GraceNote 
      { noteFullNote      :: FullNote
      , noteTies          :: [Tie]
      , noteProperties    :: NoteProperties }

    | CueNote 
      { noteFullNote      :: FullNote
      , noteDuration      :: Duration
      , noteProperties    :: NoteProperties }

    | NormalNote
      { noteFullNote      :: FullNote
      , noteDuration      :: Duration 
      , noteTies          :: [Tie]         
      , noteProperties    :: NoteProperties }

-- | The note-type type indicates the graphic note type. Values range from 256th to long. The
-- size attribute indicates full, cue, or large size, with full the default for regular notes and
-- cue the default for cue and grace notes.
type NoteType = (NoteTypeValue, SymbolSize)

-- | The notehead element indicates shapes other than the open and closed ovals associated with note
-- durations. For the enclosed shapes, the default is to be hollow for half notes and longer, and
-- filled otherwise. The filled attribute can be set to change this if needed. If the parentheses
-- attribute is set to yes, the notehead is parenthesized. It is no by default.
data NoteHead = NoteHead
    { noteHeadValue         :: NoteHeadValue
    , noteHeadfilled        :: Bool
    , noteHeadparentheses   :: Bool
    , noteHeadfont          :: Font
    , noteHeadcolor         :: Color }


