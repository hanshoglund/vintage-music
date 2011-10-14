

module Music.Model.MusicXML.Note 
where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Text
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Articulations
import Music.Model.MusicXML.Tuplet


-- *****************************************************************************
-- Simple types
-- *****************************************************************************

-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************



-- *****************************************************************************
-- Complex types
-- *****************************************************************************

-- | The accidental type represents actual notated accidentals. Editorial and cautionary indications
-- are indicated by attributes. Values for these attributes are "no" if not present. Specific graphic
-- display such as parentheses, brackets, and size are controlled by the level-display attribute
-- group.
type Accidental = TODO
{-
    <xs:complexType name="accidental">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="accidental-value">
                <xs:attribute name="cautionary" type="yes-no"/>
                <xs:attribute name="editorial" type="yes-no"/>
                <xs:attributeGroup ref="level-display"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
-- | An accidental-mark can be used as a separate notation or as part of an ornament. When used in an
-- ornament, position and placement are relative to the ornament, not relative to the note.
type AccidentalMark = TODO
{-
    <xs:complexType name="accidental-mark">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="accidental-value">
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | Notations refer to musical notations, not XML notations. Multiple notations are allowed in order
-- to represent multiple editorial levels. The set of notations may be refined and expanded over
-- time, especially to handle more instrument-specific technical notations.
type Notations = [Notation]

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

-- | The other-notation type is used to define any notations not yet in the MusicXML format. This
-- allows extended representation, though without application interoperability. It handles notations
-- where more specific extension elements such as other-dynamics and other-technical are not
-- appropriate.
type OtherNotation = TODO
{-
    <xs:complexType name="other-notation">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="start-stop-single" use="required"/>
                <xs:attribute name="number" type="number-level" default="1"/>
                <xs:attributeGroup ref="print-object"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}


-- | Pitch is represented as a combination of the step of the diatonic scale, the chromatic
-- alteration, and the octave.
type Pitch = TODO
{-
    <xs:complexType name="pitch">
        <xs:sequence>
            <xs:element name="step" type="step"/>
            <xs:element name="alter" type="semitones" minOccurs="0"/>
            <xs:element name="octave" type="octave"/>
        </xs:sequence>
    </xs:complexType>

-}




-- | The tie element indicates that a tie begins or ends with this note. The tie element
-- indicates sound; the tied element indicates notation.
data Tie = TODO

-- | The tied type represents the notated tie. The tie element represents the tie sound.
data Tied 
    = Tied
    { startStop :: StartStop
    , numberLevel :: NumberLevel
    , lineType :: LineType
    , position :: Position
    , placement :: Placement
    , orientation :: Orientation
    , bezier :: Bezier
    , color :: Color }

-- | The display-step-octave type contains the sequence of elements used by both the rest and
-- unpitched elements. This group is used to place rests and unpitched elements on the staff
-- without implying that these elements have pitch. Positioning follows the current clef. If
-- percussion clef is used, the display-step and display-octave elements are interpreted as if
-- in treble clef, with a G in octave 4 on line 2. If not present, the note is placed on the
-- middle line of the staff, generally used for one-line staffs.
type DisplayStepOctave = (Step, Octave)


-- | The full-note group is a sequence of the common note elements between cue/grace notes and
-- regular (full) notes: pitch, chord, and rest information, but not duration (cue and grace
-- notes do not have duration encoded). Unpitched elements are used for unpitched percussion,
-- speaking voice, and other musical elements lacking determinate pitch.
data FullNote = 
    -- | The chord element indicates that this note is an additional chord tone with the preceding
    -- note. The duration of this note can be no longer than the preceding note. In MuseData, a 
    -- missing duration indicates the same length as the previous note, but the MusicXML format 
    -- requires a duration for chord notes too.
    Chord 
  | Pitch  
    -- | The unpitched element indicates musical elements that are notated on the staff but lack
    -- definite pitch, such as unpitched percussion and speaking voice.
  | Unpitched DisplayStepOctave 
    -- | The rest element indicates notated rests or silences. Rest are usually empty, but
    -- placement on the -- staff can be specified using display-step and display-octave elements.
  | Rest      DisplayStepOctave

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
data  Note = 
    GraceNote 
    { fullNote          :: FullNote
    , ties              :: [Tie]          
    , instrument        :: Maybe Instrument 
    , noteType          :: Maybe NoteType 
    , dots              :: [EmptyPlacement] 
    , accidental        :: Maybe Accidental
    , timeModification  :: Maybe TimeModification
    , stem              :: Maybe Stem
    , noteHead          :: Maybe NoteHead
    , staff             :: Maybe Staff
    , beam              :: Maybe Beam
    , notations         :: [Notations]
    , lyric             :: [Lyric] }
  | CueNote 
    { fullNote          :: FullNote
    , duration          :: Duration 
    , instrument        :: Maybe Instrument 
    , noteType          :: Maybe NoteType 
    , dots              :: [EmptyPlacement] 
    , accidental        :: Maybe Accidental
    , timeModification  :: Maybe TimeModification
    , stem              :: Maybe Stem
    , noteHead          :: Maybe NoteHead
    , staff             :: Maybe Staff
    , beam              :: Maybe Beam
    , notations         :: [Notations]
    , lyric             :: [Lyric] }
  | NormalNote
    { fullNote          :: FullNote
    , duration          :: Duration 
    , ties              :: [Tie]         
    , instrument        :: Maybe Instrument 
    , noteType          :: Maybe NoteType 
    , dots              :: [EmptyPlacement] 
    , accidental        :: Maybe Accidental
    , timeModification  :: Maybe TimeModification
    , stem              :: Maybe Stem
    , noteHead          :: Maybe NoteHead
    , staff             :: Maybe Staff
    , beam              :: Maybe Beam
    , notations         :: [Notations]
    , lyric             :: [Lyric] }        
                        

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


-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************
