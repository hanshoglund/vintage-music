
{-|
    Implements MusicXML 2.0.

        * XML types, groups and attributeGroups becomes Haskell types

        * XML choices are represented by Either, or lifted to new types

        * XML sequences are represented by lists

        * XML restrictions are not implemented (yet)

    This document is derived from the MusicXML XSD, see <http://www.recordare.com/musicxml/specification>.
-}
module Music.Model.MusicXML
(  
TODO

-- * Simple types
-- ** Numeric
, Divisions, Tenths, StringNumber, NumberLevel, BeamLevel
, Percent, RotationDegrees, StaffLine, StaffNumber
, Midi16, Midi128, Midi16384
, NumberOrNormal(..)

-- ** Symbolic
, AboveBelow, OverUnder, TopBottom, UpDown, BackwardForward
, above, below, over, under, top, bottom, up, down, forward, backward
, LeftRightMiddle(..)
, Fifths, Mode(..)
, StartNote, StartStop, StartStopContinue, StartStopSingle, StartStopDiscontinoue, SSCDS(..)
, TrillBeats, TrillStep(..), TwoNoteTurn(..)
, ClefSign, StaffType, TimeSymbol
, BarStyle
, LineShape(..), LineType(..), LineEnd(..)
, MeasureNumberingValue(..)
, WedgeType(..), SymbolSize(..), NoteSizeType(..), AccidentalValue(..), BeamValue(..)
, Fan(..)
, NoteTypeValue
, eighth, quarter, half, whole, breve, long
, NoteHeadValue(..)
, Octave, Semitones
, ShowTuplet(..)
, StemValue(..)
, Step(..)
, TremoloMarks
, GroupBarlineValue(..), GroupSymbolValue(..)


-- * Attribute groups
-- , BendSound(..)  
, Bezier(..), Position(..)
, Orientation, Placement
, PrintObject, PrintSpacing, PrintStyle, PrintOut(..)
, TrillSound(..)     
, BendSound(..)
, MeasureAttributes (..)
, PartAttributes
, PartNameText


-- * Complex types

, AccidentalText
, Dynamics
, Empty
, EmptyPLacement
, EmptyPrintStyle
, EmptyTrillSound
, Fermata
, Fingering
, FormattedText
, Fret
, Level
, MidiInstrument
, NameDisplay
, StringNumber2
, TypedText
, WavyLine
, Attributes
, BeatRepeat
, Key
, KeyOctave
, MeasureRepeat
, MeasureStyle
, MultipleRest
, Slash
, StaffTuning
, Time
, Transpose
, BarStyleColor
, Ending
, Repeat
, Accord
, AccordionRegistration
, Barre
, Bass
, BassStep
, Bracket
, Dashes
, Degree
, DegreeAlter
, DegreeType
, DegreeValue
, Direction
, DirectionType
, Feature
, FirstFret
, Frame
, FrameNote
, Grouping
, Harmony
, HarpPedals
, Image
, Kind
, MeasureNumbering
, Metronome
, MetronomeBeam
, MetronomeNote
, MetronomeTuplet
, OctaveShift
, Offset
, OtherDirection
, Pedal
, PedalTuning
, PerMinute
, Print
, Rehearsal
, Root
, RootStep
, Scordatura
, Sound
, Wedge
, Encoding
, Identification
, Miscellaneous
, MiscellaneousFields
, Supports
, Appearance
, LineWidth
, MeasureLayout
, NoteSize
, OtherAppearance
, PageLayout
, PageMarigins
, Scaling
, StaffLayout
, SystemLayout
, SystemMargins
, Bookmark
, Link
, Accidental
, AccidentalMark
, Arpeggiate
, Articulations
, Backup
, Beam
, Bend
, DisplayStepOctave
, Elision
, EmptyLine
, Extend
, Figure
, FiguredBass
, Forward
, Glissando
, Grace
, HammerOnPulloff
, Harmonic
, HeelToe
, Instrument
, Lyric
, Mordent
, NonArpeggiate
, Notations
, Note(..)
, NoteType
, NoteHead
, Ornaments
, OtherNotation
, Pitch
, PlacementText
, Slide
, Slur
, Stem
, StrongAccent
, StyleText
, Technical
, TextElementData
, Tie
, Tied
, TimeModification
, Tremolo
, Tuplet
, TupletDot
, TupletPortion
, TupletType
, Credit
, Defaults
, EmptyFont
, GroupBarline
, GroupName
, GroupSymbol
, LyricFont
, LyricLanguage
, MidiDevice
, Opus
, PartGroup
, PartList
, PartName
, ScoreInstrument
, ScorePart
, Work

-- * Element groups

, Editorial
, EditorialVoice
, EditorialVoiceDirection
, Footnote
, LevelGroup
, Staff
, Tuning
, Voice
, NonTraditionalKey
, SlashGroup
, TraditionalKey
, BeatUnit
, HarmonyChord
, AllMargins
, Layout
, LeftRightMargins
, Duration
, FullNote
, MusicData(..)
, PartGroupGroup
, ScoreHeader(..)
, ScorePart



-- * Root elements
, ScorePartwise
, ScoreTimewise


)

where


-- *****************************************************************************
-- Simple types
-- *****************************************************************************


-- | The divisions type is used to express values in terms of the musical divisions defined by
-- the divisions element. It is preferred that these be integer values both for MIDI
-- interoperability and to avoid roundoff errors.
type Divisions          = Int
-- | The tenths type is a number representing tenths of interline staff space (positive or
-- negative). Both integer and decimal values are allowed, such as 5 for a half space and 2.5 for a
-- quarter space. Interline space is measured from the middle of a staff line. Distances in a
-- MusicXML file are measured in tenths of staff space. Tenths are then scaled to millimeters within
-- the scaling element, used in the defaults element at the start of a score. Individual staves can
-- apply a scaling factor to adjust staff size. When a MusicXML element or attribute refers to
-- tenths, it means the global tenths defined by the scaling element, not the local tenths as
-- adjusted by the staff-size element.
type Tenths             = Double
-- | The string-number type indicates a string number. Strings are numbered from high to low,
-- with 1 being the highest pitched string.
type StringNumber       = Int
-- | Slurs, tuplets, and many other features can be concurrent and overlapping within a single
-- musical part. The number-level type distinguishes up to six concurrent objects of the same type.
-- A reading program should be prepared to handle cases where the number-levels stop in an arbitrary
-- order. Different numbers are needed when the features overlap in MusicXML file order. When a
-- number-level value is implied, the value is 1 by default.
type NumberLevel        = Int
-- | The MusicXML format supports six levels of beaming, up to 256th notes. Unlike the
-- number-level type, the beam-level type identifies concurrent beams in a beam group. It does not
-- distinguish overlapping beams such as grace notes within regular notes, or beams used in
-- different voices.
type BeamLevel          = Int
-- | The percent type specifies a percentage from 0 to 100.
type Percent            = Int
-- | The rotation-degrees type specifies rotation, pan, and elevation values in degrees. Values
-- range from -180 to 180.
type RotationDegrees    = Int
-- | The staff-line type indicates the line on a given staff. Staff lines are numbered from
-- bottom to top, with 1 being the bottom line on a staff. Staff line values can be used to specify
-- positions outside the staff, such as a C clef positioned in the middle of a grand staff.
type StaffLine          = Int
-- | The staff-number type indicates staff numbers within a multi-staff part. Staves are numbered
-- from top to bottom, with 1 being the top staff on a part.
type StaffNumber        = Int


-- | The midi-16 type is used to express MIDI 1.0 values that range from 1 to 16.
type Midi16             = Int
-- | The midi-16 type is used to express MIDI 1.0 values that range from 1 to 128.
type Midi128            = Int
-- | The midi-16 type is used to express MIDI 1.0 values that range from 1 to 16,384.
type Midi16384          = Int

-- | The number-or-normal values can be either a decimal number or the string "normal". This is used 
-- by the line-height and letter-spacing attributes.
data NumberOrNormal     = Number Double | Normal

-- | The above-below type is used to indicate whether one element appears above or below another
-- element.
newtype AboveBelow         = AboveBelow Bool
-- | The over-under type is used to indicate whether the tips of curved lines such as slurs and
-- ties are overhand (tips down) or underhand (tips up).
newtype OverUnder        = OverUnder Bool
-- | The top-bottom type is used to indicate the top or bottom part of a vertical shape like
-- non-arpeggiate.
newtype TopBottom        = TopBottom Bool
-- | The up-down type is used for arrow direction, indicating which way the tip is pointing.
newtype UpDown           = UpDown Bool
-- | The backward-forward type is used to specify repeat directions. The start of the repeat has
-- a forward direction while the end of the repeat has a backward direction.
newtype BackwardForward  = BackwardForward Bool

above   = AboveBelow      True ; below    = AboveBelow      False
over    = OverUnder       True ; under    = OverUnder       False
top     = TopBottom       True ; bottom   = TopBottom       False
up      = UpDown          True ; down     = UpDown          False
forward = BackwardForward True ; backward = BackwardForward False


-- | The right-left-middle type is used to specify barline location.
data LeftRightMiddle = LeftPos | RightPos | MiddlePos
    deriving (Show, Eq, Enum)



-- | The fifths type represents the number of flats or sharps in a traditional key signature.
-- Negative numbers are used for flats and positive numbers for sharps, reflecting the key's
-- placement within the circle of fifths (hence the type name).
type Fifths = Int
-- | The mode type is used to specify major/minor and other mode distinctions. Valid mode values
-- include major, minor, dorian, phrygian, lydian, mixolydian, aeolian, ionian, and locrian.
data Mode   = Major | Minor | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Ionian | Locrian
    deriving (Show, Eq, Enum)



-- | The start-note type describes the starting note of trills and mordents for playback,
-- relative to the current note.
data StartNote              = StartNoteUpper | StartNoteMain | StartNoteBelow
    deriving (Show, Eq, Enum)
-- | The start-stop type is used for an attribute of musical elements that can either start or
-- stop, such as tuplets, wedges, and lines.
type StartStop              = SSCDS
-- | The start-stop-continue type is used for an attribute of musical elements that can either
-- start or stop, but also need to refer to an intermediate point in the symbol, as for complex
-- slurs.
type StartStopContinue      = SSCDS
-- | The start-stop-single type is used for an attribute of musical elements that can be used for
-- either multi-note or single-note musical elements, as for tremolos.
type StartStopSingle        = SSCDS
-- | The start-stop-discontinue type is used to specify ending types. Typically, the start type
-- is associated with the left barline of the first measure in an ending. The stop and
-- discontinue types are associated with the right barline of the last measure in an ending. Stop
-- is used when the ending mark concludes with a downward jog, as is typical for first endings.
-- Discontinue is used when there is no downward jog, as is typical for second endings that do
-- not conclude a piece.
type StartStopDiscontinoue  = SSCDS
data SSCDS = Start | Stop | Continue | Discontinue | Single
    deriving (Show, Eq, Enum)


-- | The trill-beats type specifies the beats used in a trill-sound or bend-sound attribute
-- group. It is a decimal value with a minimum value of 2.
type TrillBeats  = Double
-- | The trill-step type describes the alternating note of trills and mordents for playback,
-- relative to the current note.
data TrillStep    = TrillWholeStep | TrillHalfStep | TrillUnison
    deriving (Show, Eq, Enum)
data TwoNoteTurn  = TrillWholeTurn | TrillHalfTurn | TrillNoTurn
    deriving (Show, Eq, Enum)

-- | The clef-sign element represents the different clef symbols.
data ClefSign =
    GClef
  | FClef
  | CClef
  | PercussionClef
  | TabClef
  | NoClef
  deriving (Show, Eq, Enum)

-- | The staff-type value can be ossia, cue, editorial, regular, or alternate. An alternate staff
-- indicates one that shares the same musical data as the prior staff, but displayed differently
-- (e.g., treble and bass clef, standard notation and tab).
data StaffType  =
    OssiaStaff
  | CueStaff
  | EditorialStaff
  | RegularStaff
  | AlternateStaff
  deriving (Show, Eq, Enum)

-- | The time-symbol type indicates how to display a time signature. The normal value is the
-- usual fractional display, and is the implied symbol type if none is specified. Other options
-- are the common and cut time symbols, as well as a single number with an implied denominator.
data TimeSymbol =
    CommonTimeSymbol
  | CutTimeSymbol
  | SingleNumberTimeSymbol
  | NormalTimeSymbol
  deriving (Show, Eq, Enum)

-- | The bar-style type represents barline style information. Choices are regular, dotted,
-- dashed, heavy, light-light, light-heavy, heavy-light, heavy-heavy, tick (a short stroke
-- through the top line), short (a partial barline between the 2nd and 4th lines), and none.
data BarStyle =
    Regular
  | Dotted
  | Dashed
  | Heavy
  | LightLight
  | LightHeavy
  | HeavyLight
  | HeavyHeavy
  | Tick
  | Short
  deriving (Show, Eq, Enum)

data LineShape = StraightLine | CurvedLine
    deriving (Show, Eq, Enum)
data LineType  = SoliedLine | DashedLine | DottedLine | WavyLine
    deriving (Show, Eq, Enum)

-- | The line-end type specifies if there is a jog up or down (or both), an arrow, or nothing at
-- the start or end of a bracket.
data LineEnd =
    EndUp
  | EndDown
  | EndBoth
  | EndArrow
  | NoEnd
  deriving (Show, Eq, Enum)

-- | The measure-numbering-value type describes how measure numbers are displayed on this part:
-- no numbers, numbers every measure, or numbers every system.
data MeasureNumberingValue =
    NoNumbering
  | EachMeasure
  | EachSystem
  deriving (Show, Eq, Enum)

-- | The wedge type is crescendo for the start of a wedge that is closed at the left side,
-- diminuendo for the start of a wedge that is closed on the right side, and stop for the end of
-- a wedge.
data WedgeType = Crescendo | Diminuendo | StopWedge
    deriving (Show, Eq, Enum)

-- | The symbol-size type is used to indicate full vs. cue-sized vs. oversized symbols. The large
-- value for oversized symbols was added in version 1.1.
data SymbolSize = FullSize | CueSize | LargeSize
    deriving (Show, Eq, Enum)

-- | The note-size-type type indicates the type of note being defined by a note-size element. The
-- grace type is used for notes of cue size that that include a grace element. The cue type is
-- used for all other notes with cue size, whether defined explicitly or implicitly via a cue
-- element. The large type is used for notes of large size.
data NoteSizeType = CueNoteType | GraceNoteType | LargeNoteType
    deriving (Show, Eq, Enum)

-- | The accidental-value type represents notated accidentals supported by MusicXML. In the
-- MusicXML 2.0 DTD this was a string with values that could be included. The XSD strengthens the
-- data typing to an enumerated list.
data AccidentalValue =
    Sharp
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

data BeamValue     = BeamBegin | BeamContinue | BeamEnd | ForwardHook | BackwardHook
    deriving (Show, Eq, Enum)
data Fan           = Accel | Rit | NoFan
    deriving (Show, Eq, Enum)

-- | The note-type type is used for the MusicXML type element and represents the graphic note
-- type, from 1/256 (shortest) to long (longest).
type NoteTypeValue = Rational

eighth, quarter, half, whole, breve, long :: NoteTypeValue
eighth  = 1 / 8
quarter = 1 / 4
half    = 1 / 2
whole   = 1
breve   = 2
long    = 4

-- | The notehead type indicates shapes other than the open and closed ovals associated with note
-- durations. The values do, re, mi, fa, so, la, and ti correspond to Aikin's 7-shape system. The
-- arrow shapes differ from triangle and inverted triangle by being centered on the stem. Slashed
-- and back slashed notes include both the normal notehead and a slash. The triangle shape has
-- the tip of the triangle pointing up; the inverted triangle shape has the tip of the triangle
-- pointing down.
data NoteHeadValue =
    SlashNoteHead
  | TriangleNoteHead
  | DiamondNoteHead
  | SquareNoteHead
  | CrossNoteHead
  | XNoteHead
  | CircleXNoteHead
  | InvertedTriangleNoteHead
  | ArrowDownNoteHead
  | ArrowUpNoteHead
  | SlashedNoteHead
  | BackSlashedNoteHead
  | NormalNoteHead
  | ClusterNoteHead
  | NoNoteHeadNoteHead
  | DoNoteHead
  | ReNoteHead
  | MiNoteHead
  | FaNoteHead
  | SoNoteHead
  | LaNoteHead
  | TiNoteHead
  deriving (Show, Eq, Enum)

-- | Octaves are represented by the numbers 0 to 9, where 4 indicates the octave started by
-- middle C.
type Octave     = Int
-- | The semintones type is a number representing semitones, used for chromatic alteration. A
-- value of -1 corresponds to a flat and a value of 1 to a sharp. Decimal values like 0.5
-- (quarter tone sharp) may be used for microtones.
type Semitones  = Int
-- | The show-tuplet type indicates whether to show a part of a tuplet relating to the
-- tuplet-actual element, both the tuplet-actual and tuplet-normal elements, or neither.
data ShowTuplet = Actual | Both | NeitherTupletPart
    deriving (Show, Eq, Enum)

-- | The stem type represents the notated stem direction.
data StemValue = StemDown | StemUp | DoubleStem | NoStem
    deriving (Show, Eq, Enum)

-- | The step type represents a step of the diatonic scale, represented using the English letters
-- A through G.
data Step = A | B | C | D | E | F | G
    deriving (Show, Eq, Enum)

-- | The number of tremolo marks is represented by a number from 0 to 6: the same as beam-level
-- with 0 added.
type TremoloMarks       = Int

-- | The group-barline-value type indicates if the group should have common barlines.
data GroupBarlineValue = Common | NotCommon | Mensurstrich
    deriving (Show, Eq, Enum)

-- | The group-symbol-value type indicates how the symbol for a group is indicated in the score.
-- The default value is none.
data GroupSymbolValue  = Brace | Line | Bracket | NoSymbol
    deriving (Show, Eq, Enum)




-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

-- | The bend-sound type is used for bend and slide elements, and is
-- similar to the trill-sound attribute group. Here the beats element refers to the number of
-- discrete elements (like MIDI pitch bends) used to represent a continuous bend or slide. The
-- first-beat indicates the percentage of the direction for starting a bend; the last-beat the
-- percentage for ending it. The default choices are:
--
--      * accelerate = "no"
--
--      * beats = "4"
--
--      * first-beat = "25"
--
--      * last-beat = "75"

data BendSound = BendSound
    { accelerate    :: Bool
    , beats         :: TrillBeats
    , firstBeat     :: Percent
    , lastBeat      :: Percent }
    
data TrillSound = TrillSound
    { startNote     :: StartNote
    , trillStep     :: TrillStep
    , twoNoteTurn   :: TwoNoteTurn
    -- , accelerate    :: Bool  FIXME
    -- , beats         :: TrillBeats
    , secondBeat    :: Percent
    -- , lastBeat      :: Percent 
    }
    deriving (Show, Eq)                    


-- | The bezier attribute group is used to indicate the curvature of slurs and ties, representing
--   the control points for a cubic bezier curve. For ties, the bezier attribute group is used
--   with the tied element. Normal slurs, S-shaped slurs, and ties need only two bezier points:
--   one associated with the start of the slur or tie, the other with the stop. Complex slurs and
--   slurs divided over system breaks can specify additional bezier data at slur elements with a
--   continue type. The bezier-offset, bezier-x, and bezier-y attributes describe the outgoing
--   bezier point for slurs and ties with a start type, and the incoming bezier point for slurs
--   and ties with types of stop or continue. The attributes bezier-offset2, bezier-x2, and
--   bezier-y2 are only valid with slurs of type continue, and describe the outgoing bezier point.
--   The bezier-offset and bezier-offset2 attributes are measured in terms of musical divisions,
--   like the offset element. These are the recommended attributes for specifying horizontal
--   position. The other attributes are specified in tenths, relative to any position settings
--   associated with the slur or tied element.
data Bezier = Bezier
        { offset   :: Divisions
        , offset2  :: Divisions
        , bezierX  :: Tenths
        , bezierY  :: Tenths
        , bezierX2 :: Tenths
        , bezierY2 :: Tenths }
        deriving (Show, Eq)

type Color = TODO
type Directive = TODO
type DocumentAttributes = TODO
type Font = TODO
type HorizontalAlign = TODO
type Justify = TODO
type LetterSpacing = TODO
type LevelDisplay = TODO
type LineHeight = TODO
-- type LineShape = LineShape
-- type LineType = LineType

type Orientation = OverUnder
type Placement   = AboveBelow

-- | The position attributes are based on MuseData print suggestions. For most elements, any
-- program will compute a default x and y position. The position attributes let this be changed
-- two ways. The default-x and default-y attributes change the computation of the default
-- position. For most elements, the origin is changed relative to the left-hand side of the note
-- or the musical position within the bar (x) and the top line of the staff (y). For the following
-- elements, the default-x value changes the origin relative to the start of the current measure:
--
--     - note
--
--     - figured-bass
--
--     - harmony
--
--     - link
--
--     - directive
--
--     - measure-numbering
--
--     - all descendants of the part-list element
--
--     - all children of the direction-type element
--
-- When the part-name and part-abbreviation elements are used in the print element, the default-x
-- value changes the origin relative to the start of the first measure on the system. These values
-- are used when the current measure or a succeeding measure starts a new system. For the note,
-- figured-bass, and harmony elements, the default-x value is considered to have adjusted the
-- musical position within the bar for its descendant elements. Since the credit-words and
-- credit-image elements are not related to a measure, in these cases the default-x and default-y
-- attributes adjust the origin relative to the bottom left-hand corner of the specified page. The
-- relative-x and relative-y attributes change the position relative to the default position,
-- either as computed by the individual program, or as overridden by the default-x and default-y
-- attributes. Positive x is right, negative x is left; positive y is up, negative y is down. All
-- units are in tenths of interline space. For stems, positive relative-y lengthens a stem while
-- negative relative-y shortens it. The default-x and default-y position attributes provide
-- higher-resolution positioning data than related features such as the placement attribute and
-- the offset element. Applications reading a MusicXML file that can understand both features
-- should generally rely on the default-x and default-y attributes for their greater accuracy. For
-- the relative-x and relative-y attributes, the offset element, placement attribute, and
-- directive attribute provide context for the relative position information, so the two features
-- should be interpreted together. As elsewhere in the MusicXML format, tenths are the global
-- tenths defined by the scaling element, not the local tenths of a staff resized by the
-- staff-size element.

data Position = Position
    { defaultX  :: Tenths
    , defaultY  :: Tenths
    , relativeX :: Tenths
    , relativeY :: Tenths }
    deriving (Show, Eq)

-- | The print-object attribute specifies whether or not to print an object
-- (e.g. a note or a rest). It is yes by default.
type PrintObject = Bool
type PrintSpacing = Bool
type PrintStyle = Bool

-- | The printout attribute group collects the different controls over printing an object (e.g. a
-- note or rest) and its parts, including augmentation dots and lyrics. This is especially useful
-- for notes that overlap in different voices, or for chord sheets that contain lyrics and chords
-- but no melody. By default, all these attributes are set to yes. If print-object is set to no,
-- the print-dot and print-lyric attributes are interpreted to also be set to no if they are not
-- present.
data PrintOut = PrintStyle
    { position  :: Position
    , font      :: TODO
    , color     :: TODO }
    deriving (Show, Eq) 
    
type TextDecoration = TODO
type TextDirection = TODO
type TextFormatting = TODO
type TextRotation = TODO

-- data TrillSound

type VerticalAlign = TODO
type VerticalAlignImage = TODO
type XPosition = TODO
type YPosition = TODO
type ImageAttributes = TODO
type PrintAttributes = TODO
type ElementPosition = TODO
type LinkAttributes = TODO
type GroupNameText = TODO

-- | The measure-attributes group is used by the measure element.
--   Measures have a required number attribute (going from partwise to timewise, measures are
--   grouped via the number).
--
--   The implicit attribute is set to True for measures where the measure number should never appear,
--   such as pickup measures and the last half of mid-measure repeats. The value is False if not
--   specified.
--
--   The non-controlling attribute is intended for use in multimetric music like the
--   Don Giovanni minuet. If set to True, the left barline in this measure does not coincide with
--   the left barline of measures in other parts. The value is False if not specified. In partwise
--   files, the number attribute should be the same for measures in different parts that share the
--   same left barline.
--
--   While the number attribute is often numeric, it does not have to be. Non-numeric values are
--   typically used together with the implicit or non-controlling attributes being set to True.
--   For a pickup measure, the number attribute is typically set to 0 and the implicit attribute
--   is typically set to True. Further details about measure numbering can be defined using the
--   measure-numbering element. Measure width is specified in tenths. These are the global tenths
--   specified in the scaling element, not local tenths as modified by the staff-size element.
data MeasureAttributes = MeasureAttributes
    { number         :: String
    , implicit       :: Bool
    , nonControlling :: Bool
    , width          :: Tenths }
    deriving (Show, Eq)

type PartAttributes = TODO
type PartNameText = TODO




-- *****************************************************************************
-- Complex types
-- *****************************************************************************

{-
    <!-- Complex types derived from common.mod entities and elements -->
-}
type AccidentalText = TODO
{-
    <xs:complexType name="accidental-text">
        <xs:annotation>
            <xs:documentation>The accidental-text type represents an element with an accidental value and text-formatting attributes.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="accidental-value">
                <xs:attributeGroup ref="text-formatting"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Dynamics = TODO
{-
    <xs:complexType name="dynamics">
        <xs:annotation>
            <xs:documentation>Dynamics can be associated either with a note or a general musical direction. To avoid inconsistencies between and amongst the letter abbreviations for dynamics (what is sf vs. sfz, standing alone or with a trailing dynamic that is not always piano), we use the actual letters as the names of these dynamic elements. The other-dynamics element allows other dynamic marks that are not covered here, but many of those should perhaps be included in a more general musical direction element. Dynamics elements may also be combined to create marks not covered by a single element, such as sfmp.

These letter dynamic symbols are separated from crescendo, decrescendo, and wedge indications. Dynamic representation is inconsistent in scores. Many things are assumed by the composer and left out, such as returns to original dynamics. Systematic representations are quite complex: for example, Humdrum has at least 3 representation formats related to dynamics. The MusicXML format captures what is in the score, but does not try to be optimal for analysis or synthesis of dynamics.</xs:documentation>
        </xs:annotation>
        <xs:choice minOccurs="0" maxOccurs="unbounded">
            <xs:element name="p" type="empty"/>
            <xs:element name="pp" type="empty"/>
            <xs:element name="ppp" type="empty"/>
            <xs:element name="pppp" type="empty"/>
            <xs:element name="ppppp" type="empty"/>
            <xs:element name="pppppp" type="empty"/>
            <xs:element name="f" type="empty"/>
            <xs:element name="ff" type="empty"/>
            <xs:element name="fff" type="empty"/>
            <xs:element name="ffff" type="empty"/>
            <xs:element name="fffff" type="empty"/>
            <xs:element name="ffffff" type="empty"/>
            <xs:element name="mp" type="empty"/>
            <xs:element name="mf" type="empty"/>
            <xs:element name="sf" type="empty"/>
            <xs:element name="sfp" type="empty"/>
            <xs:element name="sfpp" type="empty"/>
            <xs:element name="fp" type="empty"/>
            <xs:element name="rf" type="empty"/>
            <xs:element name="rfz" type="empty"/>
            <xs:element name="sfz" type="empty"/>
            <xs:element name="sffz" type="empty"/>
            <xs:element name="fz" type="empty"/>
            <xs:element name="other-dynamics" type="xs:string"/>
        </xs:choice>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}
type Empty = TODO
{-
    <xs:complexType name="empty">
        <xs:annotation>
            <xs:documentation>The empty type represents an empty element with no attributes.</xs:documentation>
        </xs:annotation>
    </xs:complexType>

-}
type EmptyPLacement = TODO
{-
    <xs:complexType name="empty-placement">
        <xs:annotation>
            <xs:documentation>The empty-placement type represents an empty element with print-style and placement attributes.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}
type EmptyPrintStyle = TODO
{-    <xs:complexType name="empty-print-style">
        <xs:annotation>
            <xs:documentation>The empty-print-style type represents an empty element with print-style attributes.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="print-style"/>
    </xs:complexType>

-}
type EmptyTrillSound = TODO
{-
    <xs:complexType name="empty-trill-sound">
        <xs:annotation>
            <xs:documentation>The empty-trill-sound type represents an empty element with print-style, placement, and trill-sound attributes.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="trill-sound"/>
    </xs:complexType>


-}
type Fermata = TODO
{-
    <xs:complexType name="fermata">
        <xs:annotation>
            <xs:documentation>The fermata text content represents the shape of the fermata sign. An empty fermata element represents a normal fermata. The fermata type is upright if not specified.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="fermata-shape">
                <xs:attribute name="type" type="upright-inverted"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Fingering = TODO
{-
    <xs:complexType name="fingering">
        <xs:annotation>
            <xs:documentation>Fingering is typically indicated 1,2,3,4,5. Multiple fingerings may be given, typically to substitute fingerings in the middle of a note. The substitution and alternate values are "no" if the attribute is not present. For guitar and other fretted instruments, the fingering element represents the fretting finger; the pluck element represents the plucking finger.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="substitution" type="yes-no"/>
                <xs:attribute name="alternate" type="yes-no"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type FormattedText = TODO
{-
    <xs:complexType name="formatted-text">
        <xs:annotation>
            <xs:documentation>The formatted-text type represents a text element with text-formatting attributes.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="text-formatting"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Fret = TODO
{-
    <xs:complexType name="fret">
        <xs:annotation>
            <xs:documentation>The fret element is used with tablature notation and chord diagrams. Fret numbers start with 0 for an open string and 1 for the first fret.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:nonNegativeInteger">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Level = TODO
{-
    <xs:complexType name="level">
        <xs:annotation>
            <xs:documentation>The level type is used to specify editorial information for different MusicXML elements. If the reference attribute for the level element is yes, this indicates editorial information that is for display only and should not affect playback. For instance, a modern edition of older music may set reference="yes" on the attributes containing the music's original clef, key, and time signature. It is no by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="reference" type="yes-no"/>
                <xs:attributeGroup ref="level-display"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type MidiInstrument = TODO
{-
    <xs:complexType name="midi-instrument">
        <xs:annotation>
            <xs:documentation>The midi-instrument type defines MIDI 1.0 instrument playback. The midi-instrument element can be a part of either the score-instrument element at the start of a part, or the sound element within a part. The id attribute refers to the score-instrument affected by the change.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="midi-channel" type="midi-16" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The midi-channel element specifies a MIDI 1.0 channel numbers ranging from 1 to 16.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="midi-name" type="xs:string" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The midi-name element corresponds to a ProgramName meta-event within a Standard MIDI File.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="midi-bank" type="midi-16384" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The midi-bank element specified a MIDI 1.0 bank number ranging from 1 to 16,384.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="midi-program" type="midi-128" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The midi-program element specifies a MIDI 1.0 program number ranging from 1 to 128.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="midi-unpitched" type="midi-128" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>For unpitched instruments, the midi-unpitched element specifies a MIDI 1.0 note number ranging from 1 to 128. It is usually used with MIDI banks for percussion.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="volume" type="percent" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The volume element value is a percentage of the maximum ranging from 0 to 100, with decimal values allowed. This corresponds to a scaling value for the MIDI 1.0 channel volume controller.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="pan" type="rotation-degrees" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The pan and elevation elements allow placing of sound in a 3-D space relative to the listener. Both are expressed in degrees ranging from -180 to 180. For pan, 0 is straight ahead, -90 is hard left, 90 is hard right, and -180 and 180 are directly behind the listener.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="elevation" type="rotation-degrees" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The elevation and pan elements allow placing of sound in a 3-D space relative to the listener. Both are expressed in degrees ranging from -180 to 180. For elevation, 0 is level with the listener, 90 is directly above, and -90 is directly below.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attribute name="id" type="xs:IDREF" use="required"/>
    </xs:complexType>

-}
type NameDisplay = TODO
{-
    <xs:complexType name="name-display">
        <xs:annotation>
            <xs:documentation>The name-display type is used for exact formatting of multi-font text in part and group names to the left of the system. The print-object attribute can be used to determine what, if anything, is printed at the start of each system. Enclosure for the display-text element is none by default. Language for the display-text element is Italian ("it") by default.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice minOccurs="0" maxOccurs="unbounded">
                <xs:element name="display-text" type="formatted-text"/>
                <xs:element name="accidental-text" type="accidental-text"/>
            </xs:choice>
        </xs:sequence>
        <xs:attributeGroup ref="print-object"/>
    </xs:complexType>


-}
type StringNumber2 = TODO
{-
    <xs:complexType name="string">
        <xs:annotation>
            <xs:documentation>The string type is used with tablature notation, regular notation (where it is often circled), and chord diagrams. String numbers start with 1 for the highest string.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="string-number">
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type TypedText = TODO
{-
    <xs:complexType name="typed-text">
        <xs:annotation>
            <xs:documentation>The typed-text type represents a text element with a type attributes.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="xs:token"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type WavyLine = TODO
{-
    <xs:complexType name="wavy-line">
        <xs:annotation>
            <xs:documentation>Wavy lines are one way to indicate trills. When used with a measure element, they should always have type="continue" set.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop-continue" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="color"/>
        <xs:attributeGroup ref="trill-sound"/>
    </xs:complexType>


-}
type Attributes = TODO
{-
    <xs:complexType name="attributes">
        <xs:annotation>
            <xs:documentation>The attributes element contains musical information that typically changes on measure boundaries. This includes key and time signatures, clefs, transpositions, and staving.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="editorial"/>

            <xs:element name="divisions" type="positive-divisions" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>Musical notation duration is commonly represented as fractions. The divisions element indicates how many divisions per quarter note are used to indicate a note's duration. For example, if duration = 1 and divisions = 2, this is an eighth note duration. Duration and divisions are used directly for generating sound output, so they must be chosen to take tuplets into account. Using a divisions element lets us use just one number to represent a duration for each note in the score, while retaining the full power of a fractional representation. If maximum compatibility with Standard MIDI 1.0 files is important, do not have the divisions value exceed 16383.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="key" type="key" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The key element represents a key signature. Both traditional and non-traditional key signatures are supported. The optional number attribute refers to staff numbers. If absent, the key signature applies to all staves in the part.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="time" type="time" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>Time signatures are represented by the beats element for the numerator and the beat-type element for the denominator.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="staves" type="xs:nonNegativeInteger" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The staves element is used if there is more than one staff represented in the given part (e.g., 2 staves for typical piano parts). If absent, a value of 1 is assumed. Staves are ordered from top to bottom in a part in numerical order, with staff 1 above staff 2.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="part-symbol" type="part-symbol" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The part-symbol element indicates how a symbol for a multi-staff part is indicated in the score.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="instruments" type="xs:nonNegativeInteger" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The instruments element is only used if more than one instrument is represented in the part (e.g., oboe I and II where they play together most of the time). If absent, a value of 1 is assumed.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="clef" type="clef" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>Clefs are represented by a combination of sign, line, and clef-octave-change elements.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="staff-details" type="staff-details" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The staff-details element is used to indicate different types of staves.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="transpose" type="transpose" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>If the part is being encoded for a transposing instrument in written vs. concert pitch, the transposition must be encoded in the transpose element using the transpose type.</xs:documentation>
                </xs:annotation>
            </xs:element>

            <xs:element name="directive" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>Directives are like directions, but can be grouped together with attributes for convenience. This is typically used for tempo markings at the beginning of a piece of music. This element has been deprecated in Version 2.0 in favor of the directive attribute for direction elements. Language names come from ISO 639, with optional country subcodes from ISO 3166.</xs:documentation>
                </xs:annotation>
                <xs:complexType>
                    <xs:simpleContent>
                        <xs:extension base="xs:string">
                            <xs:attributeGroup ref="print-style"/>
                            <xs:attribute ref="xml:lang"/>
                        </xs:extension>
                    </xs:simpleContent>
                </xs:complexType>
            </xs:element>

            <xs:element name="measure-style" type="measure-style" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>A measure-style indicates a special way to print partial to multiple measures within a part. This includes multiple rests over several measures, repeats of beats, single, or multiple measures, and use of slash notation.</xs:documentation>
                </xs:annotation>
            </xs:element>

        </xs:sequence>
    </xs:complexType>

-}
type BeatRepeat = TODO
{-
    <xs:complexType name="beat-repeat">
        <xs:annotation>
            <xs:documentation>The beat-repeat type is used to indicate that a single beat (but possibly many notes) is repeated. Both the start and stop of the beat being repeated should be specified. The slashes attribute specifies the number of slashes to use in the symbol. The use-dots attribute indicates whether or not to use dots as well (for instance, with mixed rhythm patterns). By default, the value for slashes is 1 and the value for use-dots is no.

The beat-repeat element specifies a notation style for repetitions. The actual music being repeated needs to be repeated within the MusicXML file. This element specifies the notation that indicates the repeat.</xs:documentation>
        </xs:annotation>
        <xs:group ref="slash" minOccurs="0"/>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="slashes" type="xs:positiveInteger"/>
        <xs:attribute name="use-dots" type="yes-no"/>
    </xs:complexType>

    <xs:complexType name="cancel">
        <xs:annotation>
            <xs:documentation>A cancel element indicates that the old key signature should be cancelled before the new one appears. This will always happen when changing to C major or A minor and need not be specified then. The cancel value matches the fifths value of the cancelled key signature (e.g., a cancel of -2 will provide an explicit cancellation for changing from B flat major to F major). The optional location attribute indicates whether the cancellation appears to the left or the right of the new key signature. It is left by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="fifths">
                <xs:attribute name="location" type="left-right"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

    <xs:complexType name="clef">
        <xs:annotation>
            <xs:documentation>Clefs are represented by a combination of sign, line, and clef-octave-change elements. The optional number attribute refers to staff numbers within the part. A value of 1 is assumed if not present.

Sometimes clefs are added to the staff in non-standard line positions, either to indicate cue passages, or when there are multiple clefs present simultaneously on one staff. In this situation, the additional attribute is set to "yes" and the line value is ignored. The size attribute is used for clefs where the additional attribute is "yes". It is typically used to indicate cue clefs.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="sign" type="clef-sign">
                <xs:annotation>
                    <xs:documentation>The sign element represents the clef symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="line" type="staff-line" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>Line numbers are counted from the bottom of the staff. Standard values are 2 for the G sign (treble clef), 4 for the F sign (bass clef), 3 for the C sign (alto clef) and 5 for TAB (on a 6-line staff).</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="clef-octave-change" type="xs:integer" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The clef-octave-change element is used for transposing clefs. A treble clef for tenors would have a value of -1.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attribute name="number" type="staff-number"/>
        <xs:attribute name="additional" type="yes-no"/>
        <xs:attribute name="size" type="symbol-size"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="print-object"/>
    </xs:complexType>

-}
type Key = TODO
{-
    <xs:complexType name="key">
        <xs:annotation>
            <xs:documentation>The key type represents a key signature. Both traditional and non-traditional key signatures are supported. The optional number attribute refers to staff numbers. If absent, the key signature applies to all staves in the part.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice>
                <xs:group ref="traditional-key"/>
                <xs:group ref="non-traditional-key" minOccurs="0" maxOccurs="unbounded"/>
            </xs:choice>
            <xs:element name="key-octave" type="key-octave" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The optional list of key-octave elements is used to specify in which octave each element of the key signature appears.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attribute name="number" type="staff-number"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="print-object"/>
    </xs:complexType>

-}
type KeyOctave = TODO
{-
    <xs:complexType name="key-octave">
        <xs:annotation>
            <xs:documentation>The key-octave element specifies in which octave an element of a key signature appears. The content specifies the octave value using the same values as the display-octave element. The number attribute is a positive integer that refers to the key signature element in left-to-right order. If the cancel attribute is set to yes, then this number refers to an element specified by the cancel element. It is no by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="octave">
                <xs:attribute name="number" type="xs:positiveInteger" use="required"/>
                <xs:attribute name="cancel" type="yes-no"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type MeasureRepeat = TODO
{-
    <xs:complexType name="measure-repeat">
        <xs:annotation>
            <xs:documentation>The measure-repeat type is used for both single and multiple measure repeats. The text of the element indicates the number of measures to be repeated in a single pattern. The slashes attribute specifies the number of slashes to use in the repeat sign. It is 1 if not specified. Both the start and the stop of the measure-repeat must be specified. The text of the element is ignored when the type is stop.

The measure-repeat element specifies a notation style for repetitions. The actual music being repeated needs to be repeated within the MusicXML file. This element specifies the notation that indicates the repeat.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="positive-integer-or-empty">
                <xs:attribute name="type" type="start-stop" use="required"/>
                <xs:attribute name="slashes" type="xs:positiveInteger"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type MeasureStyle = TODO
{-
    <xs:complexType name="measure-style">
        <xs:annotation>
            <xs:documentation>A measure-style indicates a special way to print partial to multiple measures within a part. This includes multiple rests over several measures, repeats of beats, single, or multiple measures, and use of slash notation.

The multiple-rest and measure-repeat symbols indicate the number of measures covered in the element content. The beat-repeat and slash elements can cover partial measures. All but the multiple-rest element use a type attribute to indicate starting and stopping the use of the style. The optional number attribute specifies the staff number from top to bottom on the system, as with clef.</xs:documentation>
        </xs:annotation>
        <xs:choice>
            <xs:element name="multiple-rest" type="multiple-rest"/>
            <xs:element name="measure-repeat" type="measure-repeat"/>
            <xs:element name="beat-repeat" type="beat-repeat"/>
            <xs:element name="slash" type="slash"/>
        </xs:choice>
        <xs:attribute name="number" type="staff-number"/>
        <xs:attributeGroup ref="font"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type MultipleRest = TODO
{-
    <xs:complexType name="multiple-rest">
        <xs:annotation>
            <xs:documentation>The text of the multiple-rest type indicates the number of measures in the multiple rest. Multiple rests may use the 1-bar / 2-bar / 4-bar rest symbols, or a single shape. The use-symbols attribute indicates which to use; it is no if not specified. The element text is ignored when the type is stop.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="positive-integer-or-empty">
                <xs:attribute name="use-symbols" type="yes-no"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

    <xs:complexType name="part-symbol">
        <xs:annotation>
            <xs:documentation>The part-symbol element indicates how a symbol for a multi-staff part is indicated in the score. Values include none, brace, line, and bracket; brace is the default. The top-staff and bottom-staff elements are used when the brace does not extend across the entire part. For example, in a 3-staff organ part, the top-staff will typically be 1 for the right hand, while the bottom-staff will typically be 2 for the left hand. Staff 3 for the pedals is usually outside the brace.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="group-symbol-value">
                <xs:attribute name="top-staff" type="staff-number"/>
                <xs:attribute name="bottom-staff" type="staff-number"/>
                <xs:attributeGroup ref="position"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Slash = TODO
{-
    <xs:complexType name="slash">
        <xs:annotation>
            <xs:documentation>The slash type is used to indicate that slash notation is to be used. If the slash is on every beat, use-stems is no (the default). To indicate rhythms but not pitches, use-stems is set to yes. The type attribute indicates whether this is the start or stop of a slash notation style. The use-dots attribute works as for the beat-repeat element, and only has effect if use-stems is no.</xs:documentation>
        </xs:annotation>
        <xs:group ref="slash" minOccurs="0"/>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="use-dots" type="yes-no"/>
        <xs:attribute name="use-stems" type="yes-no"/>
    </xs:complexType>

    <xs:complexType name="staff-details">
        <xs:annotation>
            <xs:documentation>The staff-details element is used to indicate different types of staves. The optional number attribute specifies the staff number from top to bottom on the system, as with clef. The print-object attribute is used to indicate when a staff is not printed in a part, usually in large scores where empty parts are omitted. It is yes by default. If print-spacing is yes while print-object is no, the score is printed in cutaway format where vertical space is left for the empty part.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="staff-type" type="staff-type" minOccurs="0"/>
            <xs:element name="staff-lines" type="xs:nonNegativeInteger" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The staff-lines element specifies the number of lines for non 5-line staffs.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="staff-tuning" type="staff-tuning" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="capo" type="xs:nonNegativeInteger" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The capo element indicates at which fret a capo should be placed on a fretted instrument. This changes the open tuning of the strings specified by staff-tuning by the specified number of half-steps.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="staff-size" type="non-negative-decimal" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The staff-size element indicates how large a staff space is on this staff, expressed as a percentage of the work's default scaling. Values less than 100 make the staff space smaller while values over 100 make the staff space larger. A staff-type of cue, ossia, or editorial implies a staff-size of less than 100, but the exact value is implementation-dependent unless specified here. Staff size affects staff height only, not the relationship of the staff to the left and right margins.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attribute name="number" type="staff-number"/>
        <xs:attribute name="show-frets" type="show-frets"/>
        <xs:attributeGroup ref="print-object"/>
        <xs:attributeGroup ref="print-spacing"/>
    </xs:complexType>

-}
type StaffTuning = TODO
{-
    <xs:complexType name="staff-tuning">
        <xs:annotation>
            <xs:documentation>The staff-tuning type specifies the open, non-capo tuning of the lines on a tablature staff.</xs:documentation>
        </xs:annotation>
        <xs:group ref="tuning"/>
        <xs:attribute name="line" type="staff-line"/>
    </xs:complexType>

-}
type Time = TODO
{-
    <xs:complexType name="time">
        <xs:annotation>
            <xs:documentation>Time signatures are represented by the beats element for the numerator and the beat-type element for the denominator. The symbol attribute is used indicate common and cut time symbols as well as a single number display. Multiple pairs of beat and beat-type elements are used for composite time signatures with multiple denominators, such as 2/4 + 3/8. A composite such as 3+2/8 requires only one beat/beat-type pair.

The print-object attribute allows a time signature to be specified but not printed, as is the case for excerpts from the middle of a score. The value is "yes" if not present. The optional number attribute refers to staff numbers within the part. If absent, the time signature applies to all staves in the part.</xs:documentation>
        </xs:annotation>
        <xs:choice>
            <xs:sequence maxOccurs="unbounded">
                <xs:element name="beats" type="xs:string">
                    <xs:annotation>
                        <xs:documentation>The beats element indicates the number of beats, as found in the numerator of a time signature.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="beat-type" type="xs:string">
                    <xs:annotation>
                        <xs:documentation>The beat-type element indicates the beat unit, as found in the denominator of a time signature.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:sequence>
            <xs:element name="senza-misura" type="empty">
                <xs:annotation>
                    <xs:documentation>A senza-misura element explicitly indicates that no time signature is present.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:choice>
        <xs:attribute name="number" type="staff-number"/>
        <xs:attribute name="symbol" type="time-symbol"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="print-object"/>
    </xs:complexType>

-}
type Transpose = TODO
{-
    <xs:complexType name="transpose">
        <xs:annotation>
            <xs:documentation>The transpose type represents what must be added to a written pitch to get a correct sounding pitch.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="diatonic" type="xs:integer" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The diatonic element specifies the number of pitch steps needed to go from written to sounding pitch. This allows for correct spelling of enharmonic transpositions.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="chromatic" type="semitones">
                <xs:annotation>
                    <xs:documentation>The chromatic element represents the number of semitones needed to get from written to sounding pitch. This value does not include octave-change values; the values for both elements need to be added to the written pitch to get the correct sounding pitch.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="octave-change" type="xs:integer" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The octave-change element indicates how many octaves to add to get from written pitch to sounding pitch.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="double" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>If the double element is present, it indicates that the music is doubled one octave down from what is currently written (as is the case for mixed cello / bass parts in orchestral literature).</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:complexType>

    <!-- Complex types derived from barline.mod elements -->

-}
type BarStyleColor = TODO
{-
    <xs:complexType name="bar-style-color">
        <xs:annotation>
            <xs:documentation>The bar-style-color type contains barline style and color information.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="bar-style">
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

    <xs:complexType name="barline">
        <xs:annotation>
            <xs:documentation>If a barline is other than a normal single barline, it should be represented by a barline type that describes it. This includes information about repeats and multiple endings, as well as line style. Barline data is on the same level as the other musical data in a score - a child of a measure in a partwise score, or a part in a timewise score. This allows for barlines within measures, as in dotted barlines that subdivide measures in complex meters. The two fermata elements allow for fermatas on both sides of the barline (the lower one inverted).

Barlines have a location attribute to make it easier to process barlines independently of the other musical data in a score. It is often easier to set up measures separately from entering notes. The location attribute must match where the barline element occurs within the rest of the musical data in the score. If location is left, it should be the first element in the measure, aside from the print, bookmark, and link elements. If location is right, it should be the last element, again with the possible exception of the print, bookmark, and link elements. If no location is specified, the right barline is the default. The segno, coda, and divisions attributes work the same way as in the sound element. They are used for playback when barline elements contain segno or coda child elements.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="bar-style" type="bar-style-color" minOccurs="0"/>
            <xs:group ref="editorial"/>
            <xs:element name="wavy-line" type="wavy-line" minOccurs="0"/>
            <xs:element name="segno" type="empty-print-style" minOccurs="0"/>
            <xs:element name="coda" type="empty-print-style" minOccurs="0"/>
            <xs:element name="fermata" type="fermata" minOccurs="0" maxOccurs="2"/>
            <xs:element name="ending" type="ending" minOccurs="0"/>
            <xs:element name="repeat" type="repeat" minOccurs="0"/>
        </xs:sequence>
        <xs:attribute name="location" type="right-left-middle" default="right"/>
        <xs:attribute name="segno" type="xs:token"/>
        <xs:attribute name="coda" type="xs:token"/>
        <xs:attribute name="divisions" type="divisions"/>
    </xs:complexType>

-}
type Ending = TODO
{-
    <xs:complexType name="ending">
        <xs:annotation>
            <xs:documentation>The ending type represents multiple (e.g. first and second) endings. Typically, the start type is associated with the left barline of the first measure in an ending. The stop and discontinue types are associated with the right barline of the last measure in an ending. Stop is used when the ending mark concludes with a downward jog, as is typical for first endings. Discontinue is used when there is no downward jog, as is typical for second endings that do not conclude a piece. The length of the jog can be specified using the end-length attribute. The text-x and text-y attributes are offsets that specify where the baseline of the start of the ending text appears, relative to the start of the ending line.

The number attribute reflects the numeric values of what is under the ending line. Single endings such as "1" or comma-separated multiple endings such as "1,2" may be used. The ending element text is used when the text displayed in the ending is different than what appears in the number attribute. The print-object element is used to indicate when an ending is present but not printed, as is often the case for many parts in a full score.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="number" type="ending-number" use="required"/>
                <xs:attribute name="type" type="start-stop-discontinue" use="required"/>
                <xs:attributeGroup ref="print-object"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attribute name="end-length" type="tenths"/>
                <xs:attribute name="text-x" type="tenths"/>
                <xs:attribute name="text-y" type="tenths"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Repeat = TODO
{-
    <xs:complexType name="repeat">
        <xs:annotation>
            <xs:documentation>The repeat type represents repeat marks. The start of the repeat has a forward direction while the end of the repeat has a backward direction. Backward repeats that are not part of an ending can use the times attribute to indicate the number of times the repeated section is played.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="direction" type="backward-forward" use="required"/>
        <xs:attribute name="times" type="xs:nonNegativeInteger"/>
    </xs:complexType>

    <!-- Complex types derived from direction.mod elements -->

-}
type Accord = TODO
{-
    <xs:complexType name="accord">
        <xs:annotation>
            <xs:documentation>The accord type represents the tuning of a single string in the scordatura element. It uses the same group of elements as the staff-tuning element. Strings are numbered from high to low.</xs:documentation>
        </xs:annotation>
        <xs:group ref="tuning"/>
        <xs:attribute name="string" type="string-number"/>
    </xs:complexType>

-}
type AccordionRegistration = TODO
{-
    <xs:complexType name="accordion-registration">
        <xs:annotation>
            <xs:documentation>The accordion-registration type is use for accordion registration symbols. These are circular symbols divided horizontally into high, middle, and low sections that correspond to 4', 8', and 16' pipes. Each accordion-high, accordion-middle, and accordion-low element represents the presence of one or more dots in the registration diagram. An accordion-registration element needs to have at least one of the child elements present.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="accordion-high" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The accordion-high element indicates the presence of a dot in the high (4') section of the registration symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="accordion-middle" type="accordion-middle" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The accordion-middle element indicates the presence of 1 to 3 dots in the middle (8') section of the registration symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="accordion-low" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The accordion-low element indicates the presence of a dot in the low (16') section of the registration symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attributeGroup ref="print-style"/>
    </xs:complexType>

-}
type Barre = TODO
{-
    <xs:complexType name="barre">
        <xs:annotation>
            <xs:documentation>The barre element indicates placing a finger over multiple strings on a single fret. The type is "start" for the lowest pitched string (e.g., the string with the highest MusicXML number) and is "stop" for the highest pitched string.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type Bass = TODO
{-
    <xs:complexType name="bass">
        <xs:annotation>
            <xs:documentation>The bass type is used to indicate a bass note in popular music chord symbols, e.g. G/C. It is generally not used in functional harmony, as inversion is generally not used in pop chord symbols. As with root, it is divided into step and alter elements, similar to pitches.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="bass-step" type="bass-step"/>
            <xs:element name="bass-alter" type="bass-alter" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

    <xs:complexType name="bass-alter">
        <xs:annotation>
            <xs:documentation>The bass-alter type represents the chromatic alteration of the bass of the current chord within the harmony element. In some chord styles, the text for the bass-step element may include bass-alter information. In that case, the print-object attribute of the bass-alter element can be set to no. The location attribute indicates whether the alteration should appear to the left or the right of the bass-step; it is right by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="semitones">
                <xs:attributeGroup ref="print-object"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attribute name="location" type="left-right"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type BassStep = TODO
{-
    <xs:complexType name="bass-step">
        <xs:annotation>
            <xs:documentation>The bass-step type represents the pitch step of the bass of the current chord within the harmony element. The text attribute indicates how the bass should appear on the page if not using the element contents.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="step">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Bracket = TODO
{-
    <xs:complexType name="bracket">
        <xs:annotation>
            <xs:documentation>Brackets are combined with words in a variety of modern directions. The line-end attribute specifies if there is a jog up or down (or both), an arrow, or nothing at the start or end of the bracket. If the line-end is up or down, the length of the jog can be specified using the end-length attribute. The line-type is solid by default.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attribute name="line-end" type="line-end" use="required"/>
        <xs:attribute name="end-length" type="tenths"/>
        <xs:attributeGroup ref="line-type"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type Dashes = TODO
{-
    <xs:complexType name="dashes">
        <xs:annotation>
            <xs:documentation>The dashes type represents dashes, used for instance with cresc. and dim. marks.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type Degree = TODO
{-
    <xs:complexType name="degree">
        <xs:annotation>
            <xs:documentation>The degree type is used to add, alter, or subtract individual notes in the chord. The print-object attribute can be used to keep the degree from printing separately when it has already taken into account in the text attribute of the kind element. The degree-value and degree-type text attributes specify how the value and type of the degree should be displayed.

A harmony of kind "other" can be spelled explicitly by using a series of degree elements together with a root.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="degree-value" type="degree-value"/>
            <xs:element name="degree-alter" type="degree-alter"/>
            <xs:element name="degree-type" type="degree-type"/>
        </xs:sequence>
        <xs:attributeGroup ref="print-object"/>
    </xs:complexType>

-}
type DegreeAlter = TODO
{-
    <xs:complexType name="degree-alter">
        <xs:annotation>
            <xs:documentation>The degree-alter type represents the chromatic alteration for the current degree. If the degree-type value is alter or subtract, the degree-alter value is relative to the degree already in the chord based on its kind element. If the degree-type value is add, the degree-alter is relative to a dominant chord (major and perfect intervals except for a minor seventh). The plus-minus attribute is used to indicate if plus and minus symbols should be used instead of sharp and flat symbols to display the degree alteration; it is no by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="semitones">
                <xs:attributeGroup ref="print-style"/>
                <xs:attribute name="plus-minus" type="yes-no"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type DegreeType = TODO
{-
    <xs:complexType name="degree-type">
        <xs:annotation>
            <xs:documentation>The degree-type type indicates if this degree is an addition, alteration, or subtraction relative to the kind of the current chord. The value of the degree-type element affects the interpretation of the value of the degree-alter element. The text attribute specifies how the type of the degree should be displayed.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="degree-type-value">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type DegreeValue = TODO
{-
    <xs:complexType name="degree-value">
        <xs:annotation>
            <xs:documentation>The content of the degree-value type is a number indicating the degree of the chord (1 for the root, 3 for third, etc). The text attribute specifies how the type of the degree should be displayed.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:positiveInteger">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Direction = TODO
{-
    <xs:complexType name="direction">
        <xs:annotation>
            <xs:documentation>A direction is a musical indication that is not attached to a specific note. Two or more may be combined to indicate starts and stops of wedges, dashes, etc.

By default, a series of direction-type elements and a series of child elements of a direction-type within a single direction element follow one another in sequence visually. For a series of direction-type children, non-positional formatting attributes are carried over from the previous element by default.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="direction-type" type="direction-type" maxOccurs="unbounded"/>
            <xs:element name="offset" type="offset" minOccurs="0"/>
            <xs:group ref="editorial-voice-direction"/>
            <xs:group ref="staff" minOccurs="0"/>
            <xs:element name="sound" type="sound" minOccurs="0"/>
        </xs:sequence>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="directive"/>
    </xs:complexType>

-}
type DirectionType = TODO
{-
    <xs:complexType name="direction-type">
        <xs:annotation>
            <xs:documentation>Textual direction types may have more than 1 component due to multiple fonts. The dynamics element may also be used in the notations element. Attribute groups related to print suggestions apply to the individual direction-type, not to the overall direction.</xs:documentation>
        </xs:annotation>
        <xs:choice>
            <xs:element name="rehearsal" type="rehearsal" maxOccurs="unbounded"/>
            <xs:element name="segno" type="empty-print-style" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The segno element is the visual indicator of a segno sign. A sound element is needed to guide playback applications reliably.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="words" type="formatted-text" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The words element specifies a standard text direction. Left justification is assumed if not specified. Language is Italian ("it") by default. Enclosure is none by default.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="coda" type="empty-print-style" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The coda element is the visual indicator of a coda sign. A sound element is needed to guide playback applications reliably.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="wedge" type="wedge"/>
            <xs:element name="dynamics" type="dynamics" maxOccurs="unbounded"/>
            <xs:element name="dashes" type="dashes"/>
            <xs:element name="bracket" type="bracket"/>
            <xs:element name="pedal" type="pedal"/>
            <xs:element name="metronome" type="metronome"/>
            <xs:element name="octave-shift" type="octave-shift"/>
            <xs:element name="harp-pedals" type="harp-pedals"/>
            <xs:element name="damp" type="empty-print-style">
                <xs:annotation>
                    <xs:documentation>The damp element specifies a harp damping mark.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="damp-all" type="empty-print-style">
                <xs:annotation>
                    <xs:documentation>The damp-all element specifies a harp damping mark for all strings.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="eyeglasses" type="empty-print-style">
                <xs:annotation>
                    <xs:documentation>The eyeglasses element specifies the eyeglasses symbol, common in commercial music.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="scordatura" type="scordatura"/>
            <xs:element name="image" type="image"/>
            <xs:element name="accordion-registration" type="accordion-registration"/>
            <xs:element name="other-direction" type="other-direction"/>
        </xs:choice>
    </xs:complexType>

-}
type Feature = TODO
{-
    <xs:complexType name="feature">
        <xs:annotation>
            <xs:documentation>The feature type is a part of the grouping element used for musical analysis. The type attribute represents the type of the feature and the element content represents its value. This type is flexible to allow for different analyses.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="xs:token"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type FirstFret = TODO
{-
    <xs:complexType name="first-fret">
        <xs:annotation>
            <xs:documentation>The first-fret type indicates which fret is shown in the top space of the frame; it is fret 1 if the element is not present. The optional text attribute indicates how this is represented in the fret diagram, while the location attribute indicates whether the text appears to the left or right of the frame.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:positiveInteger">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attribute name="location" type="left-right"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Frame = TODO
{-
    <xs:complexType name="frame">
        <xs:annotation>
            <xs:documentation>The frame type represents a frame or fretboard diagram used together with a chord symbol. The representation is based on the NIFF guitar grid with additional information.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="frame-strings" type="xs:positiveInteger">
                <xs:annotation>
                    <xs:documentation>The frame-strings element gives the overall size of the frame in vertical lines (strings).</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="frame-frets" type="xs:positiveInteger">
                <xs:annotation>
                    <xs:documentation>The frame-frets element gives the overall size of the frame in horizontal spaces (frets).</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="first-fret" type="first-fret" minOccurs="0"/>
            <xs:element name="frame-note" type="frame-note" maxOccurs="unbounded"/>
        </xs:sequence>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="color"/>
        <xs:attributeGroup ref="halign"/>
        <xs:attributeGroup ref="valign"/>
        <xs:attribute name="height" type="tenths"/>
        <xs:attribute name="width" type="tenths"/>
    </xs:complexType>

-}
type FrameNote = TODO
{-
    <xs:complexType name="frame-note">
        <xs:annotation>
            <xs:documentation>The frame-note type represents each note included in the frame. An open string will have a fret value of 0, while a muted string will not be associated with a frame-note element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="string" type="string"/>
            <xs:element name="fret" type="fret"/>
            <xs:element name="fingering" type="fingering" minOccurs="0"/>
            <xs:element name="barre" type="barre" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

-}
type Grouping = TODO
{-
    <xs:complexType name="grouping">
        <xs:annotation>
            <xs:documentation>The grouping type is used for musical analysis. When the type attribute is "start" or "single", it usually contains one or more feature elements. The number attribute is used for distinguishing between overlapping and hierarchical groupings. The member-of attribute allows for easy distinguishing of what grouping elements are in what hierarchy. Feature elements contained within a "stop" type of grouping may be ignored.

This element is flexible to allow for different types of analyses. Future versions of the MusicXML format may add elements that can represent more standardized categories of analysis data, allowing for easier data sharing.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="feature" type="feature" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
        <xs:attribute name="type" type="start-stop-single" use="required"/>
        <xs:attribute name="number" type="xs:token" default="1"/>
        <xs:attribute name="member-of" type="xs:token"/>
    </xs:complexType>

-}
type Harmony = TODO
{-
    <xs:complexType name="harmony">
        <xs:annotation>
            <xs:documentation>The harmony type is based on Humdrum's **harm encoding, extended to support chord symbols in popular music as well as functional harmony analysis in classical music.

If there are alternate harmonies possible, this can be specified using multiple harmony elements differentiated by type. Explicit harmonies have all note present in the music; implied have some notes missing but implied; alternate represents alternate analyses.

The harmony object may be used for analysis or for chord symbols. The print-object attribute controls whether or not anything is printed due to the harmony element. The print-frame attribute controls printing of a frame or fretboard diagram. The print-style attribute group sets the default for the harmony, but individual elements can override this with their own print-style values.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="harmony-chord" maxOccurs="unbounded"/>
            <xs:element name="frame" type="frame" minOccurs="0"/>
            <xs:element name="offset" type="offset" minOccurs="0"/>
            <xs:group ref="editorial"/>
            <xs:group ref="staff" minOccurs="0"/>
        </xs:sequence>
        <xs:attribute name="type" type="harmony-type"/>
        <xs:attributeGroup ref="print-object"/>
        <xs:attribute name="print-frame" type="yes-no"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}
type HarpPedals = TODO
{-
    <xs:complexType name="harp-pedals">
        <xs:annotation>
            <xs:documentation>The harp-pedals type is used to create harp pedal diagrams. The pedal-step and pedal-alter elements use the same values as the step and alter elements. For easiest reading, the pedal-tuning elements should follow standard harp pedal order, with pedal-step values of D, C, B, E, F, G, and A.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="pedal-tuning" type="pedal-tuning" maxOccurs="unbounded"/>
        </xs:sequence>
        <xs:attributeGroup ref="print-style"/>
    </xs:complexType>

-}
type Image = TODO
{-
    <xs:complexType name="image">
        <xs:annotation>
            <xs:documentation>The image type is used to include graphical images in a score.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="image-attributes"/>
    </xs:complexType>

    <xs:complexType name="inversion">
        <xs:annotation>
            <xs:documentation>The inversion type represents harmony inversions. The value is a number indicating which inversion is used: 0 for root position, 1 for first inversion, etc.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:nonNegativeInteger">
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Kind = TODO
{-
    <xs:complexType name="kind">
        <xs:annotation>
            <xs:documentation>Kind indicates the type of chord. Degree elements can then add, subtract, or alter from these starting points

The attributes are used to indicate the formatting of the symbol. Since the kind element is the constant in all the harmony-chord groups that can make up a polychord, many formatting attributes are here.

The use-symbols attribute is yes if the kind should be represented when possible with harmony symbols rather than letters and numbers. These symbols include:

    major: a triangle, like Unicode 25B3
    minor: -, like Unicode 002D
    augmented: +, like Unicode 002B
    diminished: °, like Unicode 00B0
    half-diminished: ø, like Unicode 00F8

The text attribute describes how the kind should be spelled if not using symbols; it is ignored if use-symbols is yes. The stack-degrees attribute is yes if the degree elements should be stacked above each other. The parentheses-degrees attribute is yes if all the degrees should be in parentheses. The bracket-degrees attribute is yes if all the degrees should be in a bracket. If not specified, these values are implementation-specific. The alignment attributes are for the entire harmony-chord group of which this kind element is a part.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="kind-value">
                <xs:attribute name="use-symbols" type="yes-no"/>
                <xs:attribute name="text" type="xs:token"/>
                <xs:attribute name="stack-degrees" type="yes-no"/>
                <xs:attribute name="parentheses-degrees" type="yes-no"/>
                <xs:attribute name="bracket-degrees" type="yes-no"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="halign"/>
                <xs:attributeGroup ref="valign"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type MeasureNumbering = TODO
{-
    <xs:complexType name="measure-numbering">
        <xs:annotation>
            <xs:documentation>The measure-numbering type describes how frequently measure numbers are displayed on this part. The number attribute from the measure element is used for printing. Measures with an implicit attribute set to "yes" never display a measure number, regardless of the measure-numbering setting.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="measure-numbering-value">
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Metronome = TODO
{-
    <xs:complexType name="metronome">
        <xs:annotation>
            <xs:documentation>The metronome type represents metronome marks and other metric relationships. The beat-unit group and per-minute element specify regular metronome marks. The metronome-note and metronome-relation elements allow for the specification of more complicated metric relationships, such as swing tempo marks where two eighths are equated to a quarter note / eighth note triplet. The parentheses attribute indicates whether or not to put the metronome mark in parentheses; its value is no if not specified.</xs:documentation>
        </xs:annotation>
        <xs:choice>
            <xs:sequence>
                <xs:group ref="beat-unit"/>
                <xs:choice>
                    <xs:element name="per-minute" type="per-minute"/>
                    <xs:group ref="beat-unit"/>
                </xs:choice>
            </xs:sequence>
            <xs:sequence>
                <xs:element name="metronome-note" type="metronome-note" maxOccurs="unbounded"/>
                <xs:sequence minOccurs="0">
                    <xs:element name="metronome-relation" type="xs:string">
                        <xs:annotation>
                            <xs:documentation>The metronome-relation element describes the relationship symbol that goes between the two sets of metronome-note elements. The currently allowed value is equals, but this may expand in future versions. If the element is empty, the equals value is used.</xs:documentation>
                        </xs:annotation>
                    </xs:element>
                    <xs:element name="metronome-note" type="metronome-note" maxOccurs="unbounded"/>
                </xs:sequence>
            </xs:sequence>
        </xs:choice>
        <xs:attributeGroup ref="print-style"/>
        <xs:attribute name="parentheses" type="yes-no"/>
    </xs:complexType>

-}
type MetronomeBeam = TODO
{-
    <xs:complexType name="metronome-beam">
        <xs:annotation>
            <xs:documentation>The metronome-beam type works like the beam type in defining metric relationships, but does not include all the attributes available in the beam type.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="beam-value">
                <xs:attribute name="number" type="beam-level" default="1"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type MetronomeNote = TODO
{-
    <xs:complexType name="metronome-note">
        <xs:annotation>
            <xs:documentation>The metronome-note type defines the appearance of a note within a metric relationship mark.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="metronome-type" type="note-type-value">
                <xs:annotation>
                    <xs:documentation>The metronome-type element works like the type element in defining metric relationships.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="metronome-dot" type="empty" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The metronome-dot element works like the dot element in defining metric relationships.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="metronome-beam" type="metronome-beam" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="metronome-tuplet" type="metronome-tuplet" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

-}
type MetronomeTuplet = TODO
{-
    <xs:complexType name="metronome-tuplet">
        <xs:annotation>
            <xs:documentation>The metronome-tuplet type uses the same element structure as the time-modification element along with some attributes from the tuplet element.</xs:documentation>
        </xs:annotation>
        <xs:complexContent>
            <xs:extension base="time-modification">
                <xs:attribute name="type" type="start-stop" use="required"/>
                <xs:attribute name="bracket" type="yes-no"/>
                <xs:attribute name="show-number" type="show-tuplet"/>
            </xs:extension>
        </xs:complexContent>
    </xs:complexType>

-}
type OctaveShift = TODO
{-
    <xs:complexType name="octave-shift">
        <xs:annotation>
            <xs:documentation>The octave shift type indicates where notes are shifted up or down from their true pitched values because of printing difficulty. Thus a treble clef line noted with 8va will be indicated with an octave-shift down from the pitch data indicated in the notes. A size of 8 indicates one octave; a size of 15 indicates two octaves.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="up-down-stop" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attribute name="size" type="xs:positiveInteger" default="8"/>
        <xs:attributeGroup ref="print-style"/>
    </xs:complexType>

-}
type Offset = TODO
{-
    <xs:complexType name="offset">
        <xs:annotation>
            <xs:documentation>An offset is represented in terms of divisions, and indicates where the direction will appear relative to the current musical location. This affects the visual appearance of the direction. If the sound attribute is "yes", then the offset affects playback too. If the sound attribute is "no", then any sound associated with the direction takes effect at the current location. The sound attribute is "no" by default for compatibility with earlier versions of the MusicXML format. If an element within a direction includes a default-x attribute, the offset value will be ignored when determining the appearance of that element.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="divisions">
                <xs:attribute name="sound" type="yes-no"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type OtherDirection = TODO
{-
    <xs:complexType name="other-direction">
        <xs:annotation>
            <xs:documentation>The other-direction type is used to define any direction symbols not yet in the current version of the MusicXML format. This allows extended representation, though without application interoperability.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="print-object"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Pedal = TODO
{-
    <xs:complexType name="pedal">
        <xs:annotation>
            <xs:documentation>The pedal type represents piano pedal marks. The line attribute is yes if pedal lines are used, no if Ped and * signs are used. The change type is used with line set to yes.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop-change" use="required"/>
        <xs:attribute name="line" type="yes-no"/>
        <xs:attributeGroup ref="print-style"/>
    </xs:complexType>

-}
type PedalTuning = TODO
{-
    <xs:complexType name="pedal-tuning">
        <xs:annotation>
            <xs:documentation>The pedal-tuning type specifies the tuning of a single harp pedal.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="pedal-step" type="step">
                <xs:annotation>
                    <xs:documentation>The pedal-step element defines the pitch step for a single harp pedal.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="pedal-alter" type="semitones">
                <xs:annotation>
                    <xs:documentation>The pedal-alter element defines the chromatic alteration for a single harp pedal.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:complexType>

-}
type PerMinute = TODO
{-
    <xs:complexType name="per-minute">
        <xs:annotation>
            <xs:documentation>The per-minute type can be a number, or a text description including numbers. If a font is specified, it overrides the font specified for the overall metronome element. This allows separate specification of a music font for the beat-unit and a text font for the numeric value, in cases where a single metronome font is not used.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="font"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Print = TODO
{-
    <xs:complexType name="print">
        <xs:annotation>
            <xs:documentation>The print type contains general printing parameters, including the layout elements defined in the layout.mod file. The part-name-display and part-abbreviation-display elements used in the score.mod file may also be used here to change how a part name or abbreviation is displayed over the course of a piece. They take effect when the current measure or a succeeding measure starts a new system.

Layout elements in a print statement only apply to the current page, system, staff, or measure. Music that follows continues to take the default values from the layout included in the defaults element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="layout"/>
            <xs:element name="measure-layout" type="measure-layout" minOccurs="0"/>
            <xs:element name="measure-numbering" type="measure-numbering" minOccurs="0"/>
            <xs:element name="part-name-display" type="name-display" minOccurs="0"/>
            <xs:element name="part-abbreviation-display" type="name-display" minOccurs="0"/>
        </xs:sequence>
        <xs:attributeGroup ref="print-attributes"/>
    </xs:complexType>

-}
type Rehearsal = TODO
{-
    <xs:complexType name="rehearsal">
        <xs:annotation>
            <xs:documentation>The rehearsal type specifies a rehearsal mark. Language is Italian ("it") by default. Enclosure is square by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="text-decoration"/>
                <xs:attribute ref="xml:lang"/>
                <xs:attributeGroup ref="text-direction"/>
                <xs:attributeGroup ref="text-rotation"/>
                <xs:attribute name="enclosure" type="rehearsal-enclosure"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Root = TODO
{-
    <xs:complexType name="root">
        <xs:annotation>
            <xs:documentation>The root type indicates a pitch like C, D, E vs. a function indication like I, II, III. It is used with chord symbols in popular music. The root element has a root-step and optional root-alter element similar to the step and alter elements, but renamed to distinguish the different musical meanings.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="root-step" type="root-step"/>
            <xs:element name="root-alter" type="root-alter" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

    <xs:complexType name="root-alter">
        <xs:annotation>
            <xs:documentation>The root-alter type represents the chromatic alteration of the root of the current chord within the harmony element. In some chord styles, the text for the root-step element may include root-alter information. In that case, the print-object attribute of the root-alter element can be set to no. The location attribute indicates whether the alteration should appear to the left or the right of the root-step; it is right by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="semitones">
                <xs:attributeGroup ref="print-object"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attribute name="location" type="left-right"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type RootStep = TODO
{-
    <xs:complexType name="root-step">
        <xs:annotation>
            <xs:documentation>The root-step type represents the pitch step of the root of the current chord within the harmony element. The text attribute indicates how the root should appear on the page if not using the element contents.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="step">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Scordatura = TODO
{-
    <xs:complexType name="scordatura">
        <xs:annotation>
            <xs:documentation>Scordatura string tunings are represented by a series of accord elements, similar to the staff-tuning elements. Strings are numbered from high to low.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="accord" type="accord" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:complexType>

-}
type Sound = TODO
{-
    <xs:complexType name="sound">
        <xs:annotation>
            <xs:documentation>The sound element contains general playback parameters. They can stand alone within a part/measure, or be a component element within a direction.

Tempo is expressed in quarter notes per minute. If 0, the sound-generating program should prompt the user at the time of compiling a sound (MIDI) file.

Dynamics (or MIDI velocity) are expressed as a percentage of the default forte value (90 for MIDI 1.0).

Dacapo indicates to go back to the beginning of the movement. When used it always has the value "yes".

Segno and dalsegno are used for backwards jumps to a segno sign; coda and tocoda are used for forward jumps to a coda sign. If there are multiple jumps, the value of these parameters can be used to name and distinguish them. If segno or coda is used, the divisions attribute can also be used to indicate the number of divisions per quarter note. Otherwise sound and MIDI generating programs may have to recompute this.

By default, a dalsegno or dacapo attribute indicates that the jump should occur the first time through, while a tocoda attribute indicates the jump should occur the second time through. The time that jumps occur can be changed by using the time-only attribute.

Forward-repeat is used when a forward repeat sign is implied, and usually follows a bar line. When used it always has the value of "yes".

The fine attribute follows the final note or rest in a movement with a da capo or dal segno direction. If numeric, the value represents the actual duration of the final note or rest, which can be ambiguous in written notation and different among parts and voices. The value may also be "yes" to indicate no change to the final duration.

If the sound element applies only one time through a repeat, the time-only attribute indicates which time to apply the sound element.

Pizzicato in a sound element effects all following notes. Yes indicates pizzicato, no indicates arco.

The pan and elevation attributes are deprecated in Version 2.0. The pan and elevation elements in the midi-instrument element should be used instead. The meaning of the pan and elevation attributes is the same as for the pan and elevation elements. If both are present, the mid-instrument elements take priority.

The damper-pedal, soft-pedal, and sostenuto-pedal attributes effect playback of the three common piano pedals and their MIDI controller equivalents. The yes value indicates the pedal is depressed; no indicates the pedal is released. A numeric value from 0 to 100 may also be used for half pedaling. This value is the percentage that the pedal is depressed. A value of 0 is equivalent to no, and a value of 100 is equivalent to yes.

MIDI instruments are changed using the midi-instrument element.

The offset element is used to indicate that the sound takes place offset from the current score position. If the sound element is a child of a direction element, the sound offset element overrides the direction offset element if both elements are present. Note that the offset reflects the intended musical position for the change in sound. It should not be used to compensate for latency issues in particular hardware configurations.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="midi-instrument" type="midi-instrument" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="offset" type="offset" minOccurs="0"/>
        </xs:sequence>
        <xs:attribute name="tempo" type="non-negative-decimal"/>
        <xs:attribute name="dynamics" type="non-negative-decimal"/>
        <xs:attribute name="dacapo" type="yes-no"/>
        <xs:attribute name="segno" type="xs:token"/>
        <xs:attribute name="dalsegno" type="xs:token"/>
        <xs:attribute name="coda" type="xs:token"/>
        <xs:attribute name="tocoda" type="xs:token"/>
        <xs:attribute name="divisions" type="divisions"/>
        <xs:attribute name="forward-repeat" type="yes-no"/>
        <xs:attribute name="fine" type="xs:token"/>
        <xs:attribute name="time-only" type="xs:token"/>
        <xs:attribute name="pizzicato" type="yes-no"/>
        <xs:attribute name="pan" type="rotation-degrees"/>
        <xs:attribute name="elevation" type="rotation-degrees"/>
        <xs:attribute name="damper-pedal" type="yes-no-number"/>
        <xs:attribute name="soft-pedal" type="yes-no-number"/>
        <xs:attribute name="sostenuto-pedal" type="yes-no-number"/>
    </xs:complexType>

-}
type Wedge = TODO
{-
    <xs:complexType name="wedge">
        <xs:annotation>
            <xs:documentation>The wedge type represents crescendo and diminuendo wedge symbols. The type attribute is crescendo for the start of a wedge that is closed at the left side, and diminuendo for the start of a wedge that is closed on the right side. Spread values are measured in tenths; those at the start of a crescendo wedge or end of a diminuendo wedge are ignored.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="wedge-type" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attribute name="spread" type="tenths"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

    <!-- Complex types derived from identity.mod elements -->

-}
type Encoding = TODO
{-
    <xs:complexType name="encoding">
        <xs:annotation>
            <xs:documentation>The encoding element contains information about who did the digital encoding, when, with what software, and in what aspects. Standard type values for the encoder element are music, words, and arrangement, but other types may be used. The type attribute is only needed when there are multiple encoder elements.</xs:documentation>
        </xs:annotation>
        <xs:choice minOccurs="0" maxOccurs="unbounded">
            <xs:element name="encoding-date" type="yyyy-mm-dd"/>
            <xs:element name="encoder" type="typed-text"/>
            <xs:element name="software" type="xs:string"/>
            <xs:element name="encoding-description" type="xs:string"/>
            <xs:element name="supports" type="supports"/>
        </xs:choice>
    </xs:complexType>

-}
type Identification = TODO
{-
    <xs:complexType name="identification">
        <xs:annotation>
            <xs:documentation>Identification contains basic metadata about the score. It includes the information in MuseData headers that may apply at a score-wide, movement-wide, or part-wide level. The creator, rights, source, and relation elements are based on Dublin Core.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="creator" type="typed-text" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The creator element is borrowed from Dublin Core. It is used for the creators of the score. The type attribute is used to distinguish different creative contributions. Thus, there can be multiple creators within an identification. Standard type values are composer, lyricist, and arranger. Other type values may be used for different types of creative roles. The type attribute should usually be used even if there is just a single creator element. The MusicXML format does not use the creator / contributor distinction from Dublin Core.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="rights" type="typed-text" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The rights element is borrowed from Dublin Core. It contains copyright and other intellectual property notices. Words, music, and derivatives can have different types, so multiple rights tags with different type attributes are supported. Standard type values are music, words, and arrangement, but other types may be used. The type attribute is only needed when there are multiple rights elements.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="encoding" type="encoding" minOccurs="0"/>
            <xs:element name="source" type="xs:string" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The source for the music that is encoded. This is similar to the Dublin Core source element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="relation" type="typed-text" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>A related resource for the music that is encoded. This is similar to the Dublin Core relation element. Standard type values are music, words, and arrangement, but other types may be used.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="miscellaneous" type="miscellaneous" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

-}
type Miscellaneous = TODO
{-
    <xs:complexType name="miscellaneous">
        <xs:annotation>
            <xs:documentation>If a program has other metadata not yet supported in the MusicXML format, it can go in the miscellaneous element. The miscellaneous type puts each separate part of metadata into its own miscellaneous-field type.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="miscellaneous-field" type="miscellaneous-field" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:complexType>

-}
type MiscellaneousFields = TODO
{-
    <xs:complexType name="miscellaneous-field">
        <xs:annotation>
            <xs:documentation>If a program has other metadata not yet supported in the MusicXML format, each type of metadata can go in a miscellaneous-field element. The required name attribute indicates the type of metadata the element content represents.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="name" type="xs:token" use="required"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Supports = TODO
{-
    <xs:complexType name="supports">
        <xs:annotation>
            <xs:documentation>The supports type indicates if a MusicXML encoding supports a particular MusicXML element. This is recommended for elements like beam, stem, and accidental, where the absence of an element is ambiguous if you do not know if the encoding supports that element. For Version 2.0, the supports element is expanded to allow programs to indicate support for particular attributes or particular values. This lets applications communicate, for example, that all system and/or page breaks are contained in the MusicXML file.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="yes-no" use="required"/>
        <xs:attribute name="element" type="xs:NMTOKEN" use="required"/>
        <xs:attribute name="attribute" type="xs:NMTOKEN"/>
        <xs:attribute name="value" type="xs:token"/>
    </xs:complexType>

    <!-- Complex types derived from layout.mod elements -->

-}
type Appearance = TODO
{-
    <xs:complexType name="appearance">
        <xs:annotation>
            <xs:documentation>The appearance type controls general graphical settings for the music's final form appearance on a printed page of display. Currently this includes support for line widths and definitions for note sizes, plus an extension element for other aspects of appearance.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="line-width" type="line-width" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="note-size" type="note-size" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="other-appearance" type="other-appearance" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:complexType>

-}
type LineWidth = TODO
{-
    <xs:complexType name="line-width">
        <xs:annotation>
            <xs:documentation>The line-width type indicates the width of a line type in tenths. The type attribute defines what type of line is being defined. Values include beam, bracket, dashes, enclosure, ending, extend, heavy barline, leger, light barline, octave shift, pedal, slur middle, slur tip, staff, stem, tie middle, tie tip, tuplet bracket, and wedge. The text content is expressed in tenths.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="tenths">
                <xs:attribute name="type" type="line-width-type" use="required"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type MeasureLayout = TODO
{-
    <xs:complexType name="measure-layout">
        <xs:annotation>
            <xs:documentation>The measure-layout type includes the horizontal distance from the previous measure.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="measure-distance" type="tenths" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The measure-distance element specifies the horizontal distance from the previous measure. This value is only used for systems where there is horizontal whitespace in the middle of a system, as in systems with codas. To specify the measure width, use the width attribute of the measure element.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:complexType>

-}
type NoteSize = TODO
{-
    <xs:complexType name="note-size">
        <xs:annotation>
            <xs:documentation>The note-size type indicates the percentage of the regular note size to use for notes with a cue and large size as defined in the type element. The grace type is used for notes of cue size that that include a grace element. The cue type is used for all other notes with cue size, whether defined explicitly or implicitly via a cue element. The large type is used for notes of large size. The text content represent the numeric percentage. A value of 100 would be identical to the size of a regular note as defined by the music font.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="non-negative-decimal">
                <xs:attribute name="type" type="note-size-type" use="required"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type OtherAppearance = TODO
{-
    <xs:complexType name="other-appearance">
        <xs:annotation>
            <xs:documentation>The other-appearance type is used to define any graphical settings not yet in the current version of the MusicXML format. This allows extended representation, though without application interoperability.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="xs:token" use="required"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type PageLayout = TODO
{-
    <xs:complexType name="page-layout">
        <xs:annotation>
            <xs:documentation>Page layout can be defined both in score-wide defaults and in the print element. Page margins are specified either for both even and odd pages, or via separate odd and even page number values. The type is not needed when used as part of a print element. If omitted when used in the defaults element, "both" is the default.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:sequence minOccurs="0">
                <xs:element name="page-height" type="tenths"/>
                <xs:element name="page-width" type="tenths"/>
            </xs:sequence>
            <xs:element name="page-margins" type="page-margins" minOccurs="0" maxOccurs="2"/>
        </xs:sequence>
    </xs:complexType>

-}
type PageMarigins = TODO
{-
    <xs:complexType name="page-margins">
        <xs:annotation>
            <xs:documentation>Page margins are specified either for both even and odd pages, or via separate odd and even page number values. The type attribute is not needed when used as part of a print element. If omitted when the page-margins type is used in the defaults element, "both" is the default value.</xs:documentation>
        </xs:annotation>
        <xs:group ref="all-margins"/>
        <xs:attribute name="type" type="margin-type"/>
    </xs:complexType>

-}
type Scaling = TODO
{-
    <xs:complexType name="scaling">
        <xs:annotation>
            <xs:documentation>Margins, page sizes, and distances are all measured in tenths to keep MusicXML data in a consistent coordinate system as much as possible. The translation to absolute units is done with the scaling type, which specifies how many millimeters are equal to how many tenths. For a staff height of 7 mm, millimeters would be set to 7 while tenths is set to 40. The ability to set a formula rather than a single scaling factor helps avoid roundoff errors.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="millimeters" type="millimeters"/>
            <xs:element name="tenths" type="tenths"/>
        </xs:sequence>
    </xs:complexType>

-}
type StaffLayout = TODO
{-
    <xs:complexType name="staff-layout">
        <xs:annotation>
            <xs:documentation>Staff layout includes the vertical distance from the bottom line of the previous staff in this system to the top line of the staff specified by the number attribute. The optional number attribute refers to staff numbers within the part, from top to bottom on the system. A value of 1 is assumed if not present. When used in the defaults element, the values apply to all parts. This value is ignored for the first staff in a system.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="staff-distance" type="tenths" minOccurs="0"/>
        </xs:sequence>
        <xs:attribute name="number" type="staff-number"/>
    </xs:complexType>

-}
type SystemLayout = TODO
{-
    <xs:complexType name="system-layout">
        <xs:annotation>
            <xs:documentation>System layout includes left and right margins and the vertical distance from the previous system. The system distance is measured from the bottom line of the previous system to the top line of the current system. It is ignored for the first system on a page. The top system distance is measured from the page's top margin to the top line of the first system. It is ignored for all but the first system on a page.

Sometimes the sum of measure widths in a system may not equal the system width specified by the layout elements due to roundoff or other errors. The behavior when reading MusicXML files in these cases is application-dependent. For instance, applications may find that the system layout data is more reliable than the sum of the measure widths, and adjust the measure widths accordingly.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="system-margins" type="system-margins" minOccurs="0"/>
            <xs:element name="system-distance" type="tenths" minOccurs="0"/>
            <xs:element name="top-system-distance" type="tenths" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

-}
type SystemMargins = TODO
{-
    <xs:complexType name="system-margins">
        <xs:annotation>
            <xs:documentation>System margins are relative to the page margins. Positive values indent and negative values reduce the margin size.</xs:documentation>
        </xs:annotation>
        <xs:group ref="left-right-margins"/>
    </xs:complexType>

    <!-- Complex types derived from link.mod elements -->

-}
type Bookmark = TODO
{-
    <xs:complexType name="bookmark">
        <xs:annotation>
            <xs:documentation>The bookmark type serves as a well-defined target for an incoming simple XLink.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="id" type="xs:ID" use="required"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attributeGroup ref="element-position"/>
    </xs:complexType>

-}
type Link = TODO
{-
    <xs:complexType name="link">
        <xs:annotation>
            <xs:documentation>The link type serves as an outgoing simple XLink. It is also used to connect a MusicXML score with a MusicXML opus.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="link-attributes"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attributeGroup ref="element-position"/>
        <xs:attributeGroup ref="position"/>
    </xs:complexType>

    <!-- Complex types derived from note.mod elements -->

-}
type Accidental = TODO
{-
    <xs:complexType name="accidental">
        <xs:annotation>
            <xs:documentation>The accidental type represents actual notated accidentals. Editorial and cautionary indications are indicated by attributes. Values for these attributes are "no" if not present. Specific graphic display such as parentheses, brackets, and size are controlled by the level-display attribute group.</xs:documentation>
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
type AccidentalMark = TODO
{-
    <xs:complexType name="accidental-mark">
        <xs:annotation>
            <xs:documentation>An accidental-mark can be used as a separate notation or as part of an ornament. When used in an ornament, position and placement are relative to the ornament, not relative to the note.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="accidental-value">
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Arpeggiate = TODO
{-
    <xs:complexType name="arpeggiate">
        <xs:annotation>
            <xs:documentation>The arpeggiate type indicates that this note is part of an arpeggiated chord. The number attribute can be used to distinguish between two simultaneous chords arpeggiated separately (different numbers) or together (same number). The up-down attribute is used if there is an arrow on the arpeggio sign. By default, arpeggios go from the lowest to highest note.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="number" type="number-level"/>
        <xs:attribute name="direction" type="up-down"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}

-- | <xs:documentation>Articulations and accents are grouped together here.</xs:documentation>
type Articulations = TODO
{-
    <xs:complexType name="articulations">
        <xs:annotation>
        </xs:annotation>
        <xs:choice minOccurs="0" maxOccurs="unbounded">
            <xs:element name="accent" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The accent element indicates a regular horizontal accent mark.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="strong-accent" type="strong-accent">
                <xs:annotation>
                    <xs:documentation>The strong-accent element indicates a vertical accent mark.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="staccato" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The staccato element is used for a dot articulation, as opposed to a stroke or a wedge.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="tenuto" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The tenuto element indicates a tenuto line symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="detached-legato" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The detached-legato element indicates the combination of a tenuto line and staccato dot symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="staccatissimo" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The staccatissimo element is used for a wedge articulation, as opposed to a dot or a stroke.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="spiccato" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The spiccato element is used for a stroke articulation, as opposed to a dot or a wedge.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="scoop" type="empty-line">
                <xs:annotation>
                    <xs:documentation>The scoop element is an indeterminate slide attached to a single note. The scoop element appears before the main note and comes from below the main pitch.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="plop" type="empty-line">
                <xs:annotation>
                    <xs:documentation>The plop element is an indeterminate slide attached to a single note. The plop element appears before the main note and comes from above the main pitch.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="doit" type="empty-line">
                <xs:annotation>
                    <xs:documentation>The doit element is an indeterminate slide attached to a single note. The doit element appears after the main note and goes above the main pitch.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="falloff" type="empty-line">
                <xs:annotation>
                    <xs:documentation>The falloff element is an indeterminate slide attached to a single note. The falloff element appears before the main note and goes below the main pitch.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="breath-mark" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The breath-mark element indicates a place to take a breath. It is typically notated with a comma / apostrophe symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="caesura" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The caesura element indicates a slight pause. It is notated using a "railroad tracks" symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="stress" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The stress element indicates a stressed note.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="unstress" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The unstress element indicates an unstressed note. It is often notated using a u-shaped symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="other-articulation" type="placement-text">
                <xs:annotation>
                    <xs:documentation>The other-articulation element is used to define any articulations not yet in the MusicXML format. This allows extended representation, though without application interoperability.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:choice>
    </xs:complexType>

-}


-- | The backup and forward elements are required to coordinate multiple
-- voices in one part, including music on multiple staves. The backup type is generally used to
-- move between voices and staves. Thus the backup element does not include voice or staff
-- elements. Duration values should always be positive, and should not cross measure
-- boundaries.
type Backup = TODO
{-
    <xs:complexType name="backup">
        <xs:annotation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="duration"/>
            <xs:group ref="editorial"/>
        </xs:sequence>
    </xs:complexType>

-}
type Beam = TODO
{-
    <xs:complexType name="beam">
        <xs:annotation>
            <xs:documentation>Beam values include begin, continue, end, forward hook, and backward hook. Up to six concurrent beam levels are available to cover up to 256th notes. The repeater attribute, used for tremolos, needs to be specified with a "yes" value for each beam using it. Beams that have a begin value can also have a fan attribute to indicate accelerandos and ritardandos using fanned beams. The fan attribute may also be used with a continue value if the fanning direction changes on that note. The value is "none" if not specified.

Note that the beam number does not distinguish sets of beams that overlap, as it does for slur and other elements. Beaming groups are distinguished by being in different voices and/or the presence or absence of grace and cue elements.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="beam-value">
                <xs:attribute name="number" type="beam-level" default="1"/>
                <xs:attribute name="repeater" type="yes-no"/>
                <xs:attribute name="fan" type="fan"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Bend = TODO
{-
    <xs:complexType name="bend">
        <xs:annotation>
            <xs:documentation>The bend type is used in guitar and tablature. The bend-alter element indicates the number of steps in the bend, similar to the alter element. As with the alter element, numbers like 0.5 can be used to indicate microtones. Negative numbers indicate pre-bends or releases; the pre-bend and release elements are used to distinguish what is intended. A with-bar element indicates that the bend is to be done at the bridge with a whammy or vibrato bar. The content of the element indicates how this should be notated.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="bend-alter" type="semitones">
                <xs:annotation>
                    <xs:documentation>The bend-alter element indicates the number of steps in the bend, similar to the alter element. As with the alter element, numbers like 0.5 can be used to indicate microtones. Negative numbers indicate pre-bends or releases; the pre-bend and release elements are used to distinguish what is intended.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:choice minOccurs="0">
                <xs:element name="pre-bend" type="empty">
                    <xs:annotation>
                        <xs:documentation>The pre-bend element indicates that this is a pre-bend rather than a normal bend or a release.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="release" type="empty">
                    <xs:annotation>
                        <xs:documentation>The release element indicates that this is a release rather than a normal bend or pre-bend.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
            <xs:element name="with-bar" type="placement-text" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The with-bar element indicates that the bend is to be done at the bridge with a whammy or vibrato bar. The content of the element indicates how this should be notated.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="bend-sound"/>
    </xs:complexType>

-}
type DisplayStepOctave = TODO
{-
    <xs:complexType name="display-step-octave">
        <xs:annotation>
            <xs:documentation>The display-step-octave type contains the sequence of elements used by both the rest and unpitched elements. This group is used to place rests and unpitched elements on the staff without implying that these elements have pitch. Positioning follows the current clef. If percussion clef is used, the display-step and display-octave elements are interpreted as if in treble clef, with a G in octave 4 on line 2. If not present, the note is placed on the middle line of the staff, generally used for one-line staffs.</xs:documentation>
        </xs:annotation>
        <xs:sequence minOccurs="0">
            <xs:element name="display-step" type="step"/>
            <xs:element name="display-octave" type="octave"/>
        </xs:sequence>
    </xs:complexType>

-}
type Elision = TODO
{-
    <xs:complexType name="elision">
        <xs:annotation>
            <xs:documentation>In Version 2.0, the content of the elision type is used to specify the symbol used to display the elision. Common values are a no-break space (Unicode 00A0), an underscore (Unicode 005F), or an undertie (Unicode 203F).</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type EmptyLine = TODO
{-
    <xs:complexType name="empty-line">
        <xs:annotation>
            <xs:documentation>The empty-line type represents an empty element with line-shape, line-type, print-style and placement attributes.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="line-shape"/>
        <xs:attributeGroup ref="line-type"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}
type Extend = TODO
{-
    <xs:complexType name="extend">
        <xs:annotation>
            <xs:documentation>The extend type represents word extensions for lyrics.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="font"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type Figure = TODO
{-
    <xs:complexType name="figure">
        <xs:annotation>
            <xs:documentation>The figure type represents a single figure within a figured-bass element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="prefix" type="style-text" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>Values for the prefix element include the accidental values sharp, flat, natural, double-sharp, flat-flat, and sharp-sharp. The prefix element may contain additional values for symbols specific to particular figured bass styles.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="figure-number" type="style-text" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>A figure-number is a number. Overstrikes of the figure number are represented in the suffix element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="suffix" type="style-text" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>Values for the suffix element include the accidental values sharp, flat, natural, double-sharp, flat-flat, and sharp-sharp. Suffixes include both symbols that come after the figure number and those that overstrike the figure number. The suffix value slash is used for slashed numbers indicating chromatic alteration. The orientation and display of the slash usually depends on the figure number. The suffix element may contain additional values for symbols specific to particular figured bass styles.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="extend" type="extend" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

-}
type FiguredBass = TODO
{-
    <xs:complexType name="figured-bass">
        <xs:annotation>
            <xs:documentation>The figured-bass element represents figured bass notation. Figured bass elements take their position from the first regular note that follows. Figures are ordered from top to bottom. The value of parentheses is "no" if not present.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="figure" type="figure" maxOccurs="unbounded"/>
            <xs:group ref="duration" minOccurs="0"/>
            <xs:group ref="editorial"/>
        </xs:sequence>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="printout"/>
        <xs:attribute name="parentheses" type="yes-no"/>
    </xs:complexType>

-}

-- | The backup and forward elements are required to coordinate multiple
-- voices in one part, including music on multiple staves. The forward element is generally used
-- within voices and staves. Duration values should always be positive, and should not cross
-- measure boundaries.
type Forward = TODO
{-
    <xs:complexType name="forward">
        <xs:annotation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="duration"/>
            <xs:group ref="editorial-voice"/>
            <xs:group ref="staff" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

-}
type Glissando = TODO
{-
    <xs:complexType name="glissando">
        <xs:annotation>
            <xs:documentation>Glissando and slide types both indicate rapidly moving from one pitch to the other so that individual notes are not discerned. The distinction is similar to that between NIFF's glissando and portamento elements. A glissando sounds the half notes in between the slide and defaults to a wavy line. The optional text is printed alongside the line.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="start-stop" use="required"/>
                <xs:attribute name="number" type="number-level" default="1"/>
                <xs:attributeGroup ref="line-type"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Grace = TODO
{-
    <xs:complexType name="grace">
        <xs:annotation>
            <xs:documentation>The grace type indicates the presence of a grace note. The slash attribute for a grace note is yes for slashed eighth notes. The other grace note attributes come from MuseData sound suggestions. Steal-time-previous indicates the percentage of time to steal from the previous note for the grace note. Steal-time-following indicates the percentage of time to steal from the following note for the grace note. Make-time indicates to make time, not steal time; the units are in real-time divisions for the grace note.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="steal-time-previous" type="percent"/>
        <xs:attribute name="steal-time-following" type="percent"/>
        <xs:attribute name="make-time" type="divisions"/>
        <xs:attribute name="slash" type="yes-no"/>
    </xs:complexType>

-}
type HammerOnPulloff = TODO
{-
    <xs:complexType name="hammer-on-pull-off">
        <xs:annotation>
            <xs:documentation>The hammer-on and pull-off elements are used in guitar and fretted instrument notation. Since a single slur can be marked over many notes, the hammer-on and pull-off elements are separate so the individual pair of notes can be specified. The element content can be used to specify how the hammer-on or pull-off should be notated. An empty element leaves this choice up to the application.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="start-stop" use="required"/>
                <xs:attribute name="number" type="number-level" default="1"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Harmonic = TODO
{-
    <xs:complexType name="harmonic">
        <xs:annotation>
            <xs:documentation>The harmonic type indicates natural and artificial harmonics. Allowing the type of pitch to be specified, combined with controls for appearance/playback differences, allows both the notation and the sound to be represented. Artificial harmonics can add a notated touching-pitch; artificial pinch harmonics will usually not notate a touching pitch. The attributes for the harmonic element refer to the use of the circular harmonic symbol, typically but not always used with natural harmonics.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice minOccurs="0">
                <xs:element name="natural" type="empty">
                    <xs:annotation>
                        <xs:documentation>The natural element indicates that this is a natural harmonic. These are usually notated at base pitch rather than sounding pitch.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="artificial" type="empty">
                    <xs:annotation>
                        <xs:documentation>The artificial element indicates that this is an artificial harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
            <xs:choice minOccurs="0">
                <xs:element name="base-pitch" type="empty">
                    <xs:annotation>
                        <xs:documentation>The base pitch is the pitch at which the string is played before touching to create the harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="touching-pitch" type="empty">
                    <xs:annotation>
                        <xs:documentation>The touching-pitch is the pitch at which the string is touched lightly to produce the harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="sounding-pitch" type="empty">
                    <xs:annotation>
                        <xs:documentation>The sounding-pitch is the pitch which is heard when playing the harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
        </xs:sequence>
        <xs:attributeGroup ref="print-object"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}
type HeelToe = TODO
{-
    <xs:complexType name="heel-toe">
        <xs:annotation>
            <xs:documentation>The heel and toe elements are used with organ pedals. The substitution value is "no" if the attribute is not present.</xs:documentation>
        </xs:annotation>
        <xs:complexContent>
            <xs:extension base="empty-placement">
                <xs:attribute name="substitution" type="yes-no"/>
            </xs:extension>
        </xs:complexContent>
    </xs:complexType>

-}
type Instrument = TODO
{-
    <xs:complexType name="instrument">
        <xs:annotation>
            <xs:documentation>The instrument type distinguishes between score-instrument elements in a score-part. The id attribute is an IDREF back to the score-instrument ID. If multiple score-instruments are specified on a score-part, there should be an instrument element for each note in the part.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="id" type="xs:IDREF" use="required"/>
    </xs:complexType>

-}
type Lyric = TODO
{-
    <xs:complexType name="lyric">
        <xs:annotation>
            <xs:documentation>The lyric type represents text underlays for lyrics, based on Humdrum with support for other formats. Two text elements that are not separated by an elision element are part of the same syllable, but may have different text formatting. The MusicXML 2.0 XSD is more strict than the 2.0 DTD in enforcing this by disallowing a second syllabic element unless preceded by an elision element. The lyric number indicates multiple lines, though a name can be used as well (as in Finale's verse / chorus / section specification). Justification is center by default; placement is below by default.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice>
                <xs:sequence>
                    <xs:element name="syllabic" type="syllabic" minOccurs="0"/>
                    <xs:element name="text" type="text-element-data"/>
                    <xs:sequence minOccurs="0" maxOccurs="unbounded">
                        <xs:sequence minOccurs="0">
                            <xs:element name="elision" type="elision"/>
                            <xs:element name="syllabic" type="syllabic" minOccurs="0"/>
                        </xs:sequence>
                        <xs:element name="text" type="text-element-data"/>
                    </xs:sequence>
                    <xs:element name="extend" type="extend" minOccurs="0"/>
                </xs:sequence>
                <xs:element name="extend" type="extend"/>
                <xs:element name="laughing" type="empty">
                    <xs:annotation>
                        <xs:documentation>The laughing element is taken from Humdrum.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="humming" type="empty">
                    <xs:annotation>
                        <xs:documentation>The humming element is taken from Humdrum.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
            <xs:element name="end-line" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The end-line element comes from RP-017 for Standard MIDI File Lyric meta-events. It facilitates lyric display for Karaoke and similar applications.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="end-paragraph" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The end-paragraph element comes from RP-017 for Standard MIDI File Lyric meta-events. It facilitates lyric display for Karaoke and similar applications.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:group ref="editorial"/>
        </xs:sequence>
        <xs:attribute name="number" type="xs:NMTOKEN"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attributeGroup ref="justify"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type Mordent = TODO
{-
    <xs:complexType name="mordent">
        <xs:annotation>
            <xs:documentation>The mordent type is used for both represents the mordent sign with the vertical line and the inverted-mordent sign without the line. The long attribute is "no" by default.</xs:documentation>
        </xs:annotation>
        <xs:complexContent>
            <xs:extension base="empty-trill-sound">
                <xs:attribute name="long" type="yes-no"/>
            </xs:extension>
        </xs:complexContent>
    </xs:complexType>

-}
type NonArpeggiate = TODO
{-
    <xs:complexType name="non-arpeggiate">
        <xs:annotation>
            <xs:documentation>The non-arpeggiate type indicates that this note is at the top or bottom of a bracket indicating to not arpeggiate these notes. Since this does not involve playback, it is only used on the top or bottom notes, not on each note as for the arpeggiate type.</xs:documentation>
    </xs:annotation>
        <xs:attribute name="type" type="top-bottom" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type Notations = TODO
{-
    <xs:complexType name="notations">
        <xs:annotation>
            <xs:documentation>Notations refer to musical notations, not XML notations. Multiple notations are allowed in order to represent multiple editorial levels. The set of notations may be refined and expanded over time, especially to handle more instrument-specific technical notations.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="editorial"/>
            <xs:choice minOccurs="0" maxOccurs="unbounded">
                <xs:element name="tied" type="tied"/>
                <xs:element name="slur" type="slur"/>
                <xs:element name="tuplet" type="tuplet"/>
                <xs:element name="glissando" type="glissando"/>
                <xs:element name="slide" type="slide"/>
                <xs:element name="ornaments" type="ornaments"/>
                <xs:element name="technical" type="technical"/>
                <xs:element name="articulations" type="articulations"/>
                <xs:element name="dynamics" type="dynamics"/>
                <xs:element name="fermata" type="fermata"/>
                <xs:element name="arpeggiate" type="arpeggiate"/>
                <xs:element name="non-arpeggiate" type="non-arpeggiate"/>
                <xs:element name="accidental-mark" type="accidental-mark"/>
                <xs:element name="other-notation" type="other-notation"/>
            </xs:choice>
        </xs:sequence>
    </xs:complexType>

-}


-- | Notes are the most common type of MusicXML data. The MusicXML format keeps the 
--   MuseData distinction between elements used for sound information and elements used 
--   for notation information (e.g., tie is used for sound, tied for notation). Thus 
--   grace notes do not have a duration element. Cue notes have a duration element, as 
--   do forward elements, but no tie elements. Having these two types of information 
--   available can make interchange considerably easier, as some programs handle one type 
--   of information much more readily than the other.
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
data Note = 
    GraceNote
      {
        tie :: [Tie]    
      }
  | CueNote
      {
        duration :: Duration
        -- TODO group ref full-note
      }
  | FullNote
      {  
        duration :: Duration
  
      -- TODO group ref full-note

      , instrument        :: Maybe Instrument

      -- TODO group ref editorial-voice

      , noteType          :: Maybe NoteType

      -- One dot element is used for each dot of prolongation. The placement element is 
      -- used to specify whether the dot should appear above or below the staff line. 
      -- It is ignored for notes that appear on a staff space.
      -- , dot            :: Maybe EmptyPlacement

      , accidental        :: Maybe Accidental
      , timeModification  :: Maybe TimeModification
      , stem              :: Maybe Stem
      , noteHead          :: Maybe NoteHead
  
      -- TODO group ref staff 
  
      , beam              :: [Beam]
      , notations         :: [Notations]
      , lyric             :: [Lyric]
  
      } 

{-      


<xs:choice>
	<xs:sequence>
		<xs:element name="grace" type="grace"/>
		<xs:group ref="full-note"/>
		<xs:element name="tie" type="tie" minOccurs="0" maxOccurs="2"/>
	</xs:sequence>
	<xs:sequence>
		<xs:element name="cue" type="empty">
			<xs:annotation>
				<xs:documentation>The cue element indicates the presence of a cue note.</xs:documentation>
			</xs:annotation>
		</xs:element>
		<xs:group ref="full-note"/>
		<xs:group ref="duration"/>
	</xs:sequence>
	<xs:sequence>
		<xs:group ref="full-note"/>
		<xs:group ref="duration"/>
		<xs:element name="tie" type="tie" minOccurs="0" maxOccurs="2"/>
	</xs:sequence>
</xs:choice>


        
        <xs:attributeGroup ref="x-position"/>
        <xs:attributeGroup ref="font"/>
        <xs:attributeGroup ref="color"/>
        <xs:attributeGroup ref="printout"/>
        <xs:attribute name="dynamics" type="non-negative-decimal"/>
        <xs:attribute name="end-dynamics" type="non-negative-decimal"/>
        <xs:attribute name="attack" type="divisions"/>
        <xs:attribute name="release" type="divisions"/>
        <xs:attribute name="time-only" type="xs:token"/>
        <xs:attribute name="pizzicato" type="yes-no"/>
    </xs:complexType>

-}

-- | The note-type type indicates the graphic note type. Values range from 256th to long. The
-- size attribute indicates full, cue, or large size, with full the default for regular notes and
-- cue the default for cue and grace notes.
type NoteType = (NoteTypeValue, SymbolSize)
{-
    <xs:complexType name="note-type">
        <xs:annotation>
            <xs:documentation>The note-type type indicates the graphic note type. Values range from 256th to long. The size attribute indicates full, cue, or large size, with full the default for regular notes and cue the default for cue and grace notes.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="note-type-value">
                <xs:attribute name="size" type="symbol-size"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type NoteHead = TODO
{-
    <xs:complexType name="notehead">
        <xs:annotation>
            <xs:documentation>The notehead element indicates shapes other than the open and closed ovals associated with note durations.

For the enclosed shapes, the default is to be hollow for half notes and longer, and filled otherwise. The filled attribute can be set to change this if needed.

If the parentheses attribute is set to yes, the notehead is parenthesized. It is no by default.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="notehead-value">
                <xs:attribute name="filled" type="yes-no"/>
                <xs:attribute name="parentheses" type="yes-no"/>
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Ornaments = TODO
{-
    <xs:complexType name="ornaments">
        <xs:annotation>
            <xs:documentation>Ornaments can be any of several types, followed optionally by accidentals. The accidental-mark element's content is represented the same as an accidental element, but with a different name to reflect the different musical meaning.</xs:documentation>
        </xs:annotation>
        <xs:sequence minOccurs="0" maxOccurs="unbounded">
            <xs:choice>
                <xs:element name="trill-mark" type="empty-trill-sound">
                    <xs:annotation>
                        <xs:documentation>The trill-mark element represents the trill-mark symbol.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="turn" type="empty-trill-sound">
                    <xs:annotation>
                        <xs:documentation>The turn element is the normal turn shape which goes up then down.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="delayed-turn" type="empty-trill-sound">
                    <xs:annotation>
                        <xs:documentation>The delayed-turn element indicates a normal turn that is delayed until the end of the current note.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="inverted-turn" type="empty-trill-sound">
                    <xs:annotation>
                        <xs:documentation>The inverted-turn element has the shape which goes down and then up.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="shake" type="empty-trill-sound">
                    <xs:annotation>
                        <xs:documentation>The shake element has a similar appearance to an inverted-mordent element.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="wavy-line" type="wavy-line"/>
                <xs:element name="mordent" type="mordent">
                    <xs:annotation>
                        <xs:documentation>The mordent element represents the sign with the vertical line. The long attribute is "no" by default.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="inverted-mordent" type="mordent">
                    <xs:annotation>
                        <xs:documentation>The inverted-mordent element represents the sign without the vertical line. The long attribute is "no" by default.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="schleifer" type="empty-placement">
                    <xs:annotation>
                        <xs:documentation>The name for this ornament is based on the German, to avoid confusion with the more common slide element defined earlier.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="tremolo" type="tremolo">
                    <xs:annotation>
                        <xs:documentation>While using repeater beams was the original method for indicating tremolos, often playback and display are not well-enough integrated in an application to make that feasible. The tremolo ornament can be used to indicate either single-note or double-note tremolos.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="other-ornament" type="placement-text">
                    <xs:annotation>
                        <xs:documentation>The other-ornament element is used to define any ornaments not yet in the MusicXML format. This allows extended representation, though without application interoperability.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
            <xs:element name="accidental-mark" type="accidental-mark" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:complexType>

-}
type OtherNotation = TODO
{-
    <xs:complexType name="other-notation">
        <xs:annotation>
            <xs:documentation>The other-notation type is used to define any notations not yet in the MusicXML format. This allows extended representation, though without application interoperability. It handles notations where more specific extension elements such as other-dynamics and other-technical are not appropriate.</xs:documentation>
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
type Pitch = TODO
{-
    <xs:complexType name="pitch">
        <xs:annotation>
            <xs:documentation>Pitch is represented as a combination of the step of the diatonic scale, the chromatic alteration, and the octave.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="step" type="step"/>
            <xs:element name="alter" type="semitones" minOccurs="0"/>
            <xs:element name="octave" type="octave"/>
        </xs:sequence>
    </xs:complexType>

-}
type PlacementText = TODO
{-
    <xs:complexType name="placement-text">
        <xs:annotation>
            <xs:documentation>The placement-text type represents a text element with print-style and placement attribute groups.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Slide = TODO
{-
    <xs:complexType name="slide">
        <xs:annotation>
            <xs:documentation>Glissando and slide types both indicate rapidly moving from one pitch to the other so that individual notes are not discerned. The distinction is similar to that between NIFF's glissando and portamento elements. A slide is continuous between two notes and defaults to a solid line. The optional text for a is printed alongside the line.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="start-stop" use="required"/>
                <xs:attribute name="number" type="number-level" default="1"/>
                <xs:attributeGroup ref="line-type"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="bend-sound"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Slur = TODO
{-
    <xs:complexType name="slur">
        <xs:annotation>
            <xs:documentation>Slur types are empty. Most slurs are represented with two elements: one with a start type, and one with a stop type. Slurs can add more elements using a continue type. This is typically used to specify the formatting of cross-system slurs, or to specify the shape of very complex slurs.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop-continue" use="required"/>
        <xs:attribute name="number" type="number-level" default="1"/>
        <xs:attributeGroup ref="line-type"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="orientation"/>
        <xs:attributeGroup ref="bezier"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type Stem = TODO
{-
    <xs:complexType name="stem">
        <xs:annotation>
            <xs:documentation>Stems can be down, up, none, or double. For down and up stems, the position attributes can be used to specify stem length. The relative values specify the end of the stem relative to the program default. Default values specify an absolute end stem position. Negative values of relative-y that would flip a stem instead of shortening it are ignored.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="stem-value">
                <xs:attributeGroup ref="y-position"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type StrongAccent = TODO
{-
    <xs:complexType name="strong-accent">
        <xs:annotation>
            <xs:documentation>The strong-accent type indicates a vertical accent mark. The type attribute indicates if the point of the accent is down or up.</xs:documentation>
        </xs:annotation>
        <xs:complexContent>
            <xs:extension base="empty-placement">
                <xs:attribute name="type" type="up-down" default="up"/>
            </xs:extension>
        </xs:complexContent>
    </xs:complexType>

-}
type StyleText = TODO
{-
    <xs:complexType name="style-text">
        <xs:annotation>
            <xs:documentation>The style-text type represents a text element with a print-style attribute group.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Technical = TODO
{-
    <xs:complexType name="technical">
        <xs:annotation>
            <xs:documentation>Technical indications give performance information for individual instruments.</xs:documentation>
        </xs:annotation>
        <xs:choice minOccurs="0" maxOccurs="unbounded">
            <xs:element name="up-bow" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The up-bow element represent the symbol that is used both for up-bowing on bowed instruments, and up-stroke on plucked instruments.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="down-bow" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The down-bow element represent the symbol that is used both for down-bowing on bowed instruments, and down-stroke on plucked instruments.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="harmonic" type="harmonic"/>
            <xs:element name="open-string" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The open-string element represents the open string symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="thumb-position" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The thumb-position element represents the thumb position symbol.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="fingering" type="fingering"/>
            <xs:element name="pluck" type="placement-text">
                <xs:annotation>
                    <xs:documentation>The pluck element is used to specify the plucking fingering on a fretted instrument, where the fingering element refers to the fretting fingering. Typical values are p, i, m, a for pulgar/thumb, indicio/index, medio/middle, and anular/ring fingers.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="double-tongue" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The double-tongue element represents the double tongue symbol (two dots arranged horizontally).</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="triple-tongue" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The triple-tongue element represents the triple tongue symbol (three dots arranged horizontally).</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="stopped" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The stopped element represents the stopped symbol, which looks like a plus sign.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="snap-pizzicato" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The snap-pizzicato element represents the snap pizzicato symbol (a circle with a line).</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="fret" type="fret"/>
            <xs:element name="string" type="string"/>
            <xs:element name="hammer-on" type="hammer-on-pull-off"/>
            <xs:element name="pull-off" type="hammer-on-pull-off"/>
            <xs:element name="bend" type="bend"/>
            <xs:element name="tap" type="placement-text">
                <xs:annotation>
                    <xs:documentation>The tap element indicates a tap on the fretboard. The element content allows specification of the notation; + and T are common choices. If empty, the display is application-specific.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="heel" type="heel-toe"/>
            <xs:element name="toe" type="heel-toe"/>
            <xs:element name="fingernails" type="empty-placement">
                <xs:annotation>
                    <xs:documentation>The fingernails element is used in harp notation.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="other-technical" type="placement-text">
                <xs:annotation>
                    <xs:documentation>The other-technical element is used to define any technical indications not yet in the MusicXML format. This allows extended representation, though without application interoperability.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:choice>
    </xs:complexType>

-}
type TextElementData = TODO
{-
    <xs:complexType name="text-element-data">
        <xs:annotation>
            <xs:documentation>The text-element-data type represents a syllable or portion of a syllable for lyric text underlay. A hyphen in the string content should only be used for an actual hyphenated word. Language names for text elements come from ISO 639, with optional country subcodes from ISO 3166.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
                <xs:attributeGroup ref="text-decoration"/>
                <xs:attributeGroup ref="text-rotation"/>
                <xs:attributeGroup ref="letter-spacing"/>
                <xs:attribute ref="xml:lang"/>
                <xs:attributeGroup ref="text-direction"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Tie = TODO
{-
    <xs:complexType name="tie">
        <xs:annotation>
            <xs:documentation>The tie element indicates that a tie begins or ends with this note. The tie element indicates sound; the tied element indicates notation.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop" use="required"/>
    </xs:complexType>

-}
type Tied = TODO
{-
    <xs:complexType name="tied">
        <xs:annotation>
            <xs:documentation>The tied type represents the notated tie. The tie element represents the tie sound.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attributeGroup ref="line-type"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="orientation"/>
        <xs:attributeGroup ref="bezier"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}
type TimeModification = TODO
{-
    <xs:complexType name="time-modification">
        <xs:annotation>
            <xs:documentation>The time-modification type represents tuplets and other durational changes.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="actual-notes" type="xs:nonNegativeInteger">
                <xs:annotation>
                    <xs:documentation>The actual-notes element describes how many notes are played in the time usually occupied by the number in the normal-notes element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="normal-notes" type="xs:nonNegativeInteger">
                <xs:annotation>
                    <xs:documentation>The normal-notes element describes how many notes are usually played in the time occupied by the number in the actual-notes element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:sequence minOccurs="0">
                <xs:element name="normal-type" type="note-type-value">
                    <xs:annotation>
                        <xs:documentation>If the type associated with the number in the normal-notes element is different than the current note type (e.g., a quarter note within an eighth note triplet), then the normal-notes type (e.g. eighth) is specified in the normal-type and normal-dot elements.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="normal-dot" type="empty" minOccurs="0" maxOccurs="unbounded">
                    <xs:annotation>
                        <xs:documentation>The normal-dot element is used to specify dotted normal tuplet types.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:sequence>
        </xs:sequence>
    </xs:complexType>

-}
type Tremolo = TODO
{-
    <xs:complexType name="tremolo">
        <xs:annotation>
            <xs:documentation>While using repeater beams was the original method for indicating tremolos, often playback and display are not well-enough integrated in an application to make that feasible. The tremolo ornament can be used to indicate either single-note or double-note tremolos. Single-note tremolos use the single type, while double-note tremolos use the start and stop types. The default is "single" for compatibility with Version 1.1. The text of the element indicates the number of tremolo marks and is an integer from 0 to 6. Note that the number of attached beams is not included in this value, but is represented separately using the beam element.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="tremolo-marks">
                <xs:attribute name="type" type="start-stop-single" default="single"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Tuplet = TODO
{-
    <xs:complexType name="tuplet">
        <xs:annotation>
            <xs:documentation>A tuplet element is present when a tuplet is to be displayed graphically, in addition to the sound data provided by the time-modification elements. The number attribute is used to distinguish nested tuplets. The bracket attribute is used to indicate the presence of a bracket. If unspecified, the results are implementation-dependent. The line-shape attribute is used to specify whether the bracket is straight or in the older curved or slurred style. It is straight by default.

Whereas a time-modification element shows how the cumulative, sounding effect of tuplets compare to the written note type, the tuplet element describes how each tuplet is displayed.

The show-number attribute is used to display either the number of actual notes, the number of both actual and normal notes, or neither. It is actual by default. The show-type attribute is used to display either the actual type, both the actual and normal types, or neither. It is none by default.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="tuplet-actual" type="tuplet-portion" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The tuplet-actual element provide optional full control over how the actual part of the tuplet is displayed, including number and note type (with dots). If any of these elements are absent, their values are based on the time-modification element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="tuplet-normal" type="tuplet-portion" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The tuplet-normal element provide optional full control over how the normal part of the tuplet is displayed, including number and note type (with dots). If any of these elements are absent, their values are based on the time-modification element.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attribute name="bracket" type="yes-no"/>
        <xs:attribute name="show-number" type="show-tuplet"/>
        <xs:attribute name="show-type" type="show-tuplet"/>
        <xs:attributeGroup ref="line-shape"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}
type TupletDot = TODO
{-
    <xs:complexType name="tuplet-dot">
        <xs:annotation>
            <xs:documentation>The tuplet-dot type is used to specify dotted normal tuplet types.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="font"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

    <xs:complexType name="tuplet-number">
        <xs:annotation>
            <xs:documentation>The tuplet-number type indicates the number of notes for this portion of the tuplet.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:nonNegativeInteger">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type TupletPortion = TODO
{-
    <xs:complexType name="tuplet-portion">
        <xs:annotation>
            <xs:documentation>The tuplet-portion type provides optional full control over tuplet specifications. It allows the number and note type (including dots) to be set for the actual and normal portions of a single tuplet. If any of these elements are absent, their values are based on the time-modification element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="tuplet-number" type="tuplet-number" minOccurs="0"/>
            <xs:element name="tuplet-type" type="tuplet-type" minOccurs="0"/>
            <xs:element name="tuplet-dot" type="tuplet-dot" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:complexType>

-}
type TupletType = TODO
{-
    <xs:complexType name="tuplet-type">
        <xs:annotation>
            <xs:documentation>The tuplet-type type indicates the graphical note type of the notes for this portion of the tuplet.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="note-type-value">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

    <!-- Complex types derived from score.mod elements -->

-}
type Credit = TODO
{-
    <xs:complexType name="credit">
        <xs:annotation>
            <xs:documentation>The credit type represents the appearance of the title, composer, arranger, lyricist, copyright, dedication, and other text and graphics that commonly appears on the first page of a score. The credit-words and credit-image elements are similar to the words and image elements for directions. However, since the credit is not part of a measure, the default-x and default-y attributes adjust the origin relative to the bottom left-hand corner of the first page. The enclosure for credit-words is none by default.

By default, a series of credit-words elements within a single credit element follow one another in sequence visually. Non-positional formatting attributes are carried over from the previous element by default.

The page attribute for the credit element, new in Version 2.0, specifies the page number where the credit should appear. This is an integer value that starts with 1 for the first page. Its value is 1 by default. Since credits occur before the music, these page numbers do not refer to the page numbering specified by the print element's page-number attribute.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="link" type="link" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="bookmark" type="bookmark" minOccurs="0" maxOccurs="unbounded"/>
            <xs:choice>
                <xs:element name="credit-image" type="image"/>
                <xs:sequence>
                    <xs:element name="credit-words" type="formatted-text"/>
                    <xs:sequence minOccurs="0" maxOccurs="unbounded">
                        <xs:element name="link" type="link" minOccurs="0" maxOccurs="unbounded"/>
                        <xs:element name="bookmark" type="bookmark" minOccurs="0" maxOccurs="unbounded"/>
                        <xs:element name="credit-words" type="formatted-text"/>
                    </xs:sequence>
                </xs:sequence>
            </xs:choice>
        </xs:sequence>
        <xs:attribute name="page" type="xs:positiveInteger"/>
    </xs:complexType>

-}
type Defaults = TODO
{-
    <xs:complexType name="defaults">
        <xs:annotation>
            <xs:documentation>The defaults type specifies score-wide defaults for scaling, layout, and appearance.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="scaling" type="scaling" minOccurs="0"/>
            <xs:group ref="layout"/>
            <xs:element name="appearance" type="appearance" minOccurs="0"/>
            <xs:element name="music-font" type="empty-font" minOccurs="0"/>
            <xs:element name="word-font" type="empty-font" minOccurs="0"/>
            <xs:element name="lyric-font" type="lyric-font" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="lyric-language" type="lyric-language" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:complexType>

-}
type EmptyFont = TODO
{-
    <xs:complexType name="empty-font">
        <xs:annotation>
            <xs:documentation>The empty-font type represents an empty element with font attributes.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="font"/>
    </xs:complexType>

-}
type GroupBarline = TODO
{-
    <xs:complexType name="group-barline">
        <xs:annotation>
            <xs:documentation>The group-barline type indicates if the group should have common barlines.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="group-barline-value">
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type GroupName = TODO
{-
    <xs:complexType name="group-name">
        <xs:annotation>
            <xs:documentation>The group-name type describes the name or abbreviation of a part-group element. Formatting attributes in the group-name type are deprecated in Version 2.0 in favor of the new group-name-display and group-abbreviation-display elements.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="group-name-text"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type GroupSymbol = TODO
{-
    <xs:complexType name="group-symbol">
        <xs:annotation>
            <xs:documentation>The group-symbol type indicates how the symbol for a group is indicated in the score.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="group-symbol-value">
                <xs:attributeGroup ref="position"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type LyricFont = TODO
{-
    <xs:complexType name="lyric-font">
        <xs:annotation>
            <xs:documentation>The lyric-font type specifies the default font for a particular name and number of lyric.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="number" type="xs:NMTOKEN"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attributeGroup ref="font"/>
    </xs:complexType>

-}
type LyricLanguage = TODO
{-
    <xs:complexType name="lyric-language">
        <xs:annotation>
            <xs:documentation>The lyric-language type specifies the default language for a particular name and number of lyric.</xs:documentation>
        </xs:annotation>
        <xs:attribute name="number" type="xs:NMTOKEN"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attribute ref="xml:lang" use="required"/>
    </xs:complexType>

-}
type MidiDevice = TODO
{-
    <xs:complexType name="midi-device">
        <xs:annotation>
            <xs:documentation>The midi-device type corresponds to the DeviceName meta event in Standard MIDI Files. The optional port attribute is a number from 1 to 16 that can be used with the unofficial MIDI port (or cable) meta event.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="port" type="midi-16"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type Opus = TODO
{-
    <xs:complexType name="opus">
        <xs:annotation>
            <xs:documentation>The opus type represents a link to a MusicXML opus document that composes multiple MusicXML scores into a collection.</xs:documentation>
        </xs:annotation>
        <xs:attributeGroup ref="link-attributes"/>
    </xs:complexType>

-}
type PartGroup = TODO
{-
    <xs:complexType name="part-group">
        <xs:annotation>
            <xs:documentation>The part-group element indicates groupings of parts in the score, usually indicated by braces and brackets. Braces that are used for multi-staff parts should be defined in the attributes element for that part. The part-group start element appears before the first score-part in the group. The part-group stop element appears after the last score-part in the group.

The number attribute is used to distinguish overlapping and nested part-groups, not the sequence of groups. As with parts, groups can have a name and abbreviation. Values for the child elements are ignored at the stop of a group.

A part-group element is not needed for a single multi-staff part. By default, multi-staff parts include a brace symbol and (if appropriate given the bar-style) common barlines. The symbol formatting for a multi-staff part can be more fully specified using the part-symbol element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="group-name" type="group-name" minOccurs="0"/>
            <xs:element name="group-name-display" type="name-display" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>Formatting specified in the group-name-display element overrides formatting specified in the group-name element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="group-abbreviation" type="group-name" minOccurs="0"/>
            <xs:element name="group-abbreviation-display" type="name-display" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>Formatting specified in the group-abbreviation-display element overrides formatting specified in the group-abbreviation element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="group-symbol" type="group-symbol" minOccurs="0"/>
            <xs:element name="group-barline" type="group-barline" minOccurs="0"/>
            <xs:element name="group-time" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The group-time element indicates that the displayed time signatures should stretch across all parts and staves in the group.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:group ref="editorial"/>
        </xs:sequence>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="number" type="xs:token" default="1"/>
    </xs:complexType>

-}
type PartList = TODO
{-
    <xs:complexType name="part-list">
        <xs:annotation>
            <xs:documentation>The part-list identifies the different musical parts in this movement. Each part has an ID that is used later within the musical data. Since parts may be encoded separately and combined later, identification elements are present at both the score and score-part levels. There must be at least one score-part, combined as desired with part-group elements that indicate braces and brackets. Parts are ordered from top to bottom in a score based on the order in which they appear in the part-list.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="part-group" minOccurs="0" maxOccurs="unbounded"/>
            <xs:group ref="score-part"/>
            <xs:choice minOccurs="0" maxOccurs="unbounded">
                <xs:group ref="part-group"/>
                <xs:group ref="score-part"/>
            </xs:choice>
        </xs:sequence>
    </xs:complexType>

-}
type PartName = TODO
{-
    <xs:complexType name="part-name">
        <xs:annotation>
            <xs:documentation>The part-name type describes the name or abbreviation of a score-part element. Formatting attributes for the part-name element are deprecated in Version 2.0 in favor of the new part-name-display and part-abbreviation-display elements.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="part-name-text"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
type ScoreInstrument = TODO
{-
    <xs:complexType name="score-instrument">
        <xs:annotation>
            <xs:documentation>The score-instrument type represents a single instrument within a score-part. As with the score-part type, each score-instrument has a required ID attribute, a name, and an optional abbreviation.

A score-instrument type is also required if the score specifies MIDI 1.0 channels, banks, or programs. An initial midi-instrument assignment can also be made here. MusicXML software should be able to automatically assign reasonable channels and instruments without these elements in simple cases, such as where part names match General MIDI instrument names.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="instrument-name" type="xs:string">
                <xs:annotation>
                    <xs:documentation>The instrument-name element is typically used within a software application, rather than appearing on the printed page of a score.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="instrument-abbreviation" type="xs:string" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The optional instrument-abbreviation element is typically used within a software application, rather than appearing on the printed page of a score.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:choice minOccurs="0">
                <xs:element name="solo" type="empty">
                    <xs:annotation>
                        <xs:documentation>The solo element was added in Version 2.0. It is present if performance is intended by a solo instrument.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="ensemble" type="positive-integer-or-empty">
                    <xs:annotation>
                        <xs:documentation>The ensemble element was added in Version 2.0. It is present if performance is intended by an ensemble such as an orchestral section. The text of the ensemble element contains the size of the section, or is empty if the ensemble size is not specified.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
        </xs:sequence>
        <xs:attribute name="id" type="xs:ID" use="required"/>
    </xs:complexType>

-}
type ScorePart = TODO
{-
    <xs:complexType name="score-part">
        <xs:annotation>
            <xs:documentation>Each MusicXML part corresponds to a track in a Standard MIDI Format 1 file. The score-instrument elements are used when there are multiple instruments per track. The midi-device element is used to make a MIDI device or port assignment for the given track. Initial midi-instrument assignments may be made here as well.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="identification" type="identification" minOccurs="0"/>
            <xs:element name="part-name" type="part-name"/>
            <xs:element name="part-name-display" type="name-display" minOccurs="0"/>
            <xs:element name="part-abbreviation" type="part-name" minOccurs="0"/>
            <xs:element name="part-abbreviation-display" type="name-display" minOccurs="0"/>
            <xs:element name="group" type="xs:string" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The group element allows the use of different versions of the part for different purposes. Typical values include score, parts, sound, and data. Ordering information that is directly encoded in MuseData can be derived from the ordering within a MusicXML score or opus.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="score-instrument" type="score-instrument" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="midi-device" type="midi-device" minOccurs="0"/>
            <xs:element name="midi-instrument" type="midi-instrument" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
        <xs:attribute name="id" type="xs:ID" use="required"/>
    </xs:complexType>

-}
type Work = TODO
{-
    <xs:complexType name="work">
        <xs:annotation>
            <xs:documentation>Works are optionally identified by number and title. The work type also may indicate a link to the opus document that composes multiple scores into a collection.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="work-number" type="xs:string" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The work-number element specifies the number of a work, such as its opus number.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="work-title" type="xs:string" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The work-title element specifies the title of a work, not including its opus or other work number.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="opus" type="opus" minOccurs="0"/>
        </xs:sequence>
    </xs:complexType>

    <!-- Element groups derived from common.mod entities and elements -->

-}




-- *****************************************************************************
-- Element groups
-- *****************************************************************************

type Editorial = TODO
{-
    <xs:group name="editorial">
        <xs:annotation>
            <xs:documentation>The editorial group specifies editorial information for a musical element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="footnote" minOccurs="0"/>
            <xs:group ref="level" minOccurs="0"/>
        </xs:sequence>
    </xs:group>

-}
type EditorialVoice = TODO
{-
    <xs:group name="editorial-voice">
        <xs:annotation>
            <xs:documentation>The editorial-voice group supports the common combination of editorial and voice information for a musical element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="footnote" minOccurs="0"/>
            <xs:group ref="level" minOccurs="0"/>
            <xs:group ref="voice" minOccurs="0"/>
        </xs:sequence>
    </xs:group>

-}
type EditorialVoiceDirection = TODO
{-
    <xs:group name="editorial-voice-direction">
        <xs:annotation>
            <xs:documentation>The editorial-voice-direction group supports the common combination of editorial and voice information for a direction element. It is separate from the editorial-voice element because extensions and restrictions might be different for directions than for the note and forward elements.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="footnote" minOccurs="0"/>
            <xs:group ref="level" minOccurs="0"/>
            <xs:group ref="voice" minOccurs="0"/>
        </xs:sequence>
    </xs:group>

-}
type Footnote = TODO
{-
    <xs:group name="footnote">
        <xs:annotation>
            <xs:documentation>The footnote element specifies editorial information that appears in footnotes in the printed score. It is defined within a group due to its multiple uses within the MusicXML schema.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="footnote" type="formatted-text"/>
        </xs:sequence>
    </xs:group>

-}
type LevelGroup = TODO
{-
    <xs:group name="level">
        <xs:annotation>
            <xs:documentation>The level element specifies editorial information for different MusicXML elements. It is defined within a group due to its multiple uses within the MusicXML schema.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="level" type="level"/>
        </xs:sequence>
    </xs:group>

-}
type Staff = TODO
{-
    <xs:group name="staff">
        <xs:annotation>
            <xs:documentation>The staff element is defined within a group due to its use by both notes and direction elements.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="staff" type="xs:positiveInteger">
                <xs:annotation>
                    <xs:documentation>Staff assignment is only needed for music notated on multiple staves. Used by both notes and directions. Staff values are numbers, with 1 referring to the top-most staff in a part.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:group>

-}
type Tuning = TODO
{-
    <xs:group name="tuning">
        <xs:annotation>
            <xs:documentation>The tuning group contains the sequence of elements common to the staff-tuning and accord elements.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="tuning-step" type="step">
                <xs:annotation>
                    <xs:documentation>The tuning-step element is represented like the step element, with a different name to reflect is different function.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="tuning-alter" type="semitones" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The tuning-alter element is represented like the alter element, with a different name to reflect is different function.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="tuning-octave" type="octave">
                <xs:annotation>
                    <xs:documentation>The tuning-octave element is represented like the octave element, with a different name to reflect is different function.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:group>

-}
type Voice = TODO
{-
    <xs:group name="voice">
        <xs:annotation>
            <xs:documentation>The voice is used to distinguish between multiple voices (what MuseData calls tracks) in individual parts. It is defined within a group due to its multiple uses within the MusicXML schema.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="voice" type="xs:string"/>
        </xs:sequence>
    </xs:group>

    <!-- Element groups derived from attributes.mod elements -->

-}
type NonTraditionalKey = TODO
{-
    <xs:group name="non-traditional-key">
        <xs:annotation>
            <xs:documentation>The non-traditional-key group represents a single alteration within a non-traditional key signature. A sequence of these groups makes up a non-traditional key signature</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="key-step" type="step">
                <xs:annotation>
                    <xs:documentation>Non-traditional key signatures can be represented using the Humdrum/Scot concept of a list of altered tones. The key-step element indicates the pitch step to be altered, represented using the same names as in the step element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="key-alter" type="semitones">
                <xs:annotation>
                    <xs:documentation>Non-traditional key signatures can be represented using the Humdrum/Scot concept of a list of altered tones. The key-alter element represents the alteration for a given pitch step, represented with semitones in the same manner as the alter element.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:group>

-}
type SlashGroup = TODO
{-
    <xs:group name="slash">
        <xs:annotation>
            <xs:documentation>The slash group combines elements used for more complete specification of the slash and beat-repeat measure-style elements. They have the same values as the type and dot elements, and define what the beat is for the display of repetition marks. If not present, the beat is based on the current time signature.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="slash-type" type="note-type-value">
                <xs:annotation>
                    <xs:documentation>The slash-type element indicates the graphical note type to use for the display of repetition marks.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="slash-dot" type="empty" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The slash-dot element is used to specify any augmentation dots in the note type used to display repetition marks.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:group>

-}
type TraditionalKey = TODO
{-
    <xs:group name="traditional-key">
        <xs:annotation>
            <xs:documentation>The traditional-key group represents a traditional key signature using the cycle of fifths.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="cancel" type="cancel" minOccurs="0"/>
            <xs:element name="fifths" type="fifths"/>
            <xs:element name="mode" type="mode" minOccurs="0"/>
        </xs:sequence>
    </xs:group>

    <!-- Element groups derived from direction.mod entities and elements -->

-}
type BeatUnit = TODO
{-
    <xs:group name="beat-unit">
        <xs:annotation>
            <xs:documentation>The beat-unit group combines elements used repeatedly in the metronome element to specify a note within a metronome mark.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="beat-unit" type="note-type-value">
                <xs:annotation>
                    <xs:documentation>The beat-unit element indicates the graphical note type to use in a metronome mark.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="beat-unit-dot" type="empty" minOccurs="0" maxOccurs="unbounded">
                <xs:annotation>
                    <xs:documentation>The beat-unit-dot element is used to specify any augmentation dots for a metronome mark note.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:group>

-}
type HarmonyChord = TODO
{-
    <xs:group name="harmony-chord">
        <xs:annotation>
            <xs:documentation>A harmony element can contain many stacked chords (e.g. V of II). A sequence of harmony-chord groups is used for this type of secondary function, where V of II would be represented by a harmony-chord with a V function followed by a harmony-chord with a II function.

A root is a pitch name like C, D, E, where a function is an indication like I, II, III. It is an either/or choice to avoid data inconsistency.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice>
                <xs:element name="root" type="root"/>
                <xs:element name="function" type="style-text">
                    <xs:annotation>
                        <xs:documentation>The function element is used to represent classical functional harmony with an indication like I, II, III rather than C, D, E. It is relative to the key that is specified in the MusicXML encoding.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
            <xs:element name="kind" type="kind"/>
            <xs:element name="inversion" type="inversion" minOccurs="0"/>
            <xs:element name="bass" type="bass" minOccurs="0"/>
            <xs:element name="degree" type="degree" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:group>

    <!-- Element groups derived from layout.mod entities and elements -->

-}
type AllMargins = TODO
{-
    <xs:group name="all-margins">
        <xs:annotation>
            <xs:documentation>The all-margins group specifies both horizontal and vertical margins in tenths.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:group ref="left-right-margins"/>
            <xs:element name="top-margin" type="tenths"/>
            <xs:element name="bottom-margin" type="tenths"/>
        </xs:sequence>
    </xs:group>

-}
type Layout = TODO
{-
    <xs:group name="layout">
        <xs:annotation>
            <xs:documentation>The layout group specifies the sequence of page, system, and staff layout elements that is common to both the defaults and print elements.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="page-layout" type="page-layout" minOccurs="0"/>
            <xs:element name="system-layout" type="system-layout" minOccurs="0"/>
            <xs:element name="staff-layout" type="staff-layout" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:group>

-}
type LeftRightMargins = TODO
{-
    <xs:group name="left-right-margins">
        <xs:annotation>
            <xs:documentation>The left-right-margins group specifies horizontal margins in tenths.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="left-margin" type="tenths"/>
            <xs:element name="right-margin" type="tenths"/>
        </xs:sequence>
    </xs:group>

    <!-- Element groups derived from note.mod entities and elements -->

-}
type Duration = TODO
{-
    <xs:group name="duration">
        <xs:annotation>
            <xs:documentation>The duration element is defined within a group due to its uses within the note, figure-bass, backup, and forward elements.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="duration" type="positive-divisions">
                <xs:annotation>
                    <xs:documentation>Duration is a positive number specified in division units. This is the intended duration vs. notated duration (for instance, swing eighths vs. even eighths, or differences in dotted notes in Baroque-era music). Differences in duration specific to an interpretation or performance should use the note element's attack and release attributes.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:group>

-}
type FullNote = TODO
{-
    <xs:group name="full-note">
        <xs:annotation>
            <xs:documentation>The full-note group is a sequence of the common note elements between cue/grace notes and regular (full) notes: pitch, chord, and rest information, but not duration (cue and grace notes do not have duration encoded). Unpitched elements are used for unpitched percussion, speaking voice, and other musical elements lacking determinate pitch.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="chord" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The chord element indicates that this note is an additional chord tone with the preceding note. The duration of this note can be no longer than the preceding note. In MuseData, a missing duration indicates the same length as the previous note, but the MusicXML format requires a duration for chord notes too.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:choice>
                <xs:element name="pitch" type="pitch"/>
                <xs:element name="unpitched" type="display-step-octave">
                    <xs:annotation>
                        <xs:documentation>The unpitched element indicates musical elements that are notated on the staff but lack definite pitch, such as unpitched percussion and speaking voice.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="rest" type="display-step-octave">
                    <xs:annotation>
                        <xs:documentation>The rest element indicates notated rests or silences. Rest are usually empty, but placement on the staff can be specified using display-step and display-octave elements.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
        </xs:sequence>
    </xs:group>

    <!-- Element groups derived from score.mod entities and elements -->

-}

data MusicData =
      Note Note
    | Backup Backup
    | Forward Forward
    | Direction Direction
    | Attributes Attributes
    | Harmony Harmony
    | FiguredBass FiguredBass
    | Print Print
    | Sound Sound
    -- Barline Barline
    | Grouping Grouping
    | Link Link
    | Bookmark Bookmark
{-
    <xs:group name="music-data">
        <xs:annotation>
            <xs:documentation>The music-data group contains the basic musical data that is either associated with a part or a measure, depending on whether the partwise or timewise hierarchy is used.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice minOccurs="0" maxOccurs="unbounded">
                <xs:element name="note" type="note"/>
                <xs:element name="backup" type="backup"/>
                <xs:element name="forward" type="forward"/>
                <xs:element name="direction" type="direction"/>
                <xs:element name="attributes" type="attributes"/>
                <xs:element name="harmony" type="harmony"/>
                <xs:element name="figured-bass" type="figured-bass"/>
                <xs:element name="print" type="print"/>
                <xs:element name="sound" type="sound"/>
                <xs:element name="barline" type="barline"/>
                <xs:element name="grouping" type="grouping"/>
                <xs:element name="link" type="link"/>
                <xs:element name="bookmark" type="bookmark"/>
            </xs:choice>
        </xs:sequence>
    </xs:group>

-}
type PartGroupGroup = TODO
{-
    <xs:group name="part-group">
        <xs:annotation>
            <xs:documentation>The part-group element is defined within a group due to its multiple uses within the part-list element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="part-group" type="part-group">
            </xs:element>
        </xs:sequence>
    </xs:group>

-}

-- | The ScoreHeader group contains basic score metadata about the work and movement,
--   score-wide defaults for layout and fonts, credits that appear on the first or
--   following pages, and the part list.
data ScoreHeader = ScoreHeader
    { work            :: Maybe Work
    , movementNumber  :: Maybe String
    , movementTitle   :: Maybe String
    , identification  :: Maybe Identification
    , defaults        :: Maybe Defaults
    , credit          :: [Credit]
    , partList        :: PartList }
{-
    <xs:group name="score-header">
        <xs:sequence>
            <xs:element name="work" type="work" minOccurs="0"/>
            <xs:element name="movement-number" type="xs:string" minOccurs="0">
            </xs:element>
            <xs:element name="movement-title" type="xs:string" minOccurs="0">
            </xs:element>
            <xs:element name="identification" type="identification" minOccurs="0"/>
            <xs:element name="defaults" type="defaults" minOccurs="0"/>
            <xs:element name="credit" type="credit" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="part-list" type="part-list"/>
        </xs:sequence>
    </xs:group>

    <xs:group name="score-part">
        <xs:annotation>
            <xs:documentation>The score-part element is defined within a group due to its multiple uses within the part-list element.</xs:documentation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="score-part" type="score-part">
                <xs:annotation>
                    <xs:documentation>Each MusicXML part corresponds to a track in a Standard MIDI Format 1 file. The score-instrument elements are used when there are multiple instruments per track. The midi-device element is used to make a MIDI device or port assignment for the given track. Initial midi-instrument assignments may be made here as well.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
    </xs:group>

    <xs:annotation>
        <xs:documentation>The score is the root element for the schema. It includes the score-header group, followed either by a series of parts with measures inside (score-partwise) or a series of measures with parts inside (score-timewise). Having distinct top-level elements for partwise and timewise scores makes it easy to ensure that an XSLT stylesheet does not try to transform a document already in the desired format.</xs:documentation>
    </xs:annotation>
-}


-- *****************************************************************************
-- Root elements
-- *****************************************************************************


-- | The score-partwise element is the root element for a partwise MusicXML score. 
--   It includes a score-header group followed by a series of parts with measures 
--   inside. The document-attributes attribute group includes the version attribute.
type ScorePartwise = (ScoreHeader, [[(MeasureAttributes, [MusicData])]])

-- | The score-timewise element is the root element for a timewise MusicXML score.
--   It includes a score-header group followed by a series of measures with parts 
--   inside. The document-attributes attribute group includes the version attribute.
type ScoreTimewise = (ScoreHeader, [(MeasureAttributes, [[MusicData]])])

-- | Placeholder for unimplemented types. Can not be initiated.
data TODO = Dummy deriving (Show, Eq, Enum)
