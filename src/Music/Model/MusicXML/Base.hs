

module Music.Model.MusicXML.Base
( 
TODO
-- ------------------------------------------------------------------------- --
-- * Simple types
-- ------------------------------------------------------------------------- --

-- ** Numeric
, Divisions
, Tenths
, StringNumber
, NumberLevel
, BeamLevel
, Percent
, RotationDegrees
, StaffLine
, StaffNumber
, Midi16
, Midi128
, Midi16384
, Color
, NumberOrNormal(..)

-- ** Placement and directions
, AboveBelow
, OverUnder
, TopBottom
, UpDown
, BackwardForward
, above, below, over, under, top, bottom, up, down, forward, backward
, LeftCenterRight(..) 

-- ** Continuity

, StartStop
, StartStopContinue
, StartStopSingle
, StartStopDiscontinoue
, SSCDS(..)


-- ** Staves
, StaffType(..)
, MeasureNumberingValue(..)
, BarlineValue(..)
, GroupSymbolValue(..)

-- ** Measures
, BarStyle(..)

-- ** Key, time and clef
, Fifths
, Mode(..)
, TimeSymbol(..)
, ClefSign(..)

-- ** Notes
, NoteTypeValue
, eighth, quarter, half, whole, breve, long
, Octave
, Semitones
, AccidentalValue(..)
, NoteHeadValue(..)
, SymbolSize(..)
, NoteSizeType(..)
, BeamValue(..)
, Fan(..)   
, StemValue(..)
, Step(..)
, TremoloMarks
, ShowTuplet(..)

-- ** Lines
, LineShape(..)
, LineType(..)
, LineEnd(..)
, WedgeType(..)
, TrillBeats
, TrillStep(..)
, StartNote(..)
, TwoNoteTurn(..) 
)
where

import Data.Word


-- | Placeholder for unimplemented types. Can not be initiated.
data TODO = Dummy deriving (Show, Eq, Enum)


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

type Color = Word32                                   

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


data LeftCenterRight = Left | Center | Right
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
data BarlineValue = Common | NotCommon | Mensurstrich
    deriving (Show, Eq, Enum)

-- | The group-symbol-value type indicates how the symbol for a group is indicated in the score.
-- The default value is none.
data GroupSymbolValue  = Brace | Line | Bracket | NoSymbol
    deriving (Show, Eq, Enum)



-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

-- *****************************************************************************
-- Complex types
-- *****************************************************************************

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************
