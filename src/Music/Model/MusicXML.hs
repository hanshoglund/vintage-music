
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

-- ** Text
, FontStyle(..)
, FontWeight(..)


-- ------------------------------------------------------------------------- --
-- * Attribute groups
-- ------------------------------------------------------------------------- --

, BendSound(..)
, TrillSound(..)

, Bezier(..)
, Directive

, HorizontalAlign
, LetterSpacing
, LevelDisplay
, LineHeight

, Orientation
, Placement
, Position(..)
, PrintStyle(..)
, PrintOut(..)
, VerticalAlign(..)
, VerticalAlignImage(..)
, ElementPosition
, GroupNameText

, DocumentAttributes(..)
, LinkAttributes
, ImageAttributes
, PrintAttributes(..)
, MeasureAttributes (..)
, PartAttributes(..)


-- ------------------------------------------------------------------------- --
-- * Complex types
-- ------------------------------------------------------------------------- --

, AccidentalText
, Dynamics
, Empty
, EmptyPlacement
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
, Barline
, GroupName
, GroupSymbol
, LyricFont
, LyricLanguage
, MidiDevice
, Opus(..)
, PartGroup(..)
, PartList(..)
, PartName(..)
, ScoreInstrument(..)
, ScorePart(..)
, Work(..)

-- ------------------------------------------------------------------------- --
-- * Element groups
-- ------------------------------------------------------------------------- --

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
, FullNote(..)
, MusicData(..)
, ScoreHeader(..)


-- ------------------------------------------------------------------------- --
-- * Root elements
-- ------------------------------------------------------------------------- --

, Part
, Measure
, Score(..)

)

where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Articulations
import Music.Model.MusicXML.Harmony
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Note
import Music.Model.MusicXML.Opus
import Music.Model.MusicXML.Score
import Music.Model.MusicXML.Sound
import Music.Model.MusicXML.Text
import Music.Model.MusicXML.Tuplet

