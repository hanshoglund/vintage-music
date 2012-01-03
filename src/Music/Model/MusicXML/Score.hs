
-- ------------------------------------------------------------

{- |
   Module     : Music.Model.MusicXML.Score
   Copyright  : Copyright (C) 2012 Hans Höglund
   License    : MIT

   Maintainer : Hans Hoglund (hans@hanshoglund.se)
   Stability  : experimental
   Portability: non-portable

   Defines the root element 'Score' and related information such as the score
   header, basic attributes and meta-information such as credits, layout options
   etc. Each score must have at lest one part. In the trivial case, a simple
   treble clef part is provided. For a more complicated instrumention, see 'PartList'.

   The actual musical data is wrapped up in the 'MusicData' type, which references
   types defined in other modules, notably 'Note', 'Direction' and 'Attributes'.
-}

-- ------------------------------------------------------------

module Music.Model.MusicXML.Score
(
-- * Root elements
      Score(..)
    , Part(..)
    , Measure(..)

-- * Basic Attributes
    , MeasureAttributes(..)
    , PartAttributes(..)
    , DocumentAttributes(..)

-- * Header
    , ScoreHeader(..)

-- ** Meta-information
    , Work(..)
    , Identification(..)
    , Defaults(..)
    , Credit(..)

-- ** Parts and groups
    , PartList
    , ScorePart(..)
    , PartGroup(..)
    , PartName
    , GroupName(..)
    , GroupSymbol(..)
    , MidiDevice(..)
    , MidiInstrument(..)
    , ScoreInstrument(..)

-- * Body
    , MusicData(..)
)

where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Articulations
import Music.Model.MusicXML.Attributes
import Music.Model.MusicXML.Harmony
import Music.Model.MusicXML.Image
import Music.Model.MusicXML.Sound
import Music.Model.MusicXML.Note
import Music.Model.MusicXML.Opus
import Music.Model.MusicXML.Write


-- ------------------------------------------------------------

-- | The score is the root element for the schema. It includes the 'ScoreHeader' group, followed either
--   by a series of parts with measures inside or a series of measures with parts inside.

data Score
    = PartwiseScore { attributes :: Maybe DocumentAttributes
                    , header     :: ScoreHeader
                    , parts      :: [Part] }

    | TimewiseScore { attributes :: Maybe DocumentAttributes
                    , header     :: ScoreHeader
                    , measures   :: [Measure] }


instance Trivial Score where
    trivial = PartwiseScore trivial trivial trivial

instance WriteXml Score where
    writeXml (PartwiseScore a h p) = element "score-partwise"
                                         [writeXml a]
                                         [writeXml h <+> writeXml p]

    writeXml (TimewiseScore a h p) = element "score-timewise"
                                         [writeXml a]
                                         [writeXml h <+> writeXml p]

type Part    = [(MeasureAttributes, [MusicData])]
type Measure = (MeasureAttributes, [[MusicData]])

instance WriteXml Part where
    writeXml = undefined

instance WriteXml Measure where
    writeXml = undefined


-- ------------------------------------------------------------

-- Attribute groups

-- | The MeasureAttributes group is used by the measure element. Measures have a required number
--   attribute (going from partwise to timewise, measures are grouped via the number).
--
--   The implicit attribute is set to 'True' for measures where the measure number should never appear,
--   such as pickup measures and the last half of mid-measure repeats. The value is 'False' if not
--   specified.
--
--   The nonControlling attribute is intended for use in multimetric music.
--   If set to 'True', the left barline in this measure does not coincide with
--   the left barline of measures in other parts. The value is 'False' if not specified. In partwise
--   files, the number attribute should be the same for measures in different parts that share the
--   same left barline.
--
--   While the number attribute is often numeric, it does not have to be. Non-numeric values are
--   typically used together with the implicit or nonControlling attributes being set to 'True'.
--   For a pickup measure, the number attribute is typically set to 0 and the implicit attribute
--   is typically set to 'True'. Further details about measure numbering can be defined using the
--   measureNumbering element. Measure width is specified in tenths. These are the global tenths
--   specified in the scaling element, not local tenths as modified by the staffSize element.

data MeasureAttributes = MeasureAttributes { number         :: String
                                           , implicit       :: Maybe Bool
                                           , nonControlling :: Maybe Bool
                                           , width          :: Maybe Tenths }
                                           deriving (Show, Eq)

instance Trivial MeasureAttributes where
    trivial = MeasureAttributes "0" Nothing Nothing Nothing

-- instance WriteXml MeasureAttributes where
--     writeXml a = stringAttrs [("version", version a)]


-- | In either partwise or timewise format, the part element has an id attribute that is a reference
--   back to a 'ScorePart' in the 'PartList'.
data PartAttributes = PartAttributes { partAttributesId :: String }

instance WriteXml PartAttributes where
    writeXml a = stringAttrs [("id", partAttributesId a)]


-- | The DocumentAttributes attribute group is used to specify the attributes for an entire MusicXML
--   document. Currently this is used for the version attribute.
--
--   The version attribute was added in Version 1.1 for the 'PartwiseScore' and 'TimewiseScore'
--   documents. It provides an easier way to get version information than through the MusicXML public
--   ID. The default value is 1.0 to make it possible for programs that handle later versions to
--   distinguish earlier version files reliably. Programs that write MusicXML 1.1 or 2.0 files should
--   set this attribute.
data DocumentAttributes = DocumentAttributes { version :: String }

instance WriteXml DocumentAttributes where
    writeXml a = stringAttrs [("version", version a)]


-- ------------------------------------------------------------

-- | Works are optionally identified by number and title. The work type also may indicate a link to
--   the 'Opus' document that composes multiple scores into a collection.
data Work = Work {
                 -- | The workNumber element specifies the number of a work, such as its opus
                 -- number.
                 workNumber :: Maybe String
                 -- | The workTitle element specifies the title of a work, not including its opus or
                 -- other work number.
                 , workTitle  :: Maybe String
                 , opus       :: Maybe Opus
                 }

instance Trivial Work where
    trivial = Work Nothing Nothing Nothing

instance WriteXml Work where
    writeXml work = element "work" [] elements
        where elements = [ element "work-number" [] [writeXml $ workNumber work ]
                         , element "work-title"  [] [writeXml $ workTitle  work ]
                         , element "opus"        [] [writeXml $ opus       work ] ]

-- | Identification contains basic metadata about the score. It includes the information in MuseData
--   headers that may apply at a score-wide, movement-wide, or part-wide level. The creator, rights,
--   source, and relation elements are based on Dublin Core.
type Identification = TODO


-- | The defaults type specifies score-wide defaults for scaling, layout, and appearance.
type Defaults = TODO

-- | The credit type represents the appearance of the title, composer, arranger, lyricist, copyright,
--   dedication, and other text and graphics that commonly appears on the first page of a score. The
--   creditWords and creditImage elements are similar to the words and image elements for directions.
--   However, since the credit is not part of a measure, the default-x and default-y attributes adjust
--   the origin relative to the bottom left-hand corner of the first page. The enclosure for
--   credit-words is none by default.
--   
--   By default, a series of credit-words elements within a single credit element follow one another in
--   sequence visually. Non-positional formatting attributes are carried over from the previous element
--   by default.
--   
--   The page attribute for the credit element, new in Version 2.0, specifies the page number where the
--   credit should appear. This is an integer value that starts with 1 for the first page. Its value is
--   1 by default. Since credits occur before the music, these page numbers do not refer to the page
--   numbering specified by the print element's pageNumber attribute.

type Credit = TODO


-- ------------------------------------------------------------

-- | The MidiDevice type corresponds to the DeviceName meta event in Standard MIDI Files. The
--   optional port attribute is a number from 1 to 16 that can be used with the unofficial MIDI port
--   (or cable) meta event.
type MidiDevice = TODO

-- | The MidiInstrument type defines MIDI 1.0 instrument playback. The MidiInstrument element can
--   be a part of either the 'ScoreInstrument' element at the start of a part, or the sound element
--   within a part. The id attribute refers to the ScoreInstrument affected by the change.
type MidiInstrument = TODO

-- | The ScoreInstrument type represents a single instrument within a score-part. As with the
--   'ScorePart' type, each ScoreInstrument has a required ID attribute, a name, and an optional
--   abbreviation.
--
--   A ScoreInstrument type is also required if the score specifies MIDI 1.0 channels, banks, or
--   programs. An initial MidiInstrument assignment can also be made here. MusicXML software should be
--   able to automatically assign reasonable channels and instruments without these elements in simple
--   cases, such as where part names match General MIDI instrument names.

type ScoreInstrument = TODO

-- | The PartName type describes the name or abbreviation of a 'ScorePart' element. Formatting
--   attributes for the PartName element are deprecated in Version 2.0 in favor of the new
--   'PartNameDisplay' and 'PartAbbreviationDisplay' elements.
type PartName = TODO

-- | Each MusicXML part corresponds to a track in a Standard MIDI Format 1 file. The 'ScoreInstrument'
--   elements are used when there are multiple instruments per track. The midi-device element is used to
--   make a MIDI device or port assignment for the given track. Initial 'MidiInstrument' assignments may
--   be made here as well.
data ScorePart
    = ScorePart { partIdentification      :: Maybe Identification
                , partName                :: Maybe PartName
                , partNameDisplay         :: Maybe NameDisplay
                , partAbbreviation        :: Maybe PartName
                , partAbbreviationDisplay :: Maybe NameDisplay
                -- | The group element allows the use of different versions of the part for different purposes.
                -- Typical values include score, parts, sound, and data. Ordering information that is directly
                -- encoded in MuseData can be derived from the ordering within a MusicXML score or opus.
                , scorePartGroup          :: Maybe String
                , scoreInstruments        :: [ScoreInstrument]
                , scoreMidiDevice         :: Maybe MidiDevice
                , scoreMidiInstruments    :: [MidiInstrument] }

instance Trivial ScorePart where
    trivial = ScorePart n n n n n n [] n []
        where n = Nothing

instance WriteXml ScorePart where
    writeXml = undefined


-- | The GroupName type describes the name or abbreviation of a 'PartGroup' element. Formatting
--   attributes in the GroupName type are deprecated in Version 2.0 in favor of the new
--   'GroupNameDisplay' and 'GroupAbbreviationDisplay' elements.
type GroupName = TODO
{-
    <xs:complexType name="group-name">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="group-name-text"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | The GroupSymbol type indicates how the symbol for a group is indicated in the score.
type GroupSymbol = TODO
{-
    <xs:complexType name="group-symbol">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="group-symbol-value">
                <xs:attributeGroup ref="position"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | The PartGroup element indicates groupings of parts in the score, usually indicated by braces and
--   brackets. Braces that are used for multi-staff parts should be defined in the attributes element
--   for that part. The 'PartGroup' start element appears before the first 'ScorePart' in the group. The
--   PartGroup stop element appears after the last score-part in the group. The number attribute is used
--   to distinguish overlapping and nested part-groups, not the sequence of groups. As with parts,
--   groups can have a name and abbreviation. Values for the child elements are ignored at the stop of a
--   group. A PartGroup element is not needed for a single multi-staff part. By default, multi-staff
--   parts include a brace symbol and (if appropriate given the bar-style) common barlines. The symbol
--   formatting for a multi-staff part can be more fully specified using the 'PartSymbol' element.

data PartGroup = PartGroup { groupName                 :: Maybe GroupName
                           , groupNameDisplay          :: Maybe NameDisplay
                           , groupAbbreviation         :: Maybe GroupName
                           , groupAbbreviationDisplay  :: Maybe NameDisplay
                           , groupSymbol               :: Maybe GroupSymbol
                           , groupBarline              :: Maybe (GroupBarlineValue, Maybe Color)
                           , groupTime                 :: Maybe Empty
                           , groupType                 :: StartStop
                           , groupNumber               :: String }

instance WriteXml PartGroup where
    writeXml = undefined

-- | The PartList identifies the different musical parts in this movement. Each part has an identifier
--   that is used later within the musical data. Since parts may be encoded separately and combined later,
--   'Identification' elements are present at both the score and 'ScorePart' levels. There must be at least
--   one ScorePart, combined as desired with 'PartGroup' elements that indicate braces and brackets. Parts
--   are ordered from top to bottom in a score based on the order in which they appear in the PartList.
type PartList = [Either PartGroup ScorePart]


-- ------------------------------------------------------------

-- | The ScoreHeader group contains basic score metadata about the work and movement,
--   score-wide defaults for layout and fonts, credits that appear on the first or
--   following pages, and the part list.
data ScoreHeader = ScoreHeader { work            :: Maybe Work
                               , movementNumber  :: Maybe String
                               , movementTitle   :: Maybe String
                               , identification  :: Maybe Identification
                               , defaults        :: Maybe Defaults
                               , credit          :: [Credit]
                               , partList        :: PartList }

instance Trivial ScoreHeader where
    trivial = ScoreHeader { work = trivial
                          , movementNumber  = trivial
                          , movementTitle   = trivial
                          , identification  = trivial
                          , defaults        = trivial
                          , credit          = trivial
                          , partList        = trivial }

instance WriteXml ScoreHeader where
    writeXml h  =  writeXml (work h)
               <+> writeXml (movementNumber h)
               <+> writeXml (movementTitle h)
               <+> writeXml (identification h)
               <+> writeXml (defaults h)
               <+> writeXml (credit h)
               <+> writeXml (partList h)


-- ------------------------------------------------------------

-- | The music-data group contains the basic musical data that is either associated with a part or
--   a measure, depending on whether the partwise or timewise hierarchy is used.

data MusicData = Note Note
               | Backup Backup
               | Forward Forward
               | Direction Direction
               | Attributes Attributes
               | Harmony Harmony
               | FiguredBass FiguredBass
               | Print Print
               | Sound Sound
               | Barline Barline
               | Grouping Grouping
               | Link Link
               | Bookmark Bookmark



