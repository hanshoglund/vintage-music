

module Music.Model.MusicXML.Score 
where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Articulations
import Music.Model.MusicXML.Harmony
import Music.Model.MusicXML.Sound
import Music.Model.MusicXML.Note
import Music.Model.MusicXML.Opus


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
    | Barline Barline
    | Grouping Grouping
    | Link Link
    | Bookmark Bookmark 


-- *****************************************************************************
-- Simple types
-- *****************************************************************************

-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

-- | The measure-attributes group is used by the measure element.
--   Measures have a required number attribute (going from partwise to timewise, measures are
--   grouped via the number).
--
--   The implicit attribute is set to True for measures where the measure number should never appear,
--   such as pickup measures and the last half of mid-measure repeats. The value is False if not
--   specified.
--
--   The non-controlling attribute is intended for use in multimetric music. 
--   If set to True, the left barline in this measure does not coincide with
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
    , implicit       :: Maybe Bool
    , nonControlling :: Maybe Bool
    , width          :: Maybe Tenths }
    deriving (Show, Eq)

-- | In either partwise or timewise format, the part element has an id attribute that is a reference 
-- back to a score-part in the part-list.
data PartAttributes = PartAttributes { id :: String }


-- | The document-attributes attribute group is used to specify the attributes for an entire MusicXML
-- document. Currently this is used for the version attribute.
--
-- The version attribute was added in Version 1.1 for the score-partwise and score-timewise
-- documents. It provides an easier way to get version information than through the MusicXML public
-- ID. The default value is 1.0 to make it possible for programs that handle later versions to
-- distinguish earlier version files reliably. Programs that write MusicXML 1.1 or 2.0 files should
-- set this attribute.
data DocumentAttributes = DocumentAttributes
    { version :: String }


-- *****************************************************************************
-- Complex types
-- *****************************************************************************

-- | The credit type represents the appearance of the title, composer, arranger, lyricist, copyright,
-- dedication, and other text and graphics that commonly appears on the first page of a score. The
-- credit-words and credit-image elements are similar to the words and image elements for directions.
-- However, since the credit is not part of a measure, the default-x and default-y attributes adjust
-- the origin relative to the bottom left-hand corner of the first page. The enclosure for
-- credit-words is none by default.
-- 
-- By default, a series of credit-words elements within a single credit element follow one another in
-- sequence visually. Non-positional formatting attributes are carried over from the previous element
-- by default.
-- 
-- The page attribute for the credit element, new in Version 2.0, specifies the page number where the
-- credit should appear. This is an integer value that starts with 1 for the first page. Its value is
-- 1 by default. Since credits occur before the music, these page numbers do not refer to the page
-- numbering specified by the print element's page-number attribute.

type Credit = TODO
{-
    <xs:complexType name="credit">
        <xs:annotation>
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

-- | The defaults type specifies score-wide defaults for scaling, layout, and appearance.
type Defaults = TODO
{-
    <xs:complexType name="defaults">
        <xs:annotation>
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

-- | Identification contains basic metadata about the score. It includes the information in MuseData
-- headers that may apply at a score-wide, movement-wide, or part-wide level. The creator, rights,
-- source, and relation elements are based on Dublin Core.
type Identification = TODO
{-
    <xs:complexType name="identification">
        <xs:annotation>
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

-- | Works are optionally identified by number and title. The work type also may indicate a link to
-- the opus document that composes multiple scores into a collection.
data Work = Work
    { 
      -- The work-number element specifies the number of a work, such as its opus
      -- number.
      workNumber :: Maybe String
      -- The work-title element specifies the title of a work, not including its opus or
      -- other work number.            
    , workTitle  :: Maybe String
    , opus       :: Maybe Opus 
    }
    


-- *****************************************************************************
-- Element groups
-- *****************************************************************************


-- | The midi-device type corresponds to the DeviceName meta event in Standard MIDI Files. The
-- optional port attribute is a number from 1 to 16 that can be used with the unofficial MIDI port
-- (or cable) meta event.
type MidiDevice = TODO
{-
    <xs:complexType name="midi-device">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="port" type="midi-16"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}  

-- | The midi-instrument type defines MIDI 1.0 instrument playback. The midi-instrument element can
-- be a part of either the score-instrument element at the start of a part, or the sound element
-- within a part. The id attribute refers to the score-instrument affected by the change.
type MidiInstrument = TODO
{-
    <xs:complexType name="midi-instrument">
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

-- | The part-name type describes the name or abbreviation of a score-part element. Formatting
-- attributes for the part-name element are deprecated in Version 2.0 in favor of the new
-- part-name-display and part-abbreviation-display elements.
type PartName = TODO
{-
    <xs:complexType name="part-name">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="part-name-text"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | The part-group element indicates groupings of parts in the score, usually indicated by braces
-- and brackets. Braces that are used for multi-staff parts should be defined in the attributes
-- element for that part. The part-group start element appears before the first score-part in the
-- group. The part-group stop element appears after the last score-part in the group. The number
-- attribute is used to distinguish overlapping and nested part-groups, not the sequence of groups.
-- As with parts, groups can have a name and abbreviation. Values for the child elements are ignored
-- at the stop of a group. A part-group element is not needed for a single multi-staff part. By
-- default, multi-staff parts include a brace symbol and (if appropriate given the bar-style) common
-- barlines. The symbol formatting for a multi-staff part can be more fully specified using the
-- part-symbol element.

type PartGroup = TODO
{-
    <xs:complexType name="part-group">
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

 

-- | The score-instrument type represents a single instrument within a score-part. As with the
-- score-part type, each score-instrument has a required ID attribute, a name, and an optional
-- abbreviation.
-- 
-- A score-instrument type is also required if the score specifies MIDI 1.0 channels, banks, or
-- programs. An initial midi-instrument assignment can also be made here. MusicXML software should be
-- able to automatically assign reasonable channels and instruments without these elements in simple
-- cases, such as where part names match General MIDI instrument names.

type ScoreInstrument = TODO
{-
    <xs:complexType name="score-instrument">
        <xs:annotation>
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

-- | Each MusicXML part corresponds to a track in a Standard MIDI Format 1 file. The score-instrument
-- elements are used when there are multiple instruments per track. The midi-device element is used
-- to make a MIDI device or port assignment for the given track. Initial midi-instrument assignments
-- may be made here as well.
data ScorePart = ScorePart
    { partIdentification      :: Maybe Identification
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
    , scoreMidiInstruments    :: [MidiInstrument]
    }
{-
    <xs:complexType name="score-part">
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


-- | The part-list identifies the different musical parts in this movement. Each part has an identifier that
-- is used later within the musical data. Since parts may be encoded separately and combined later,
-- identification elements are present at both the score and score-part levels. There must be at
-- least one score-part, combined as desired with part-group elements that indicate braces and
-- brackets. Parts are ordered from top to bottom in a score based on the order in which they appear
-- in the part-list.
-- FIXME
data PartList = PartList
    { partGroup :: Maybe PartGroup
    , scorePart :: ScorePart
    , children :: Maybe (Either PartGroup ScorePart) }
{-
    <xs:complexType name="part-list">
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
-}

-- *****************************************************************************
-- Root elements
-- *****************************************************************************

type Part    = [(MeasureAttributes, [MusicData])]
type Measure = (MeasureAttributes, [[MusicData]])
                                                            
-- | The score is the root element for the schema. It includes the score-header
-- group, followed either by a series of parts with measures inside (PartwiseScore) or a series of
-- measures with parts inside (TimewiseScore). Having distinct top-level elements for partwise and
-- timewise scores makes it easy to ensure that an XSLT stylesheet does not try to transform a
-- document already in the desired format.
-- The document-attributes attribute group includes the version attribute.
data Score = 
  PartwiseScore 
    { attributes :: Maybe DocumentAttributes
    , header     :: ScoreHeader
    , parts      :: [Part] }
  | TimewiseScore 
    { attributes :: Maybe DocumentAttributes
    , header     :: ScoreHeader
    , measures   :: [Measure] }


