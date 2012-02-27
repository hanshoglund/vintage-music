

module Music.Model.MusicXML.Attributes 
where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout


type PartSymbol = TODO
type Instruments = TODO
type Clef = TODO
type StaffDetails = TODO

-- | The key type represents a key signature. Both traditional and non-traditional key signatures are
-- supported. The optional number attribute refers to staff numbers. If absent, the key signature
-- applies to all staves in the part.
type Key = TODO
{-
    <xs:complexType name="key">
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
-- | The key-octave element specifies in which octave an element of a key signature appears. The
-- content specifies the octave value using the same values as the display-octave element. The number
-- attribute is a positive integer that refers to the key signature element in left-to-right order.
-- If the cancel attribute is set to yes, then this number refers to an element specified by the
-- cancel element. It is no by default.
type KeyOctave = TODO
{-
    <xs:complexType name="key-octave">
        <xs:simpleContent>
            <xs:extension base="octave">
                <xs:attribute name="number" type="xs:positiveInteger" use="required"/>
                <xs:attribute name="cancel" type="yes-no"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | Time signatures are represented by the beats element for the numerator and the beat-type element
-- for the denominator. The symbol attribute is used indicate common and cut time symbols as well as
-- a single number display. Multiple pairs of beat and beat-type elements are used for composite time
-- signatures with multiple denominators, such as 2/4 + 3/8. A composite such as 3+2/8 requires only
-- one beat/beat-type pair. The print-object attribute allows a time signature to be specified but
-- not printed, as is the case for excerpts from the middle of a score. The value is "yes" if not
-- present. The optional number attribute refers to staff numbers within the part. If absent, the
-- time signature applies to all staves in the part.
type Time = TODO
{-
    <xs:complexType name="time">
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
-- | The transpose type represents what must be added to a written pitch to get a correct sounding
-- pitch.
type Transpose = TODO
{-
    <xs:complexType name="transpose">
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

-- | The attributes element contains musical information that typically changes on measure
-- boundaries. This includes key and time signatures, clefs, transpositions, and staving.
data Attributes = Attributes
    { 
    -- | Musical notation duration is commonly represented as fractions. The divisions element indicates how
    -- many divisions per quarter note are used to indicate a note's duration. For example, if duration = 1
    -- and divisions = 2, this is an eighth note duration. Duration and divisions are used directly for
    -- generating sound output, so they must be chosen to take tuplets into account. Using a divisions
    -- element lets us use just one number to represent a duration for each note in the score, while
    -- retaining the full power of a fractional representation. If maximum compatibility with Standard MIDI
    -- 1.0 files is important, do not have the divisions value exceed 16383.
          attributesDivisions     :: Maybe Divisions

    -- | The key element represents a key signature. Both traditional and non-traditional key signatures are
    -- supported. The optional number attribute refers to staff numbers. If absent, the key signature applies
    -- to all staves in the part.
        , attributesKey           :: [Key]

    -- | Time signatures are represented by the beats element for the numerator and the beat-type element for
    -- the denominator.
        , attributesTime          :: [Time]

    -- | The staves element is used if there is more than one staff represented in the given part (e.g., 2
    -- staves for typical piano parts). If absent, a value of 1 is assumed. Staves are ordered from top to
    -- bottom in a part in numerical order, with staff 1 above staff 2.
        , attributesStaves        :: Maybe Int

    -- | The part-symbol element indicates how a symbol for a multi-staff part is indicated in the score.
        , attributesPartSymbol    :: Maybe PartSymbol

    -- | The instruments element is only used if more than one instrument is represented in the part (e.g.,
    -- oboe I and II where they play together most of the time). If absent, a value of 1 is assumed.
        , attributesInstruments   :: Maybe Instruments

    -- | Clefs are represented by a combination of sign, line, and clef-octave-change elements.
        , attributesClef          :: [Clef]

    -- | The staff-details element is used to indicate different types of staves.
        , attributesStaffDetails  :: [StaffDetails]

    -- | If the part is being encoded for a transposing instrument in written vs. concert pitch, the
    -- transposition must be encoded in the transpose element using the transpose type.
        , attributesTranspose     :: [Transpose]

    -- | Directives are like directions, but can be grouped together with attributes for convenience. This is
    -- typically used for tempo markings at the beginning of a piece of music. This element has been
    -- deprecated in Version 2.0 in favor of the directive attribute for direction elements. Language names
    -- come from ISO 639, with optional country subcodes from ISO 3166.
        , attributesDirective     :: [Directive]

    -- | A measure-style indicates a special way to print partial to multiple measures within a part. This
    -- includes multiple rests over several measures, repeats of beats, single, or multiple measures, and use
    -- of slash notation.
        , attributesMeasureStyle  :: [MeasureStyle] 
        
    }