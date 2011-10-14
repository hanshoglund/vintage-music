

module Music.Model.MusicXML.Harmony 
where

import Music.Model.MusicXML.Base


-- *****************************************************************************
-- Simple types
-- *****************************************************************************

-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

-- *****************************************************************************
-- Complex types
-- *****************************************************************************

-- | The harmony type is based on Humdrum's harm encoding, extended to support chord symbols in
-- popular music as well as functional harmony analysis in classical music.
--
-- If there are alternate harmonies possible, this can be specified using multiple harmony elements
-- differentiated by type. Explicit harmonies have all note present in the music; implied have some
-- notes missing but implied; alternate represents alternate analyses.
--
-- The harmony object may be used for analysis or for chord symbols. The print-object attribute
-- controls whether or not anything is printed due to the harmony element. The print-frame attribute
-- controls printing of a frame or fretboard diagram. The print-style attribute group sets the
-- default for the harmony, but individual elements can override this with their own print-style
-- values.
type Harmony = TODO
{-
    <xs:complexType name="harmony">
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
   
-- | The figure type represents a single figure within a figured-bass element.
type Figure = TODO
{-
    <xs:complexType name="figure">
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

-- | The figured-bass element represents figured bass notation. Figured bass elements take their
-- position from the first regular note that follows. Figures are ordered from top to bottom. The
-- value of parentheses is False if not present.
type FiguredBass = TODO
{-
    <xs:complexType name="figured-bass">
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

-- | The bass type is used to indicate a bass note in popular music chord symbols, e.g. G/C. It is
-- generally not used in functional harmony, as inversion is generally not used in pop chord symbols.
-- As with root, it is divided into step and alter elements, similar to pitches.
type Bass = TODO
{-
    <xs:complexType name="bass">
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

-- | The bass-step type represents the pitch step of the bass of the current chord within the harmony
-- element. The text attribute indicates how the bass should appear on the page if not using the
-- element contents.
type BassStep = TODO
{-
    <xs:complexType name="bass-step">
        <xs:simpleContent>
            <xs:extension base="step">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | The degree type is used to add, alter, or subtract individual notes in the chord. The
-- print-object attribute can be used to keep the degree from printing separately when it has already
-- taken into account in the text attribute of the kind element. The degree-value and degree-type
-- text attributes specify how the value and type of the degree should be displayed. A harmony of
-- kind "other" can be spelled explicitly by using a series of degree elements together with a root.
type Degree = TODO
{-
    <xs:complexType name="degree">
        <xs:sequence>
            <xs:element name="degree-value" type="degree-value"/>
            <xs:element name="degree-alter" type="degree-alter"/>
            <xs:element name="degree-type" type="degree-type"/>
        </xs:sequence>
        <xs:attributeGroup ref="print-object"/>
    </xs:complexType>

-}
-- | The degree-alter type represents the chromatic alteration for the current degree. If the
-- degree-type value is alter or subtract, the degree-alter value is relative to the degree already
-- in the chord based on its kind element. If the degree-type value is add, the degree-alter is
-- relative to a dominant chord (major and perfect intervals except for a minor seventh). The
-- plus-minus attribute is used to indicate if plus and minus symbols should be used instead of sharp
-- and flat symbols to display the degree alteration; it is no by default.
type DegreeAlter = TODO
{-
    <xs:complexType name="degree-alter">
        <xs:simpleContent>
            <xs:extension base="semitones">
                <xs:attributeGroup ref="print-style"/>
                <xs:attribute name="plus-minus" type="yes-no"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
-- | The degree-type type indicates if this degree is an addition, alteration, or subtraction
-- relative to the kind of the current chord. The value of the degree-type element affects the
-- interpretation of the value of the degree-alter element. The text attribute specifies how the type
-- of the degree should be displayed.
type DegreeType = TODO
{-
    <xs:complexType name="degree-type">
        <xs:simpleContent>
            <xs:extension base="degree-type-value">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
-- | The content of the degree-value type is a number indicating the degree of the chord (1 for the
-- root, 3 for third, etc). The text attribute specifies how the type of the degree should be
-- displayed.
type DegreeValue = TODO
{-
    <xs:complexType name="degree-value">
        <xs:simpleContent>
            <xs:extension base="xs:positiveInteger">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
                
-- | Kind indicates the type of chord. Degree elements can then add, subtract, or alter from these
-- starting points
--
-- The attributes are used to indicate the formatting of the symbol. Since the kind element is the
-- constant in all the harmony-chord groups that can make up a polychord, many formatting attributes
-- are here.
--
-- The use-symbols attribute is yes if the kind should be represented when possible with harmony
-- symbols rather than letters and numbers. These symbols include:
--
--     major: a triangle, like Unicode 25B3
--     minor: -, like Unicode 002D
--     augmented: +, like Unicode 002B
--     diminished: °, like Unicode 00B0
--     half-diminished: ø, like Unicode 00F8
--
-- The text attribute describes how the kind should be spelled if not using symbols; it is ignored if
-- use-symbols is yes. The stack-degrees attribute is yes if the degree elements should be stacked
-- above each other. The parentheses-degrees attribute is yes if all the degrees should be in
-- parentheses. The bracket-degrees attribute is yes if all the degrees should be in a bracket. If
-- not specified, these values are implementation-specific. The alignment attributes are for the
-- entire harmony-chord group of which this kind element is a part.
type Kind = TODO
{-
    <xs:complexType name="kind">
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

-- | The root type indicates a pitch like C, D, E vs. a function indication like I, II, III. It is
-- used with chord symbols in popular music. The root element has a root-step and optional root-alter
-- element similar to the step and alter elements, but renamed to distinguish the different musical
-- meanings.
type Root = TODO
{-
    <xs:complexType name="root">
        <xs:annotation>
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

-- | The root-step type represents the pitch step of the root of the current chord within the harmony
-- element. The text attribute indicates how the root should appear on the page if not using the
-- element contents.
type RootStep = TODO
{-
    <xs:complexType name="root-step">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="step">
                <xs:attribute name="text" type="xs:token"/>
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- | The non-traditional-key group represents a single alteration within a non-traditional key
-- signature. A sequence of these groups makes up a non-traditional key signature
type NonTraditionalKey = TODO
{-
    <xs:group name="non-traditional-key">
        <xs:annotation>
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

-- | The traditional-key group represents a traditional key signature using the cycle of fifths.
type TraditionalKey = TODO
{-
    <xs:group name="traditional-key">
        <xs:annotation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="cancel" type="cancel" minOccurs="0"/>
            <xs:element name="fifths" type="fifths"/>
            <xs:element name="mode" type="mode" minOccurs="0"/>
        </xs:sequence>
    </xs:group>

    <!-- Element groups derived from direction.mod entities and elements -->

-}

-- | A harmony element can contain many stacked chords (e.g. V of II). A sequence of harmony-chord
-- groups is used for this type of secondary function, where V of II would be represented by a
-- harmony-chord with a V function followed by a harmony-chord with a II function.
--
-- A root is a pitch name like C, D, E, where a function is an indication like I, II, III. It is an
-- either/or choice to avoid data inconsistency.
type HarmonyChord = TODO
{-
    <xs:group name="harmony-chord">
        <xs:annotation>
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
-- *****************************************************************************
-- Root elements
-- *****************************************************************************
